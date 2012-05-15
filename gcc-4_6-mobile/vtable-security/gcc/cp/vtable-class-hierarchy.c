/* Copyright (C) 2012  Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file is part of the vtable security implementation.  It collects
   class hierarchy information about the program being compiled and
   inserts calls to __VLTRegisterPair, registering this information.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "timevar.h"
#include "cpplib.h"
#include "tree.h"
#include "cp-tree.h"
#include "intl.h"
#include "c-family/c-pragma.h"
#include "decl.h"
#include "flags.h"
#include "diagnostic-core.h"
#include "output.h"
#include "target.h"
#include "cgraph.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "plugin.h"
#include "tree-threadsafe-analyze.h"
#include "tree-iterator.h"

static GTY(()) tree vlt_register_pairs_fndecl = NULL_TREE;
static GTY(()) tree vlt_change_permission_fndecl = NULL_TREE;

struct list_node {
  tree class_type;
  struct list_node *next;
};

struct node {
  tree ptr_decl_or_template_type_id;
  struct list_node *class_list;
  struct node *left;
  struct node *right;
};

struct node2 {
  tree base_map_var_decl;
  tree vtable_decl;
  unsigned offset;
  struct node2 *left;
  struct node2 *right;
};

static void init_functions (void);
static void linked_list_insert (struct list_node **, tree);
static void binary_tree_insert (struct node **, tree, tree, tree);
static struct node *binary_tree_find (struct node *, tree);
static struct node *binary_tree_find_template (struct node *, tree);
static void dump_class_hierarchy_information (struct node *root);
static void register_all_pairs (struct node *root, tree body);
/* static void compute_hierarchy_transitive_closure (void); */
static void template_info_tree_insert (struct node **, tree, tree);
static struct list_node *template_list_search (tree class_type);
static struct list_node *template_tree_find (struct node *, tree type_id);

static struct node *vlt_class_hierarchy_info = NULL;
static struct node2 *registered_pairs = NULL;
static struct node *vlt_template_vptr_info = NULL;

static void
init_functions (void)
{
  tree void_ptr_type = build_pointer_type (void_type_node);
  tree arg_types = NULL_TREE;
  tree register_pairs_type = void_ptr_type;
  tree change_permission_type = void_ptr_type;
  tree char_ptr_type = build_pointer_type (char_type_node);

  arg_types = build_tree_list (NULL_TREE, char_ptr_type);
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, integer_type_node));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_type_node));

  change_permission_type = build_function_type (change_permission_type,
                                                arg_types);
  vlt_change_permission_fndecl = build_fn_decl ("__VLTChangePermission",
                                                change_permission_type);
  TREE_NOTHROW (vlt_change_permission_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_change_permission_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES (vlt_change_permission_fndecl));
  TREE_PUBLIC (vlt_change_permission_fndecl) = 1;
  DECL_PRESERVE_P (vlt_change_permission_fndecl) = 1;
  retrofit_lang_decl (vlt_change_permission_fndecl);
  SET_DECL_LANGUAGE (vlt_change_permission_fndecl, lang_cplusplus);

  arg_types = build_tree_list (NULL_TREE, build_pointer_type (void_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_ptr_type));
  /* Start: Arg types to be removed when we remove debugging parameters from
     the library function. */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, char_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, integer_type_node));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, char_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, integer_type_node));
  /* End: Arg types to be removed...*/
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_type_node));

  register_pairs_type = build_function_type (register_pairs_type, arg_types);
  vlt_register_pairs_fndecl = build_fn_decl ("__VLTRegisterPair",
                                             register_pairs_type);
  TREE_NOTHROW (vlt_register_pairs_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_register_pairs_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES (vlt_register_pairs_fndecl));
  TREE_PUBLIC (vlt_register_pairs_fndecl) = 1;
  DECL_PRESERVE_P (vlt_register_pairs_fndecl) = 1;
  retrofit_lang_decl (vlt_register_pairs_fndecl);
  SET_DECL_LANGUAGE (vlt_register_pairs_fndecl, lang_cplusplus);
}

static void
dump_class_hierarchy_information (struct node *root)
{
  struct list_node *current;

  /* Dump current node */
  if (!root || ! root->ptr_decl_or_template_type_id)
    return;

  fprintf (stdout, "Base class '%s' is inherited by: ",
           IDENTIFIER_POINTER (DECL_NAME (root->ptr_decl_or_template_type_id)));

  current = root->class_list;
  while (current)
    {
      if (current->class_type)
	fprintf (stdout, " (%s, %s)",
		 IDENTIFIER_POINTER (DECL_NAME
				     (TYPE_NAME (current->class_type))),
                 IDENTIFIER_POINTER (get_mangled_id(TREE_CHAIN(current->class_type))));
      current = current->next;
    }
  fprintf (stdout, "\n");

  /* Dump left child */
  dump_class_hierarchy_information (root->left);

  /* Dump right child */
  dump_class_hierarchy_information (root->right);
}

static void
list_append (struct list_node *old_list, struct list_node *new_list)
{
  struct list_node *old_cur;
  struct list_node *old_end;
  struct list_node *new_cur;
  struct list_node *new_node;

  /* Append to the old list anything in the new list that isn't
     already there.*/

  /* Find the end of the old list */
  old_end = old_list;
  while (old_end->next != NULL)
    old_end = old_end->next;

  /* Look at each elemement in new list */
  for (new_cur = new_list; new_cur; new_cur = new_cur->next)
    {
      /* Look for 'new_cur' in the old list.  */
      bool found = false;

      for (old_cur = old_list; old_cur && !found; old_cur = old_cur->next)
        if (get_mangled_id (TREE_CHAIN (new_cur->class_type))
            == get_mangled_id (TREE_CHAIN (old_cur->class_type)))
          found = true;

      /* if not found, copy new node and append to end of old list */
      if (!found)
        {
          new_node = (struct list_node *) xmalloc (sizeof (struct list_node));
          new_node->class_type = new_cur->class_type;
          new_node->next = NULL;

          old_end->next = new_node;
          old_end = old_end->next;
        }
    }

}

static struct node *
binary_tree_find_template (struct node *root, tree class_type)
{
  struct node *child_found = NULL;
  char *cur_node_name, *dup_node_name;
  char *cptr;
  char *mangled_class_name = NULL,  *orig_mangled_name = NULL;

  if (!root)
    return NULL;

  dup_node_name = cur_node_name = xstrdup (IDENTIFIER_POINTER (DECL_NAME
                                                               (root->ptr_decl_or_template_type_id)));
  cptr = strstr (cur_node_name, ".vtable_map");
  if (cptr)
    cptr[0] = '\0';

  if (strncmp (cur_node_name, "_ZTV", 4) == 0)
    cur_node_name += 4;

  while ((cur_node_name[0] >= '0') && (cur_node_name[0] <= '9'))
    cur_node_name++;

  if (TREE_CHAIN (class_type)
      && (TREE_CODE (TREE_CHAIN (class_type)) == TYPE_DECL))
    orig_mangled_name = mangled_class_name = IDENTIFIER_POINTER (get_mangled_id (TREE_CHAIN (class_type)));

  while ((mangled_class_name[0] >= '0') && (mangled_class_name[0] <= '9'))
    mangled_class_name++;

  /* DEBUG
  fprintf(stdout, "find template comparing class_type=%s with cur_node_name=%s\n", 
          orig_mangled_name, cur_node_name);
  */

  if (strcmp (cur_node_name, mangled_class_name) == 0)
  {
    free (dup_node_name);
    return root;
  }
  else
    {
      /*
	Luis: This is a complete and total hack.  For some reason,
	when the .vtable_map variable first gets created for the
	uninstantiated template class (in parser.c), the template
	class record type isn't a complete type definition.  SO for
	some bizarre reason, when I get the manged id THEN it is
	slightly different than the mangled id that I get at this
	point.  In particular, the original mangled name contains
	"TX0_" and the new mangled name contains "TX_".  In all other
	respects the two mangled names are identical.  I'm tired and
	it's late so I don't have time to chase this down and figure
	out why this is happening.  I'm inserting this hack here to
	see if, with it, we can get a proper match for the templates,
	so that we can build a transitive closure that has an
	uninstantiated template type in the middle.
       */
      char *cptr1 = strstr (cur_node_name, "XT0_");
      char *cptr2 = strstr (mangled_class_name, "XT_");
      if (cptr1 && cptr2)
	{
	  cptr1 += 3;
	  cptr2 += 2;
	  if (strcmp (cptr1, cptr2) == 0)
          {
            free (dup_node_name);
	    return root;
          }
	}
    }

  free (dup_node_name);

  child_found = binary_tree_find_template (root->left, class_type);

  if (!child_found)
    child_found = binary_tree_find_template (root->right, class_type);

  return child_found;
}

static struct node *
binary_tree_find (struct node *root, tree var_decl)
{
  if (!root)
    return NULL;

  if (DECL_NAME (root->ptr_decl_or_template_type_id) == DECL_NAME (var_decl))
    return root;

  else if (var_decl < root->ptr_decl_or_template_type_id)
    return binary_tree_find (root->left, var_decl);
  else
    return binary_tree_find (root->right, var_decl);
}

static bool
build_transitive_closure (struct node *root, struct node *cur_node)
{
  bool current_changed = false;
  bool left_changed = false;
  bool right_changed = false;
  struct list_node *cur_list;
  char * cur_node_name;
  char *cptr;

  if (!cur_node)
    return false;

  /* DEBUG
  fprintf(stderr, "build_transitive_closure: cur_node: ");
  debug_c_tree(cur_node->ptr_decl_or_template_type_id);
  */

  cur_node_name = xstrdup (IDENTIFIER_POINTER
			         (DECL_NAME
                                    (cur_node->ptr_decl_or_template_type_id)));
  cptr = strstr (cur_node_name, ".vtable_map");
  if (cptr)
    cptr[0] = '\0';
  /* Handle current tree node */
  for (cur_list = cur_node->class_list; cur_list; cur_list = cur_list->next)
    {
      const char *cur_list_name = NULL;
      char *var_id_name = NULL;
      tree var_id = NULL_TREE;
      tree var_decl = NULL_TREE;
      struct node *tmp_node = NULL;
      tree vtbl_var_decl;

      /* DEBUG
      fprintf(stderr, "build_transitive_closure: cur_list:");
      debug_c_tree(cur_list->class_type);
      */

      struct list_node *template_list = template_list_search (cur_list->class_type);
      bool is_template_type = (template_list != NULL);

      /* TO DO: Extract the following into a separate function, to add
	 safety checks! */

      if (is_template_type)
	{
	  tree type_decl = TREE_CHAIN (cur_list->class_type);
	  tree type_id = get_mangled_id (type_decl);
	  cur_list_name = ACONCAT (("_ZTV", IDENTIFIER_POINTER (type_id), NULL));
	  if (strcmp (cur_list_name, cur_node_name) == 0)
	      continue;

	  tmp_node = binary_tree_find_template (root, cur_list->class_type);
	}
      else
	{
	  if ((! cur_list->class_type)
	      || (! TYPE_BINFO (cur_list->class_type))
	      || (! BINFO_VTABLE (TYPE_BINFO (cur_list->class_type))))
	    continue;

	  vtbl_var_decl = TREE_OPERAND
	    (TREE_OPERAND
	     (BINFO_VTABLE
	      (TYPE_BINFO
	       (cur_list->class_type)), 0), 0);

	  if (!vtbl_var_decl)
	    continue;
	  cur_list_name = (const char *) IDENTIFIER_POINTER
	    (DECL_NAME (vtbl_var_decl));

	  if (!cur_list_name)
	    continue;

	  if (strcmp (cur_list_name, cur_node_name) == 0)
	    continue;

	  var_id_name = (char *) xmalloc (strlen (cur_list_name) + 12);
	  sprintf (var_id_name, "%s.vtable_map", cur_list_name);
	  var_id = get_identifier (var_id_name);
	  free (var_id_name);

	  var_decl = vtable_find_map_decl (var_id);
	  if (!var_decl)
	    continue;

	  tmp_node = binary_tree_find (root, var_decl);
	}

      if (!tmp_node)
        continue;
      else
        {
          list_append (cur_node->class_list, tmp_node->class_list);
          current_changed = true;
        }
    }

  free(cur_node_name);

  /* Handle left child */
  left_changed = build_transitive_closure (root, cur_node->left);

  /* Handle right child */
  right_changed = build_transitive_closure (root, cur_node->right);

  return (current_changed || left_changed || right_changed);
}

/* static void */
void
/* compute_hierarchy_transitive_closure (void) */
compute_class_hierarchy_transitive_closure (void)
{
  build_transitive_closure (vlt_class_hierarchy_info,
			    vlt_class_hierarchy_info);
}

static bool
tree_three_key_insert (struct node2 **root, tree key1, tree key2, unsigned key3)
{
  /* In "struct node2", base_map_var_decl is the primary sort key (the base
     class .vtable_map variable decl), and vtable_decl is the secondary sort
     key (the var decl the vtable). The third key is the offset added to the
     vtable to get the actual recorded vtable pointer address.  */

  struct node2 *new_node;

  if (!(*root))
    {
      new_node = (struct node2 *) xmalloc (sizeof (struct node2));
      new_node->base_map_var_decl = key1;
      new_node->vtable_decl = key2;
      new_node->offset = key3;
      new_node->left = NULL;
      new_node->right = NULL;
      (*root) = new_node;
      return false;
    }
  else if ((*root)->base_map_var_decl == key1)
    {
      if ((*root)->vtable_decl == key2)
        {
          if ((*root)->offset == key3)
            return true;
          else if (key3 < (*root)->offset)
            return tree_three_key_insert (&((*root)->left), key1, key2, key3);
          else if (key3 > (*root)->offset)
            return tree_three_key_insert (&((*root)->right), key1, key2, key3);
        }
      else if (key2 < (*root)->vtable_decl)
        return tree_three_key_insert (&((*root)->left), key1, key2, key3);
      else if (key2 > (*root)->vtable_decl)
        return tree_three_key_insert (&((*root)->right), key1, key2, key3);
    }
  else if (key1 < (*root)->base_map_var_decl)
    return tree_three_key_insert (&((*root)->left), key1, key2, key3);
  else if (key1 > (*root)->base_map_var_decl)
    return tree_three_key_insert (&((*root)->right), key1, key2, key3);

  return false;
}

static bool
record_register_pairs (tree base_ptr_decl, tree vtable_decl, tree vptr_address)
{
  unsigned offset = TREE_INT_CST_LOW (TREE_OPERAND (vptr_address, 1));
  return tree_three_key_insert (&registered_pairs, base_ptr_decl, vtable_decl, offset);
}

static void
register_vptr_fields (tree base_class_decl_arg, tree record_type, tree body)
{
  /* A class may contain secondary vtables in it, for various
     reasons.  This function goes through the decl chain of a class
     record looking for any fields that point to secondary vtables,
     and adding calls to __VLTRegisterPair for the secondary vtable
     pointers.  */

  tree vtbl_var_decl;
  tree arg1;
  tree arg2;

  if (TREE_CODE (record_type) != RECORD_TYPE)
    return;

  vtbl_var_decl = get_vtbl_decl_for_binfo (TYPE_BINFO (record_type));

  if (vtbl_var_decl)
    {
      tree ztt_decl = DECL_CHAIN (vtbl_var_decl);
      bool already_registered = false;

      /* construction vtable */
      if (true &&
          ztt_decl != NULL_TREE
          && (DECL_NAME (ztt_decl))
          && (strncmp (IDENTIFIER_POINTER (DECL_NAME (ztt_decl)),
                       "_ZTT", 4) == 0))
        {
          tree values = DECL_INITIAL (ztt_decl);
          struct varpool_node * vp_node = varpool_node (ztt_decl);
          if ( vp_node->needed && vp_node->finalized 
	       && (values != NULL_TREE)
              && (TREE_CODE (values) == CONSTRUCTOR)
              && (TREE_CODE (TREE_TYPE (values)) == ARRAY_TYPE))
            {
              tree call_expr = NULL_TREE;
              unsigned HOST_WIDE_INT cnt;
              constructor_elt *ce;

              for (cnt = 0;
                   VEC_iterate (constructor_elt, CONSTRUCTOR_ELTS (values),
				cnt, ce);
                   cnt++)
                {
                  tree value = ce->value;
                  tree val_vtbl_decl = TREE_OPERAND (TREE_OPERAND (value, 0), 0);
                  int len1 = strlen (IDENTIFIER_POINTER
				     (DECL_NAME
				          (TREE_OPERAND
					     (base_class_decl_arg, 0))));
                  int len2 = strlen (IDENTIFIER_POINTER
				     (DECL_NAME (val_vtbl_decl)));
                  arg1 = build_string_literal (len1,
                                               IDENTIFIER_POINTER
					        (DECL_NAME
						 (TREE_OPERAND
						  (base_class_decl_arg, 0))));
                  arg2 = build_string_literal (len2,
                                               IDENTIFIER_POINTER
					        (DECL_NAME (val_vtbl_decl)));

                  already_registered = record_register_pairs (TREE_OPERAND
                                                               (base_class_decl_arg, 0),
                                                              val_vtbl_decl,
                                                              value);

                  if (already_registered)
                    continue;

		  /* This call expr has the 2 "real" arguments, plus 4
		     debugging arguments.  Eventually it will be
		     replaced with the one just below it, which only
		     has the 2 real arguments.  */
                  call_expr = build_call_expr
		                        (vlt_register_pairs_fndecl, 6,
					 base_class_decl_arg, value,
					 arg1,
					 build_int_cst (integer_type_node,
							len1),
					 arg2,
					 build_int_cst (integer_type_node,
							len2));
		  /* See comments above.
                  call_expr = build_call_expr (vlt_register_pairs_fndecl, 2,
                                               base_class_decl_arg, value);
		  */
		  append_to_statement_list (call_expr, &body);
                }
            }
        }
    }
}

static struct list_node *
template_tree_find (struct node *root, tree type_id)
{
  if (root == NULL)
    return NULL;
  else if (root->ptr_decl_or_template_type_id == type_id)
    return root->class_list;
  else if (type_id < root->ptr_decl_or_template_type_id)
    return template_tree_find (root->left, type_id);
  else
    return template_tree_find (root->right, type_id);
}

static struct list_node *
template_list_search (tree class_type)
{
  tree type_id = NULL_TREE;

  if (TYPE_NAME (class_type))
    {
      if (TREE_CODE (TYPE_NAME (class_type)) == IDENTIFIER_NODE)
	type_id = TYPE_NAME (class_type);
      else if (TREE_CODE (TYPE_NAME (class_type)) == TYPE_DECL)
	/* && DECL_NAME (TYPE_NAME (class_type))) */
	/* type_id = DECL_NAME (TYPE_NAME (class_type)); */
	type_id = get_mangled_id (TYPE_NAME (class_type));
    }

  if (type_id != NULL_TREE)
      return template_tree_find (vlt_template_vptr_info, type_id);

  return NULL;
}

static void
register_other_binfo_vtables (tree binfo, tree body, tree arg1, tree str1,
                              int len1, tree str2, int len2)
{
  unsigned ix;
  tree base_binfo;
  tree vtable_decl;
  bool already_registered;

  if (binfo == NULL_TREE)
    return;

  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      if ((!BINFO_PRIMARY_P (base_binfo)
           || BINFO_VIRTUAL_P (base_binfo))
          && (vtable_decl=get_vtbl_decl_for_binfo (base_binfo))
          && !(DECL_VTABLE_OR_VTT_P(vtable_decl) && DECL_CONSTRUCTION_VTABLE_P(vtable_decl)))
        {
          tree vtable_address = build_vtbl_address (base_binfo);
          tree call_expr;

          already_registered = record_register_pairs (TREE_OPERAND (arg1, 0),
                                                      vtable_decl,
                                                      vtable_address);
          if (!already_registered)
            {
              call_expr = build_call_expr (vlt_register_pairs_fndecl, 6,
                                            arg1, vtable_address,
                                            str1,
                                            build_int_cst (integer_type_node,
                                                           len1),
                                            str2,
                                            build_int_cst (integer_type_node,
                                                           len2));
              append_to_statement_list (call_expr, &body);
            }
        }

      register_other_binfo_vtables (base_binfo, body, arg1, str1, len1, str2,
                                    len2);
    }
}

static void
register_all_pairs (struct node *root, tree body)
{
  struct list_node *current;
  tree base_ptr_var_decl;

  /* Handle current node */
  if (!root || ! root->ptr_decl_or_template_type_id)
    return;

  base_ptr_var_decl = root->ptr_decl_or_template_type_id;

  current = root->class_list;
  while (current)
    {
      if (current->class_type
          && (TREE_CODE (current->class_type) == RECORD_TYPE))
        {
          tree new_type;
          tree arg1;
          tree call_expr;
          bool already_registered;

          tree binfo = TYPE_BINFO (current->class_type);
	  tree vtable = NULL_TREE;
          tree vtable_decl;
          bool vtable_should_be_output = false;

          struct list_node *template_vtable_list = NULL;

	  if (binfo)
	    vtable = BINFO_VTABLE (binfo);
	  
	  if (!vtable && binfo)
	    {
	      /* Possibly look for vtable in instantiated template types */
	      template_vtable_list = template_list_search
                                                        (current->class_type);

	      if (template_vtable_list)
 		{
                  /* If we found a list of corresponding instantiated
                     template types, insert them all into the current
                     types list (just after our current position) so
                     we will go through them all on the next
                     iterations of this while loop.  */
                  struct list_node *cur_list;
                  for (cur_list = template_vtable_list; cur_list;
                       cur_list = cur_list->next)
                    {
                      struct list_node *new_type_node =
                          (struct list_node *) xmalloc (sizeof
                                                        (struct list_node));
                      new_type_node->class_type = cur_list->class_type;
                      new_type_node->next = current->next;
                      current->next = new_type_node;
                    }
		}
	    }

          vtable_decl = CLASSTYPE_VTABLES (current->class_type);

          /* Handle main vtable for this class. */

          if (vtable_decl)
            {
              struct varpool_node *node = varpool_node (vtable_decl);
              vtable_should_be_output = node->needed;
            }

          if (vtable_decl && vtable_should_be_output && BINFO_VTABLE (binfo))
            {
              tree vtable_address = build_vtbl_address (binfo);
              int len1  = IDENTIFIER_LENGTH (DECL_NAME (base_ptr_var_decl));
              int len2  = IDENTIFIER_LENGTH (DECL_NAME (vtable_decl));
              tree str1 = build_string_literal (len1,
                                                IDENTIFIER_POINTER
                                                  (DECL_NAME
                                                     (base_ptr_var_decl)));
              tree str2 = build_string_literal (len2,
                                                IDENTIFIER_POINTER
                                                  (DECL_NAME (vtable_decl)));

              already_registered = record_register_pairs (base_ptr_var_decl,
                                                          vtable_decl,
                                                          vtable_address);

              if (!already_registered)
                {
                  new_type = build_pointer_type (TREE_TYPE
                                                   (base_ptr_var_decl));
                  arg1 = build1 (ADDR_EXPR, new_type, base_ptr_var_decl);

                  /* This call expr has the 2 "real" arguments, plus 4
                     debugging arguments.  Eventually it will be replaced
                     with the one just below it, which only has the 2 real
                     arguments.  */
                  call_expr = build_call_expr
                      (vlt_register_pairs_fndecl, 6,
                       arg1, vtable_address,
                       str1, build_int_cst (integer_type_node,
                                            len1),
                       str2,  build_int_cst (integer_type_node,
                                             len2));
                  /* See comments above.  call_expr = build_call_expr
                     (vlt_register_pairs_fndecl, 2, arg1, vtable);  */

                  append_to_statement_list (call_expr, &body);

                  /* Find and handle any 'extra' vtables associated
                     with this class, via virtual inheritance.   */
                  register_vptr_fields (arg1, current->class_type, body);

                  /* Find and handle any 'extra' vtables associated
                     with this class, via multiple inheritance.   */
                  register_other_binfo_vtables (binfo, body, arg1, str1, len1,
                                                str2, len2);
                }
            } /* if vtable_decl && vtable_should_be_output */
        }
      current = current->next;

    } /* while there's a node in the linked list */

  /* Handle left child */
  register_all_pairs (root->left, body);

  /* Handle right child */
  register_all_pairs (root->right, body);
}

static void
linked_list_insert (struct list_node **root, tree new_class)
{
  struct list_node *current;
  struct list_node *prev;
  bool found = false;

  for (prev = NULL, current = (*root); 
       current && !found; 
       prev = current, current = current->next)
    if (current 
	&& current->class_type == new_class)
      found = true;

  if (!found)
    {
      struct list_node *new_node = (struct list_node *)
                                           xmalloc (sizeof (struct list_node));

      new_node->class_type = new_class;
      new_node->next = NULL;

      if (!prev)
	(*root) = new_node;
      else
	prev->next = new_node;
    }
}

static void
template_info_tree_insert (struct node **root, tree template_type_id,
			   tree instantiated_type)
{
  if (!(*root))
    {
      struct node *new_node = (struct node *) xmalloc (sizeof (struct node));

      new_node->ptr_decl_or_template_type_id = template_type_id;
      new_node->left = NULL;
      new_node->right = NULL;
      new_node->class_list = NULL;
      linked_list_insert (&(new_node->class_list), instantiated_type);
      (*root) = new_node;
    }
  else if ((*root)->ptr_decl_or_template_type_id == template_type_id)
    linked_list_insert (&((*root)->class_list), instantiated_type);
  else if (template_type_id < (*root)->ptr_decl_or_template_type_id)
    template_info_tree_insert (&((*root)->left), template_type_id,
			       instantiated_type);
  else
    template_info_tree_insert (&((*root)->right), template_type_id,
			       instantiated_type);
}

static void
binary_tree_insert (struct node **root, tree ptr_decl, tree base_class,
                    tree new_class)
{
  /* DEBUG
  fprintf(stderr, "binary_tree_insert: base_class: \n");
  debug_tree(base_class);
  fprintf(stderr, "binary_tree_insert, derived_class: \n");
  debug_tree(new_class);
  */

  if (!(*root))
    {
      struct node *new_node = (struct node *) xmalloc (sizeof (struct node));
      new_node->ptr_decl_or_template_type_id = ptr_decl;
      new_node->left = NULL;
      new_node->right = NULL;
      new_node->class_list = NULL;
      linked_list_insert (&(new_node->class_list), base_class);
      linked_list_insert (&(new_node->class_list), new_class);
      (*root) = new_node;
    }
  else if ((DECL_NAME ((*root)->ptr_decl_or_template_type_id)) == (DECL_NAME (ptr_decl)))
    linked_list_insert (&((*root)->class_list), new_class);
  else if (ptr_decl < (*root)->ptr_decl_or_template_type_id)
    binary_tree_insert (&((*root)->left), ptr_decl, base_class, new_class);
  else
    binary_tree_insert (&((*root)->right), ptr_decl, base_class, new_class);
}

void
update_class_hierarchy_information (tree base_class_ptr_decl,
                                    tree base_class,
                                    tree derived_class)
{
  binary_tree_insert (&vlt_class_hierarchy_info, base_class_ptr_decl,
		      base_class, derived_class);
}

bool
register_class_hierarchy_information (tree body)
{
  tree call_expr;
  tree arg;
  tree arg2;
  bool ret_val = false;

  init_functions ();

  /* DEBUG */
  if (false)  /* This is here for debugging purposes. */
    dump_class_hierarchy_information (vlt_class_hierarchy_info);

  if (vlt_class_hierarchy_info != NULL)
    {
      /* Set permissions on vtable map data structure to be Read/Write. */

      arg = build_string_literal (strlen ("rw"), "rw");
      arg2 = build_int_cst (integer_type_node, 2);
      call_expr = build_call_expr (vlt_change_permission_fndecl, 2, arg, arg2);
      append_to_statement_list (call_expr, &body);

      /* Add class hierarchy pairs to the vtable map data structure. */

      /* compute_hierarchy_transitive_closure (); */
      register_all_pairs (vlt_class_hierarchy_info, body);

      /* Set permission on vtable map data structure to be Read-only.  */

      arg = build_string_literal (strlen ("ro"), "ro");
      call_expr = build_call_expr (vlt_change_permission_fndecl, 2, arg, arg2);
      append_to_statement_list (call_expr, &body);

      ret_val = true;  /* Actually did some work/wrote something out.  */
    }

  return ret_val;
}

tree
vtable_find_map_decl (tree var_id)
{
  struct varpool_node *node;

  for (node = varpool_nodes; node; node = node->next)
    {
      tree var_decl = node->decl;
      if (DECL_NAME (var_decl) == var_id)
        return var_decl;
    }
  return NULL_TREE;
}

void
record_template_vtable_info (tree instantiated_class_type,
			     tree template_class_type)
{
  tree type_id = NULL_TREE;

  if (instantiated_class_type == NULL_TREE
      || template_class_type == NULL_TREE)
    return;

  if (TYPE_NAME (template_class_type))
    {
      if (TREE_CODE (TYPE_NAME (template_class_type)) == IDENTIFIER_NODE)
	type_id = TYPE_NAME (template_class_type);
      else if (TREE_CODE (TYPE_NAME (template_class_type)) == TYPE_DECL)
	/* && DECL_NAME (TYPE_NAME (template_class_type))) */
	/* type_id = DECL_NAME (TYPE_NAME (template_class_type)); */
	type_id = get_mangled_id (TYPE_NAME (template_class_type));
    }

  if (type_id == NULL_TREE)
    return;

  template_info_tree_insert (&vlt_template_vptr_info, type_id,
			     instantiated_class_type);
}
