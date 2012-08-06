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
#include "tree-vtable-verify.h"
#include "gimple.h"

static GTY(()) tree vlt_register_pairs_fndecl = NULL_TREE;
static GTY(()) tree vlt_change_permission_fndecl = NULL_TREE;

struct work_node {
  struct vtv_graph_node *node;
  struct work_node *next;
};

static void init_functions (void);

static void dump_class_hierarchy_information (void);
static int  guess_num_vtable_pointers (struct vtv_graph_node *);
static void register_all_pairs (tree body);
static void add_hierarchy_pair (struct vtv_graph_node *,
                                struct vtv_graph_node *);
static struct vtv_graph_node *find_graph_node (tree);
static struct vtv_graph_node *
                  find_and_remove_next_leaf_node (struct work_node **worklist);
static bool vtv_register_class_hierarchy_information (tree register_pairs_body);

/* TODO: remove this. */
#if 0
static void vtv_create_unprotect_function            (void);
static void vtv_create_protect_function              (void);
#endif

void update_class_hierarchy_information (tree, tree);
struct vtbl_map_node *vtable_find_or_create_map_decl (tree);

static void
init_functions (void)
{
  tree void_ptr_type = build_pointer_type (void_type_node);
  tree arg_types = NULL_TREE;
  tree register_pairs_type = void_ptr_type;
  tree change_permission_type = void_ptr_type;
  tree char_ptr_type = build_pointer_type (char_type_node);

  arg_types = build_tree_list (NULL_TREE, integer_type_node);
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
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   integer_type_node));
  /* Start: Arg types to be removed when we remove debugging parameters from
     the library function. */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, char_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   integer_type_node));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, char_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   integer_type_node));
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
dump_class_hierarchy_information (void)
{
  struct vtbl_map_node *cur;
  unsigned i;

  for (cur = vtbl_map_nodes; cur; cur = cur->next)
    {
      fprintf (stdout, "Base class '%s' is inherited by: ",
               IDENTIFIER_POINTER
                   (DECL_NAME
                        (TREE_CHAIN (cur->class_info->class_type))));

      for (i = 0; i < num_vtable_map_nodes; ++i)
        if (TEST_BIT (cur->class_info->descendants, i))
          {
            struct vtbl_map_node *descendant = vtbl_map_nodes_array[i];
            tree class_type = descendant->class_info->class_type;
            fprintf (stdout, " (%s, %s)",
                     IDENTIFIER_POINTER (DECL_NAME
                                         TYPE_NAME (class_type)),
                     IDENTIFIER_POINTER (get_mangled_id
                                         (TREE_CHAIN (class_type))));
          }
      fprintf (stdout, "\n");
    }
}

static void
add_to_worklist (struct work_node **worklist, struct vtv_graph_node *node,
                 sbitmap inserted)
{
  struct work_node *new_work_node;

  if (TEST_BIT (inserted, node->class_uid))
    return;

  new_work_node = (struct work_node *) xmalloc (sizeof (struct work_node));
  new_work_node->next = *worklist;
  new_work_node->node = node;
  *worklist = new_work_node;

  SET_BIT (inserted, node->class_uid);
}

static struct vtv_graph_node *
find_and_remove_next_leaf_node (struct work_node **worklist)
{
  struct work_node *prev, *cur;

  for (prev = NULL, cur = *worklist; cur; prev = cur, cur = cur->next)
    {
      if (cur->node->num_children == cur->node->num_processed_children)
        {
          if (prev == NULL)
            (*worklist) = cur->next;
          else
            prev->next = cur->next;

          cur->next = NULL;
          return cur->node;
        }
    }

  return NULL;
}

void
vtv_compute_class_hierarchy_transitive_closure (void)
{
  struct work_node *worklist = NULL;
  struct vtbl_map_node *cur;
  sbitmap inserted = sbitmap_alloc (num_vtable_map_nodes);
  unsigned i;

  /* Note: Every node in the graph gets added to the worklist exactly
   once and removed from the worklist exactly once (when all of its
   children have been processed).  Each node's children edges are
   followed exactly once, and each node's parent edges are followed
   exactly once.  So this algorithm is roughly O(V + 2E), i.e.
   O(E + V). */

  /* Set-up:                                                                */
  /* Find all the "leaf" nodes in the graph, and add them to the worklist.  */
  sbitmap_zero (inserted);
  for (cur = vtbl_map_nodes; cur; cur = cur->next)
    {
      if (cur->class_info
          && (cur->class_info->num_children == 0)
          && ! (TEST_BIT (inserted, cur->class_info->class_uid)))
        add_to_worklist (&worklist, cur->class_info, inserted);
    }


  /* Main work: pull next leaf node off work list, process it, add its
     parents to the worklist, where a 'leaf' node is one that has no
     children, or all of its children have been processed. */
  while (worklist)
    {
      struct vtv_graph_node *temp_node =
                                  find_and_remove_next_leaf_node (&worklist);

      gcc_assert (temp_node != NULL);
      temp_node->descendants = sbitmap_alloc (num_vtable_map_nodes);
      sbitmap_zero (temp_node->descendants);
      SET_BIT (temp_node->descendants, temp_node->class_uid);
      for (i = 0; i < temp_node->num_children; ++i)
        sbitmap_a_or_b (temp_node->descendants, temp_node->descendants,
                        temp_node->children[i]->descendants);
      for (i = 0; i < temp_node->num_parents; ++i)
        {
          temp_node->parents[i]->num_processed_children =
                    temp_node->parents[i]->num_processed_children + 1;
          if (!TEST_BIT (inserted, temp_node->parents[i]->class_uid))
            add_to_worklist (&worklist, temp_node->parents[i], inserted);
        }
    }
}

static bool
record_register_pairs (tree vtable_decl, tree vptr_address,
                       tree base_class)
{
  unsigned offset = TREE_INT_CST_LOW (TREE_OPERAND (vptr_address, 1));
  tree base_id;
  struct vtbl_map_node *base_vtable_map_node;

  if (TREE_CHAIN (base_class))
    base_id = DECL_ASSEMBLER_NAME (TREE_CHAIN (base_class));
  else
    base_id = DECL_ASSEMBLER_NAME (TYPE_NAME (base_class));

  base_vtable_map_node = vtbl_map_get_node (base_id);

  if (vtbl_map_node_registration_find (base_vtable_map_node, vtable_decl,
                                       offset))
    return true;

  vtbl_map_node_registration_insert (base_vtable_map_node, vtable_decl,
                                       offset);
  return false;
}

static void
register_vptr_fields (tree base_class_decl_arg, tree base_class,
                      tree record_type, tree body)
{
  /* A class may contain secondary vtables in it, for various
     reasons.  This function goes through the decl chain of a class
     record looking for any fields that point to secondary vtables,
     and adding calls to __VLTRegisterPair for the secondary vtable
     pointers.  */

  tree vtbl_var_decl;
  tree arg1;
  tree arg2;
  int hint = 0;

  if (TREE_CODE (record_type) != RECORD_TYPE)
    return;

  vtbl_var_decl = get_vtbl_decl_for_binfo (TYPE_BINFO (record_type));

  if (vtbl_var_decl)
    {
      tree ztt_decl = DECL_CHAIN (vtbl_var_decl);
      bool already_registered = false;

      /* construction vtable */
      if (ztt_decl != NULL_TREE
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
                  tree val_vtbl_decl = TREE_OPERAND (TREE_OPERAND (value, 0),
                                                     0);
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

                  already_registered = record_register_pairs (val_vtbl_decl,
                                                              value,
                                                              base_class);

                  if (already_registered)
                    continue;

		  /* This call expr has the 3 "real" arguments, plus 4
		     debugging arguments.  Eventually it will be
		     replaced with the one just below it, which only
		     has the 3 real arguments.  */
                  call_expr = build_call_expr
		                        (vlt_register_pairs_fndecl, 7,
					 base_class_decl_arg, value,
                                         build_int_cst (integer_type_node,
                                                        hint),
					 arg1,
					 build_int_cst (integer_type_node,
							len1),
					 arg2,
					 build_int_cst (integer_type_node,
							len2));
		  /* See comments above.
                  call_expr = build_call_expr (vlt_register_pairs_fndecl, 3,
                                               base_class_decl_arg, value,
                                               buid_int_cst (integer_type_node,
                                                             hint));
		  */
		  append_to_statement_list (call_expr, &body);
                }
            }
        }
    }
}

static void
register_other_binfo_vtables (tree binfo, tree body, tree arg1, tree str1,
                              int len1, tree str2, int len2, tree base_class)
{
  unsigned ix;
  tree base_binfo;
  tree vtable_decl;
  bool already_registered;
  int hint = 0;

  if (binfo == NULL_TREE)
    return;

  for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
    {
      if ((!BINFO_PRIMARY_P (base_binfo)
           || BINFO_VIRTUAL_P (base_binfo))
          && (vtable_decl=get_vtbl_decl_for_binfo (base_binfo))
          && !(DECL_VTABLE_OR_VTT_P(vtable_decl)
               && DECL_CONSTRUCTION_VTABLE_P(vtable_decl)))
        {
          tree vtable_address = build_vtbl_address (base_binfo);
          tree call_expr;

          already_registered = record_register_pairs (vtable_decl,
                                                      vtable_address,
                                                      base_class);
          if (!already_registered)
            {
              call_expr = build_call_expr (vlt_register_pairs_fndecl, 7,
                                           arg1, vtable_address,
                                           build_int_cst (integer_type_node,
                                                          hint),
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
                                    len2, base_class);
    }
}

static int
guess_num_vtable_pointers (struct vtv_graph_node *class_node)
{
  tree vtbl;
  int total_num_vtbls = 0;
  unsigned i;

  for (i = 0; i < num_vtable_map_nodes; ++i)
    if (TEST_BIT (class_node->descendants, i))
      {
        tree class_type = vtbl_map_nodes_array[i]->class_info->class_type;
        for (vtbl = CLASSTYPE_VTABLES (class_type); vtbl;
             vtbl = DECL_CHAIN (vtbl))
          total_num_vtbls ++;
      }
  return total_num_vtbls;
}

static void
register_all_pairs (tree body)
{
  struct vtbl_map_node *current;
  tree base_ptr_var_decl;


  for (current = vtbl_map_nodes; current; current = current->next)
    {
      unsigned i;
      tree base_class = current->class_info->class_type;
      int size_hint = guess_num_vtable_pointers (current->class_info);
      base_ptr_var_decl = current->vtbl_map_decl;

      gcc_assert (current->class_info != NULL);

      for (i = 0; i < num_vtable_map_nodes; ++i)
        if (TEST_BIT (current->class_info->descendants, i))
          {
            struct vtbl_map_node *vtbl_class_node = vtbl_map_nodes_array[i];
            tree class_type = vtbl_class_node->class_info->class_type;

            if (class_type
                && (TREE_CODE (class_type) == RECORD_TYPE))
            {
              tree new_type;
              tree arg1;
              tree call_expr;
              bool already_registered;

              tree binfo = TYPE_BINFO (class_type);
              tree vtable_decl;
              bool vtable_should_be_output = false;

              vtable_decl = CLASSTYPE_VTABLES (class_type);

              /* Handle main vtable for this class. */

              if (vtable_decl)
                {
                  struct varpool_node *node = varpool_node (vtable_decl);
                  vtable_should_be_output = node->needed;
                }

              if (vtable_decl && vtable_should_be_output
                  && BINFO_VTABLE (binfo))
                {
                  tree vtable_address = build_vtbl_address (binfo);
                  int len1  = IDENTIFIER_LENGTH
                                               (DECL_NAME (base_ptr_var_decl));
                  int len2  = IDENTIFIER_LENGTH (DECL_NAME (vtable_decl));
                  tree str1 = build_string_literal (len1,
                                                    IDENTIFIER_POINTER
                                                    (DECL_NAME
                                                     (base_ptr_var_decl)));
                  tree str2 = build_string_literal (len2,
                                                    IDENTIFIER_POINTER
                                                    (DECL_NAME (vtable_decl)));

                  already_registered = record_register_pairs (vtable_decl,
                                                              vtable_address,
                                                              base_class);

                  if (!already_registered)
                    {
                      new_type = build_pointer_type (TREE_TYPE
                                                     (base_ptr_var_decl));
                      arg1 = build1 (ADDR_EXPR, new_type, base_ptr_var_decl);

                      /* This call expr has the 3 "real" arguments, plus 4
                         debugging arguments.  Eventually it will be replaced
                         with the one just below it, which only has the 2 real
                         arguments.  */
                      call_expr = build_call_expr
                          (vlt_register_pairs_fndecl, 7,
                           arg1, vtable_address,
                           build_int_cst (integer_type_node, size_hint),
                           str1, build_int_cst (integer_type_node,
                                                len1),
                           str2,  build_int_cst (integer_type_node,
                                                 len2));
                      /* See comments above.  call_expr = build_call_expr
                         (vlt_register_pairs_fndecl, 3, arg1, vtable,
                         build_int_cst (integer_type_node, size_hint));  */

                      append_to_statement_list (call_expr, &body);

                      /* Find and handle any 'extra' vtables associated
                         with this class, via virtual inheritance.   */
                      register_vptr_fields (arg1, base_class, class_type,
                                            body);

                      /* Find and handle any 'extra' vtables associated
                         with this class, via multiple inheritance.   */
                      register_other_binfo_vtables (binfo, body, arg1, str1,
                                                    len1, str2, len2,
                                                    base_class);
                    }
                } /* if vtable_decl && vtable_should_be_output */
            } /* if TREE_TYPE (class_type) == RECORD... */
          } /* if TEST_BIT (descendants, i) */
    } /* for cur = vtbl_map_nodes... */
}

static struct vtv_graph_node *
find_graph_node (tree class_type)
{
  tree class_decl = TREE_CHAIN (class_type);
  tree class_name_id;
  struct vtbl_map_node *vtbl_node;

  if (class_decl)
    class_name_id = DECL_ASSEMBLER_NAME (class_decl);
  else
    class_name_id = DECL_ASSEMBLER_NAME (TYPE_NAME (class_type));

  vtbl_node = vtbl_map_get_node (class_name_id);

  if (vtbl_node)
    return vtbl_node->class_info;

  return NULL;
}

static void
add_edge_to_graph (struct vtv_graph_node ***edge_array, unsigned *num_entries,
                   unsigned *max_entries, struct vtv_graph_node *new_entry)
{
  /* Check array size, and re-size it if necessary.  */
  if (*num_entries >= ((*max_entries) - 1))
    {
      unsigned new_size = 2 * (*max_entries);
      unsigned i;
      *edge_array = (struct vtv_graph_node **)
          xrealloc (*edge_array, new_size * sizeof (struct vtv_graph_node *));

      for (i = *max_entries; i < new_size; ++i)
        (*edge_array)[i] = NULL;
      *max_entries = new_size;
    }

  (*edge_array)[*num_entries] = new_entry;
  *num_entries = (*num_entries) + 1;
}

static void
add_hierarchy_pair (struct vtv_graph_node *base_node,
                    struct vtv_graph_node *derived_node)
{
  add_edge_to_graph (&(base_node->children), &(base_node->num_children),
                     &(base_node->max_children), derived_node);
  add_edge_to_graph (&(derived_node->parents), &(derived_node->num_parents),
                     &(derived_node->max_parents), base_node);
}

void
update_class_hierarchy_information (tree base_class,
                                    tree derived_class)
{
  struct vtv_graph_node *base_node = find_graph_node (base_class);
  struct vtv_graph_node *derived_node = find_graph_node (derived_class);

  add_hierarchy_pair (base_node, derived_node);
}

bool
vtv_register_class_hierarchy_information (tree register_pairs_body)
{
  bool ret_val = false;

  init_functions ();

  /* DEBUG */
  if (false)  /* This is here for debugging purposes. */
    dump_class_hierarchy_information ();

  /* TODO: Temp fix. Needs to be tighten */
  /*  if (any_verification_calls_generated) */
  if (num_vtable_map_nodes > 0)
    {
      /* If this function is going into the preinit_array, then we
         need to manually call __VLTChangePermission, rather than
         depending on initialization prioritys in vtv_init. */
      if (flag_vtable_verify == VTV_PREINIT_PRIORITY)
        {
          /* Pass __VLTP_READ_WRITE value as defined in vtv_rts.h */
          tree arg_read_write = build_int_cst (integer_type_node, 1);
          tree call_expr = build_call_expr (vlt_change_permission_fndecl,
                                            1, arg_read_write);
          append_to_statement_list (call_expr, &register_pairs_body);
        }

      /* Add class hierarchy pairs to the vtable map data structure. */
      register_all_pairs (register_pairs_body);

      /* If this function is going into the preinit_array, then we
         need to manually call __VLTChangePermission, rather than
         depending on initialization prioritys in vtv_init. */
      if (flag_vtable_verify == VTV_PREINIT_PRIORITY)
        {
          tree arg_read_only = build_int_cst (integer_type_node, 0);
          tree call_expr = build_call_expr (vlt_change_permission_fndecl,
                                            1, arg_read_only);
          append_to_statement_list (call_expr, &register_pairs_body);
        }

      ret_val = true;  /* Actually did some work/wrote something out.  */
    }

  return ret_val;
}

/* Generate the special constructor function that calls
   __VLTChangePermission and __VLTRegisterPairs, and give it a very high initialization 
   priority.  */

void
vtv_generate_init_routine(const char * filename)
{
  const char * cwd = filename;
  char temp_name[58];
  tree register_pairs_body;
  char * cptr;
  int i;
  bool vtable_classes_found = false;

  /* The last part of the directory tree will be where it
     differentiates; the first part may be the same. */
  if (strlen (cwd) > 50)
    {
      int pos = (strlen (cwd) - 50);
      cwd = cwd + pos;
    }

  /* TODO: Are these all the chars we need to map? */
  sprintf (temp_name, "%.50s.vtable", cwd);
  for (cptr = temp_name, i = 0;
       (cptr[0] != '\0') && (i < 50);
       cptr++, i++)
    if ((cptr[0] == '/') || (cptr[0] == '-') || (cptr[0] == '+'))
      cptr[0] = '_';

  push_lang_context (lang_name_c);

  /* The priority for this init function (constructor) is carefully chosen
     so that it will happen after the calls to unprotect the memory used for 
     vtable verification and before the memory is protected again */
  register_pairs_body = start_objects ('I', MAX_RESERVED_INIT_PRIORITY - 1,
                                       (const char *) temp_name);

  vtable_classes_found =
      vtv_register_class_hierarchy_information (register_pairs_body);

  if (vtable_classes_found)
    {
      current_function_decl =
          finish_objects ('I', MAX_RESERVED_INIT_PRIORITY - 1, register_pairs_body);
      allocate_struct_function (current_function_decl, false);
      TREE_STATIC (current_function_decl) = 1;
      TREE_USED (current_function_decl) = 1;
      DECL_PRESERVE_P (current_function_decl) = 1;
      if (flag_vtable_verify == VTV_PREINIT_PRIORITY)
      {
        DECL_STATIC_CONSTRUCTOR (current_function_decl) = 0;
        assemble_vtv_preinit_initializer (current_function_decl);
      }
      gimplify_function_tree (current_function_decl);
      cgraph_add_new_function (current_function_decl, false);

      /*
      vtv_create_unprotect_function();
      vtv_create_protect_function();
      */

      cgraph_process_new_functions ();
    }
  pop_lang_context ();
}

struct vtbl_map_node *
vtable_find_or_create_map_decl (tree base_type)
{
  tree base_decl = TREE_CHAIN (base_type);
  tree base_id;
  tree var_decl = NULL;
  char *var_name = NULL;
  struct vtbl_map_node *vtable_map_node = NULL;


  /* Verify the type has an associated vtable */
  if (!TYPE_BINFO (base_type) || !BINFO_VTABLE (TYPE_BINFO (base_type)))
    return NULL;

  if (base_decl)
    base_id = DECL_ASSEMBLER_NAME (base_decl);
  else
    base_id = DECL_ASSEMBLER_NAME (TYPE_NAME (base_type));

  /* Create map lookup symbol for base class */
  var_name = ACONCAT (("_ZTV", IDENTIFIER_POINTER (base_id),
                       "__vtable_map", NULL));
  if (base_id)
    /* We've already created the variable; just look it.  */
    vtable_map_node = vtbl_map_get_node (base_id);

  if (!vtable_map_node || (vtable_map_node->vtbl_map_decl == NULL_TREE))
    {
      /* If we haven't already created the *__vtable_map
         global variable for this class, do so now, and
         add it to the varpool, to make sure it gets saved
         and written out.  */

      char *sect_name = NULL;
      tree var_type = build_pointer_type (void_type_node);
      tree initial_value = build_int_cst
          (make_node (INTEGER_TYPE), 0);
      var_decl  = build_decl (UNKNOWN_LOCATION, VAR_DECL,
                              get_identifier (var_name),
                              var_type);
      TREE_PUBLIC (var_decl) = 1;
      DECL_EXTERNAL (var_decl) = 0;
      TREE_STATIC (var_decl) = 1;
      SET_DECL_ASSEMBLER_NAME (var_decl,
                               get_identifier (var_name));
      DECL_ARTIFICIAL (var_decl) = 1;
      TREE_READONLY (var_decl) = 1;
      DECL_IGNORED_P (var_decl) = 1;

      /* Put these mmap variables in to data.rel.ro sections.
	 It turns out this needs a previous fix in binutils as
	 explained here:
         http://sourceware.org/ml/binutils/2011-05/msg00083.html
      */

      sect_name = ACONCAT ((".data.rel.ro.", "vtable_map_vars",
                            NULL));
      DECL_SECTION_NAME (var_decl) =
          build_string (strlen (sect_name), sect_name);
      DECL_HAS_IMPLICIT_SECTION_NAME_P (var_decl) = true;
      DECL_COMDAT_GROUP (var_decl) = get_identifier (var_name);
      DECL_INITIAL (var_decl) = initial_value;

      varpool_finalize_decl (var_decl);
      if (!vtable_map_node)
        vtable_map_node = vtbl_map_node (base_type);
      if (vtable_map_node->vtbl_map_decl == NULL_TREE)
        vtable_map_node->vtbl_map_decl = var_decl;
    }

  gcc_assert (vtable_map_node);
  return vtable_map_node;
}

void
vtv_save_base_class_info (tree type)
{
  if (flag_vtable_verify)
    {
      tree binfo =  TYPE_BINFO (type);
      tree base_binfo;
      struct vtbl_map_node *own_map;
      int i;

      /* first make sure to create the map for this record type */
      own_map = vtable_find_or_create_map_decl (type);
      if (own_map == NULL)
        return;

      /* Go through the list of all base classes for the current (derived)
         type, make sure the *__vtable_map global variable for the base class
	 exists, and add the base class/derived class pair to the class
	 hierarchy information we are accumulating (for vtable pointer
	 verification).  */
      for (i = 0; BINFO_BASE_ITERATE(binfo, i, base_binfo); i++)
        {
          tree tree_val = BINFO_TYPE(base_binfo);
          struct vtbl_map_node *vtable_map_node = NULL;

          vtable_map_node = vtable_find_or_create_map_decl (tree_val);

          if (vtable_map_node != NULL)
            update_class_hierarchy_information (tree_val, type);
        }
    }
}
