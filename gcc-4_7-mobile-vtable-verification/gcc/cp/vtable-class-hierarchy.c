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

/* Virtual Table Pointer Security Pass - Detect corruption of vtable pointers
   before using them for virtual method dispatches.  */

/* This file is part of the vtable security feature implementation.
   The vtable security feature is designed to detect when a virtual
   call is about to be made through an invalid vtable pointer
   (possibly due to data corruption or malicious attacks). The
   compiler finds every virtual call, and inserts a verification call
   before the virtual call.  The verification call takes the actual
   vtable pointer value in the object through which the virtual call
   is being made, and compares the vtable pointer against a set of all
   valid vtable pointers that the object could contain (this set is
   based on the declared type of the object).  If the pointer is in
   the valid set, execution is allowed to continue; otherwise the
   program is halted.

  There are several pieces needed in order to make this work: 1. For
  every virtual class in the program (i.e. a class that contains
  virtual methods), we need to build the set of all possible valid
  vtables that an object of that class could point to.  This includes
  vtables for any class(es) that inherit from the class under
  consideration.  2. For every such data set we build up, we need a
  way to find and reference the data set.  This is complicated by the
  fact that the real vtable addresses are not known until runtime,
  when the program is loaded into memory, but we need to reference the
  sets at compile time when we are inserting verification calls into
  the program.  3.  We need to find every virtual call in the program,
  and insert the verification call (with the appropriate arguments)
  before the virtual call.  4. We need some runtime library pieces:
  the code to build up the data sets at runtime; the code to actually
  perform the verification using the data sets; and some code to set
  protections on the data sets, so they themselves do not become
  hacker targets.

  To find and reference the set of valid vtable pointers for any given
  virtual class, we create a special global varible for each virtual
  class.  We refer to this as the "vtable map variable" for that
  class.  The vtable map variable has the type "void *", and is
  initialized by the compiler to NULL.  At runtime when the set of
  valid vtable pointers for a virtual class, e.g. class Foo, is built,
  the vtable map variable for class Foo is made to point to the set.
  During compile time, when the compiler is inserting verification
  calls into the program, it passes the vtable map variable for the
  appropriate class to the verification call, so that at runtime the
  verification call can find the appropriate data set.

  The actual set of valid vtable pointers for a virtual class,
  e.g. class Foo, cannot be built until runtime, when the vtables get
  loaded into memory and their addresses are known.  But the knowledge
  about which vtables belong in which class' hierarchy is only known
  at compile time.  Therefore at compile time we collect class
  hierarchy and vtable information about every virtual class, and we
  generate calls to build up the data sets at runtime.  To build the
  data sets, we call one of the functions we add to the runtime
  library, __VLTRegisterPair.  __VLTRegisterPair takes two arguments,
  a vtable map variable and the address of a vtable.  If the vtable
  map variable is currently NULL, it creates a new data set (hash
  table), makes the vtable map variable point to the new data set, and
  inserts the vtable address into the data set.  If the vtable map
  variable is not NULL, it just inserts the vtable address into the
  data set.  In order to make sure that our data sets are built before
  any verification calls happen, we create a special constructor
  initialization function for each compilation unit, give it a very
  high initialization priority, and insert all of our calls to
  __VLTRegisterPair into our special constructor initialization
  function.

  The vtable verification feature is controlled by the flag
  '-fvtable-verify='.  There are three flavors of this:
  '-fvtable-verify=std', '-fvtable-verify=preinit', and
  '-fvtable-verify=none'.  If the option '-fvtable-verfy=preinit' is
  used, then our constructor initialization function gets put into the
  preinit array.  This is necessary if there are data sets that need
  to be built very early in execution.  If the constructor
  initialization function gets put into the preinit array, the we also
  add calls to __VLTChangePermission at the beginning and end of the
  function.  The call at the beginning sets the permissions on the
  data sets and vtable map variables to read/write, and the one at the
  end makes them read-only.  If the '-fvtable-verify=std' option is
  used, the constructor initialization functions are executed at their
  normal time, and the __VLTChangePermission calls are handled
  differently (see the comments in libstdc++-v3/libsupc++/vtv_rts.cc).
  The option '-fvtable-verify=none' turns off vtable verification.

  This file contains code to find and record the class hierarchies for
  the virtual classes in a program, and all the vtables associated
  with each such class; to generate the vtable map variables; and to
  generate the constructor initialization function (with the calls to
  __VLTRegisterPair, and __VLTChangePermission).  The main data
  structures used for collecting the class hierarchy data and
  building/maintaining the vtable map variable data are defined in
  gcc/tree-vtable-verify.h, because they are used both here and in
  gcc/tree-vtable-verify.c.  */

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
#include "tree-iterator.h"
#include "tree-vtable-verify.h"
#include "gimple.h"

/* Mark these specially since they need to be stored in precompiled
   header IR.  */
static GTY (()) tree vlt_saved_class_info = NULL_TREE;
static GTY (()) tree vlt_register_pairs_fndecl = NULL_TREE;
static GTY (()) tree vlt_init_set_symbol_fndecl = NULL_TREE;
static GTY (()) tree vlt_change_permission_fndecl = NULL_TREE;

struct work_node {
  struct vtv_graph_node *node;
  struct work_node *next;
};

struct vtbl_map_node *vtable_find_or_create_map_decl (tree);

/* As part of vtable verification the compiler generates and inserts calls
   to __VLTRegisterPair and __VLTChangePermission, which are in libsupc++.
   This function builds and initializes the function decls that are used
   in generating those function calls.

   In addition to __VLTRegisterPair there is also __VLTRegisterPairDebug
   which can be used in place of __VLTRegisterPair, and which takes extra
   parameters and outputs extra information, to help debug problems.  The
   debug version of this function is generated and used if VTV_DEBUG is
   defined.

   The signatures for these functions are:

   void __VLTChangePermission (int);
   void __VLTRegisterPair (void **, void*, int);
   void __VLTRegisterPairDebug (void**, void *, int, char *, int, char *, int);
*/

static void
init_functions (void)
{
  tree void_ptr_type = build_pointer_type (void_type_node);
  tree arg_types = NULL_TREE;
  tree change_permission_type = void_type_node;
  tree register_pairs_type = void_type_node;
  tree init_set_symbol_type = void_type_node;
#ifdef VTV_DEBUG
  tree const_char_ptr_type = build_pointer_type (build_qualified_type
                                                           (char_type_node,
                                                            TYPE_QUAL_CONST));
#endif

  if (vlt_change_permission_fndecl != NULL_TREE)
    return;

  gcc_assert (vlt_register_pairs_fndecl == NULL_TREE);

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
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   const_ptr_type_node));

#ifdef VTV_DEBUG
  /* These arguments are only used by the debug version of RegisterPair */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   const_char_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   const_char_ptr_type));
#endif

  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_type_node));

  register_pairs_type = build_function_type (register_pairs_type, arg_types);

#ifdef VTV_DEBUG
  /* void
     __VLTRegisterPairDebug (void ** set_handle_ptr, const void * vtable_ptr,
                        const char * set_symbol_name, const char * vtable_name)
  */

  vlt_register_pairs_fndecl = build_fn_decl ("__VLTRegisterPairDebug",
                                             register_pairs_type);
#else
   /* void __VLTRegisterPair (void **set_handle_ptr, const void *vtable_ptr) */
 vlt_register_pairs_fndecl = build_fn_decl ("__VLTRegisterPair",
                                             register_pairs_type);
#endif

  TREE_NOTHROW (vlt_register_pairs_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_register_pairs_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES (vlt_register_pairs_fndecl));
  TREE_PUBLIC (vlt_register_pairs_fndecl) = 1;
  DECL_PRESERVE_P (vlt_register_pairs_fndecl) = 1;
  retrofit_lang_decl (vlt_register_pairs_fndecl);
  SET_DECL_LANGUAGE (vlt_register_pairs_fndecl, lang_cplusplus);

  arg_types = build_tree_list (NULL_TREE, build_pointer_type (void_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   const_ptr_type_node));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   size_type_node));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_type_node));

  init_set_symbol_type = build_function_type (init_set_symbol_type, arg_types);

#ifdef VTV_DEBUG
  /* void __VLTInitSetSymbolDebug(void ** set_handle_ptr, 
                                 const void * set_symbol_key,
                                 size_t size_hint)
  */
  vlt_init_set_symbol_fndecl = build_fn_decl ("__VLTInitSetSymbolDebug",
                                              init_set_symbol_type);
#else
  /* void __VLTInitSetSymbol(void ** set_handle_ptr, 
                             const void * set_symbol_key,
                             size_t size_hint)
  */
  vlt_init_set_symbol_fndecl = build_fn_decl ("__VLTInitSetSymbol",
                                              init_set_symbol_type);
#endif

  TREE_NOTHROW (vlt_init_set_symbol_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_init_set_symbol_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES (vlt_init_set_symbol_fndecl));
  TREE_PUBLIC (vlt_init_set_symbol_fndecl) = 1;
  DECL_PRESERVE_P (vlt_init_set_symbol_fndecl) = 1;
  retrofit_lang_decl (vlt_init_set_symbol_fndecl);
  SET_DECL_LANGUAGE (vlt_init_set_symbol_fndecl, lang_cplusplus);
}

/* This is a helper function for
   vtv_compute_class_hierarchy_transitive_closure.  It adds a
   vtv_graph_node to the WORKLIST, which is a linked list of
   seen-but-not-yet-processed nodes.  INSERTED is a bitmap, one bit
   per node, to help make sure that we don't insert a node into the
   worklist more than once.  Each node represents a class somewhere in
   our class hierarchy information. Every node in the graph gets added
   to the worklist exactly once and removed from the worklist exactly
   once (when all of its children have been processed).  */

static void
add_to_worklist (struct work_node **worklist, struct vtv_graph_node *node,
                 sbitmap inserted)
{
  struct work_node *new_work_node;

  if (TEST_BIT (inserted, node->class_uid))
    return;

  new_work_node = XNEW (struct work_node);
  new_work_node->next = *worklist;
  new_work_node->node = node;
  *worklist = new_work_node;

  SET_BIT (inserted, node->class_uid);
}

/* This is a helper function for
   vtv_compute_class_hierarchy_transitive_closure.  It goes through
   the WORKLIST of class hierarchy nodes looking for a "leaf" node,
   i.e. a node whose children in the hierarchy have all been
   processed.  When it finds the next leaf node, it removes it from
   the linked list (WORKLIST) and returns the node.  */

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

/* In our class hierarchy graph, each class node contains a bitmap,
   with one bit for each class in the hierarchy.  The bits are set for
   classes that are descendants in the graph of the current node.
   Initially the descendants bitmap is only set for immediate
   descendants.  This function traverses the class hierarchy graph,
   bottom up, filling in the transitive closures for the descendants
   as we rise up the graph.  */

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
   O(E + V).  */

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
     children, or all of its children have been processed.  */
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

/* Keep track of which pairs we have already created __VLTRegisterPair
   calls for, to prevent creating duplicate calls within the same
   compilation unit.  VTABLE_DECL is the var decl for the vtable of
   the (descendant) class that we are adding to our class hierarchy
   data.  VPTR_ADDRESS is and expression for calculating the correct
   offset into the vtable (VTABLE_DECL).  It is the actual vtable
   pointer address that will be stored in our list of valid vtable
   pointers for BASE_CLASS.  BASE_CLASS is the record_type node for
   the base class to whose hiearchy we want to add
   VPTR_ADDRESS. (VTABLE_DECL should be the vtable for BASE_CLASS or
   one of BASE_CLASS' descendents.  */

static bool
record_register_pairs (tree vtable_decl, tree vptr_address,
                       tree base_class)
{
  unsigned offset;
  struct vtbl_map_node *base_vtable_map_node;

  if (TREE_OPERAND_LENGTH (vptr_address) == 1)
    {
      tree tmp_address = TREE_OPERAND (vptr_address, 0);
      offset = TREE_INT_CST_LOW (TREE_OPERAND (tmp_address, 1));
    }
  else
    offset = TREE_INT_CST_LOW (TREE_OPERAND (vptr_address, 1));
  
  base_vtable_map_node = vtbl_map_get_node (base_class);

  if (vtbl_map_node_registration_find (base_vtable_map_node, vtable_decl,
                                       offset))
    return true;

  vtbl_map_node_registration_insert (base_vtable_map_node, vtable_decl,
                                     offset);
  return false;
}

/* A class may contain secondary vtables in it, for various reasons.
   This function goes through the decl chain of a class record looking
   for any fields that point to secondary vtables, and adding calls to
   __VLTRegisterPair for the secondary vtable pointers.

   BASE_CLASS_DECL_ARG is an expression for the address of the vtable
   map variable for the BASE_CLASS (whose hierarchy we are currently
   updating).  BASE_CLASS is the record_type node for the base class.
   RECORD_TYPE is the record_type node for the descendant class that
   we are possibly adding to BASE_CLASS's hierarchy.  BODY is the
   function body for the constructor init function to which we are
   adding our calls to __VLTRegisterPair.  */

static void
register_vptr_fields (tree base_class_decl_arg, tree base_class,
                      tree record_type, tree body)
{
  tree vtbl_var_decl;

  if (TREE_CODE (record_type) != RECORD_TYPE)
    return;

  vtbl_var_decl = get_vtbl_decl_for_binfo (TYPE_BINFO (record_type));

  if (vtbl_var_decl)
    {
      tree ztt_decl = DECL_CHAIN (vtbl_var_decl);
      bool already_registered = false;

      /* Check to see if we have found a constructor vtable.  Add its
         data if appropriate.  */
      if (ztt_decl != NULL_TREE && (DECL_NAME (ztt_decl))
          && (strncmp (IDENTIFIER_POINTER (DECL_NAME (ztt_decl)),
                       "_ZTT", 4) == 0))
        {
          tree values = DECL_INITIAL (ztt_decl);
          struct varpool_node *vp_node = varpool_node (ztt_decl);
          if (vp_node->finalized
              && TREE_ASM_WRITTEN (ztt_decl)
              && values != NULL_TREE
              && TREE_CODE (values) == CONSTRUCTOR
              && TREE_CODE (TREE_TYPE (values)) == ARRAY_TYPE)
            {
              tree call_expr = NULL_TREE;
              unsigned HOST_WIDE_INT cnt;
              constructor_elt *ce;
#ifdef VTV_DEBUG
              int len1 = strlen (IDENTIFIER_POINTER
                                 (DECL_NAME (TREE_OPERAND (base_class_decl_arg,
                                                           0))));
              tree arg1 = build_string_literal (len1 + 1,
                                                IDENTIFIER_POINTER
                                                  (DECL_NAME
                                                     (TREE_OPERAND
                                                        (base_class_decl_arg,
                                                         0))));
#endif
              /* Loop through the initialization values for this vtable to
                 get all the correct vtable pointer addresses that we need
                 to add to our set of valid vtable pointers for the current
                 base class.  */

              for (cnt = 0;
                   VEC_iterate (constructor_elt, CONSTRUCTOR_ELTS (values),
				cnt, ce);
                   cnt++)
                {
                  tree value = ce->value;
                  tree val_vtbl_decl;

		  /* We need to check value and find the bit where we have
		     something with 2 arguments, the first argument of which
		     is an ADDR_EXPR and the second argument of which is
		     an INTEGER_CST.  */

		  while (value && TREE_OPERAND_LENGTH (value) == 1
			 && TREE_CODE (TREE_OPERAND (value, 0)) == ADDR_EXPR)
		    value = TREE_OPERAND (value, 0);

		  /*
		  gcc_assert (TREE_OPERAND_LENGTH (value) >= 2
			      && TREE_CODE (TREE_OPERAND (value, 0)) == ADDR_EXPR
			      && TREE_CODE (TREE_OPERAND (value, 1)) == INTEGER_CST);
		  */
		  /* The VAR_DECL for the vtable should be the first argument of
		     the ADDR_EXPR, which is the first argument of value.*/

		  if (TREE_OPERAND (value, 0))
		    val_vtbl_decl = TREE_OPERAND (value, 0);

		  while (TREE_CODE (val_vtbl_decl) != VAR_DECL
			 && TREE_OPERAND (val_vtbl_decl, 0))
		    val_vtbl_decl = TREE_OPERAND (val_vtbl_decl, 0);

                  gcc_assert (TREE_CODE (val_vtbl_decl) == VAR_DECL);

                  /* Check to see if we already have this vtable pointer in
                     our valid set for this base class.  */
                  already_registered = record_register_pairs (val_vtbl_decl,
                                                              value,
                                                              base_class);

                  if (already_registered)
                    continue;
#ifdef VTV_DEBUG
                  {
                    int len2 = strlen (IDENTIFIER_POINTER
                                                (DECL_NAME (val_vtbl_decl)));
                    tree arg2 = build_string_literal (len2 + 1,
                                                      IDENTIFIER_POINTER
                                                        (DECL_NAME
                                                           (val_vtbl_decl)));

                    /* Generate the call to __VLTRegisterPairDebug to
                       add this vtable pointer to our set of valid
                       pointers for the base class.  */

                    call_expr = build_call_expr (vlt_register_pairs_fndecl, 4,
                                                 base_class_decl_arg, value,
                                                 arg1, arg2);
                  }
#else
                  /* Generate the call to __VLTRegisterPair to add
                     this vtable pointer to our set of valid pointers
                     for the base class.  */

                  call_expr = build_call_expr (vlt_register_pairs_fndecl, 2,
                                               base_class_decl_arg, value);
#endif
                  append_to_statement_list (call_expr, &body);
                }
            }
        }
    }
}

/* This function iterates through all the vtables it can find from the
   BINFO of a class, to make sure we have found ALL of the vtables
   that an object of that class could point to.  Generate calls to
   __VLTRegisterPair for those vtable pointers that we find.

   BINFO is the tree_binfo node for the BASE_CLASS.  BODY is the
   function body for the constructor init function to which we are
   adding calls to __VLTRegisterPair.  ARG1 is an expression for the
   address of the vtable map variable (for the BASE_CLASS), that will
   point to the updated data set.  BASE_CLASS is the record_type node
   for the base class whose set of valid vtable pointers we are
   updating. STR1 and STR2 are all debugging information, to be passed
   as parameters to __VLTRegisterPairDebug.  STR1 represents the name
   of the vtable map variable to be updated by the call.  Similarly,
   STR2 represents the name of the class whose vtable pointer is being
   added to the hierarchy.  */

static void
register_other_binfo_vtables (tree binfo, tree body, tree arg1, tree str1,
                              tree str2, tree base_class)
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
          && (vtable_decl = get_vtbl_decl_for_binfo (base_binfo))
          && !(DECL_VTABLE_OR_VTT_P (vtable_decl)
               && DECL_CONSTRUCTION_VTABLE_P (vtable_decl)))
        {
          tree vtable_address = build_vtbl_address (base_binfo);
          tree call_expr;

          already_registered = record_register_pairs (vtable_decl,
                                                      vtable_address,
                                                      base_class);
          if (!already_registered)
            {
#ifdef VTV_DEBUG
                call_expr = build_call_expr (vlt_register_pairs_fndecl, 4,
                                             arg1, vtable_address,
                                             str1, str2);
#else
                call_expr = build_call_expr (vlt_register_pairs_fndecl, 2,
                                             arg1, vtable_address);
#endif
              append_to_statement_list (call_expr, &body);
            }
        }

      register_other_binfo_vtables (base_binfo, body, arg1, str1, str2,
                                    base_class);
    }
}

/* The set of valid vtable pointers for any given class are stored in
   a hash table.  For reasons of efficiency, that hash table size is
   always a power of two.  In order to try to prevent re-sizing the
   hash tables very often, we pass __VLTRegisterPair an initial guess
   as to the number of entries the hashtable will eventually need
   (rounded up to the nearest power of two).  This function takes the
   class information we have collected for a particular class,
   CLASS_NODE, and calculates the hash table size guess.  */

static int
guess_num_vtable_pointers (struct vtv_graph_node *class_node)
{
  tree vtbl;
  int total_num_vtbls = 0;
  int num_vtbls_power_of_two = 1;
  unsigned i;

  for (i = 0; i < num_vtable_map_nodes; ++i)
    if (TEST_BIT (class_node->descendants, i))
      {
        tree class_type = vtbl_map_nodes_array[i]->class_info->class_type;
        for (vtbl = CLASSTYPE_VTABLES (class_type); vtbl;
             vtbl = DECL_CHAIN (vtbl))
          {
            total_num_vtbls++;
            if (total_num_vtbls > num_vtbls_power_of_two)
              num_vtbls_power_of_two <<= 1;
          }
      }
  return num_vtbls_power_of_two;
}

/* This function goes through our internal class hierarchy & vtable
   pointer data structure and outputs calls to __VLTRegisterPair for
   every class-vptr pair (for those classes whose vtable would be
   output in the current compilation unit).  These calls get put into
   our constructor initialization function.  BODY is the function
   body, so far, of our constructor initialization function, to which we
   add the calls.  */

static bool
register_all_pairs (tree body)
{
  struct vtbl_map_node *current;
  bool registered_at_least_one = false;

  for (current = vtbl_map_nodes; current; current = current->next)
    {
      unsigned i;
      tree base_class = current->class_info->class_type;
      tree base_ptr_var_decl = current->vtbl_map_decl;
      tree str1 = NULL_TREE;

      gcc_assert (current->class_info != NULL);


#ifdef VTV_DEBUG
        str1 = build_string_literal
                        (IDENTIFIER_LENGTH (DECL_NAME (base_ptr_var_decl)) + 1,
                         IDENTIFIER_POINTER (DECL_NAME (base_ptr_var_decl)));
#endif

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

              /* Handle main vtable for this class.  */

              if (vtable_decl)
                vtable_should_be_output = TREE_ASM_WRITTEN (vtable_decl);

              if (vtable_decl && vtable_should_be_output
                  && BINFO_VTABLE (binfo))
                {
                  tree vtable_address = build_vtbl_address (binfo);

                  already_registered = record_register_pairs (vtable_decl,
                                                              vtable_address,
                                                              base_class);

                  if (!already_registered)
                    {
                      tree str2 = NULL_TREE;
                      new_type = build_pointer_type (TREE_TYPE
                                                     (base_ptr_var_decl));
                      arg1 = build1 (ADDR_EXPR, new_type, base_ptr_var_decl);

#ifdef VTV_DEBUG
                      str2 = build_string_literal (IDENTIFIER_LENGTH
                                                   (DECL_NAME (vtable_decl))
                                                                            + 1,
                                                   IDENTIFIER_POINTER
                                                    (DECL_NAME (vtable_decl)));

                        /* This call expr has the 2 "real" arguments,
                           plus 2 debugging arguments.   */
                      call_expr = build_call_expr (vlt_register_pairs_fndecl,
                                                   4, arg1, vtable_address,
                                                   str1, str2);
#else                      
                      call_expr = build_call_expr (vlt_register_pairs_fndecl,
                                                   2, arg1, vtable_address);
#endif

                      append_to_statement_list (call_expr, &body);

                      registered_at_least_one = true;

                      /* Find and handle any 'extra' vtables associated
                         with this class, via virtual inheritance.   */
                      register_vptr_fields (arg1, base_class, class_type,
                                            body);

                      /* Find and handle any 'extra' vtables associated
                         with this class, via multiple inheritance.   */
                      register_other_binfo_vtables (binfo, body, arg1, str1,
                                                    str2, base_class);
                    }
                }
            }
          }
    }

  return registered_at_least_one;
}

/* Given a tree containing a class type (CLASS_TYPE), this function
   finds and returns the class hierarchy node for that class in our
   data structure.  */

static struct vtv_graph_node *
find_graph_node (tree class_type)
{
  struct vtbl_map_node *vtbl_node;

  vtbl_node = vtbl_map_get_node (class_type);
  if (vtbl_node)
    return vtbl_node->class_info;

  return NULL;
}

/* This function adds an edge to our class hierarchy graph.
   EDGE_ARRAY will either be an array of parent nodes or an array of
   children nodes for a particular class.  NUM_ENTRIES is the current
   number of entries in the array.  MAX_WENTRIES is the maximum number
   of entries the array can hold.  NEW_ENTRY is a vtv_graph_node
   representing the new child or parent node to be added to the
   EDGE_ARRAY.  */

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

/* Add base class/derived class pair to our internal class hierarchy
   data structure.  BASE_NODE is our vtv_graph_node that corresponds
   to a base class.  DERIVED_NODE is our vtv_graph_node that
   corresponds to a class that is a descendant of the base class
   (possibly the base class itself).  */

static void
add_hierarchy_pair (struct vtv_graph_node *base_node,
                    struct vtv_graph_node *derived_node)
{
  add_edge_to_graph (&(base_node->children), &(base_node->num_children),
                     &(base_node->max_children), derived_node);
  add_edge_to_graph (&(derived_node->parents), &(derived_node->num_parents),
                     &(derived_node->max_parents), base_node);
}

/* This functions adds a new base class/derived class relationship to
   our class hierarchy data structure.  Both parameters are trees
   representing the class types, i.e. RECORD_TYPE trees.
   DERIVED_CLASS can be the same as BASE_CLASS.  */

static void
update_class_hierarchy_information (tree base_class,
                                    tree derived_class)
{
  struct vtv_graph_node *base_node = find_graph_node (base_class);
  struct vtv_graph_node *derived_node = find_graph_node (derived_class);

  add_hierarchy_pair (base_node, derived_node);
}

/* Generate an undefined variable (a reference) to a varible defined
   in the vtv_init libraty. In that way, if the a module is not linked
   with the vtv_init library, the linker will generate an undefined
   symbol error.  Which is much better that getting a segmentation
   violation at runtime.  The parameter, INIT_ROUTINE_BODY, is the
   function body of our constructor initialization function, to which
   we add the reference to this symbol (and all of our calls to
   __VLTRegisterPair).

   For more information, see comments in
   libstdc++-v3/libsupc++/vtv_init.cc.  */

static void
create_undef_reference_to_vtv_init (tree init_routine_body)
{
  const char *vtv_init_undef_var = "__vtv_defined_in_vtv_init_lib";
  tree var_decl;
  tree init_zero;

  var_decl  = build_decl (UNKNOWN_LOCATION, VAR_DECL,
                          get_identifier (vtv_init_undef_var),
                          int32_type_node);
  TREE_PUBLIC (var_decl) = 1;
  DECL_EXTERNAL (var_decl) = 1;
  TREE_STATIC (var_decl) = 1;
  SET_DECL_ASSEMBLER_NAME (var_decl, get_identifier (vtv_init_undef_var));
  DECL_ARTIFICIAL (var_decl) = 1;
  TREE_READONLY (var_decl) = 0;
  DECL_IGNORED_P (var_decl) = 1;
  DECL_PRESERVE_P (var_decl) = 1;
  DECL_VISIBILITY (var_decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (var_decl) = 1;
  varpool_finalize_decl (var_decl);

  /* Store a value in the undefined variable to force the creation of a
     a reference.  */
  init_zero = build2 (MODIFY_EXPR, TREE_TYPE (var_decl), var_decl,
                      integer_zero_node);
  append_to_statement_list (init_zero, &init_routine_body);

}

/* A simple hash function on strings */
/* Be careful about changing this routine. The values generated will
   be stored in the calls to InitSet. So, changing this routine may
   cause a binary incompatibility.  */

static uint32_t
vtv_string_hash(const char *in)
{
  const char *s = in;
  uint32_t h = 0;

  gcc_assert (in != NULL);
  for ( ; *s; ++s)
    h = 5 * h + *s;
  return h;
}

/* This function goes through all of our vtable map nodes, and for
   each one that is actually used, it generates a call to
   __VLTInitSetSymbol, with the appropriate arguments, and inserts the
   calls as the start of our constructor initialization function
   (INIT_ROUTINE_BODY).  */

static bool
init_all_sets (tree init_routine_body)
{
  struct vtbl_map_node *current;
  bool inited_at_least_one = false;
  tree_stmt_iterator i = tsi_start (init_routine_body);

  for (current = vtbl_map_nodes; current; current = current->next)
    {
      if (!(current->is_used || (htab_elements (current->registered) > 0)))
        continue;

      size_t size_hint = guess_num_vtable_pointers (current->class_info);
      tree set_handle_var_decl = current->vtbl_map_decl;

      tree void_ptr_type = build_pointer_type (TREE_TYPE (set_handle_var_decl));
      tree arg1 = build1 (ADDR_EXPR, void_ptr_type, set_handle_var_decl);

      uint32_t len1 = IDENTIFIER_LENGTH (DECL_NAME (set_handle_var_decl));
      uint32_t hash_value = vtv_string_hash (IDENTIFIER_POINTER
                                            (DECL_NAME (set_handle_var_decl)));
      tree arg2, arg3, init_set_call;

      /* Build a buffer with the memory representation of
         insert_only_hash_map::key_value as defined in vtv_map.h. This
         will be passed as the second argument to InitSet.  */
      #define KEY_TYPE_FIXED_SIZE 8

      void *key_buffer = xmalloc (len1 + KEY_TYPE_FIXED_SIZE);
      uint32_t *value_ptr = (uint32_t *) key_buffer;

      /* Set the len and hash for the string.  */
      *value_ptr = len1;
      value_ptr++;
      *value_ptr = hash_value;

      /* Now copy the string representation of the vtbl map name...  */
      memcpy ((char *) key_buffer + KEY_TYPE_FIXED_SIZE,
              IDENTIFIER_POINTER (DECL_NAME (set_handle_var_decl)),
              len1);

      /* ... and build a string literal from it. This will make a copy
         so the key_bufffer is not needed anymore after this.  */
      arg2 = build_string_literal (len1 + KEY_TYPE_FIXED_SIZE,
                                   (char *) key_buffer);
      free (key_buffer);

      /* size_t maybe different at compile time vs at runtime but
         there should not be a problem in here. We dont expect such
         large number of elements in the set.  */
      arg3 = build_int_cst (size_type_node, size_hint);
      init_set_call = build_call_expr (vlt_init_set_symbol_fndecl,
                                       3, arg1, arg2, arg3);
      gcc_assert (size_hint != 0);
      tsi_link_before (&i, init_set_call, TSI_SAME_STMT);

      inited_at_least_one = true;
    }
  return inited_at_least_one;
}


/* This function calls register_all_pairs, which actually generates
   all the calls to __VLTRegisterPair (in the verification constructor
   init function).  It also generates the calls to
   __VLTChangePermission, if the verification constructor init
   function is going into the preinit array.  INIT_ROUTINE_BODY is
   the body of our constructior initialization function, to which we
   add our function calls.*/

static bool
vtv_register_class_hierarchy_information (tree init_routine_body)
{
  bool registered_something = false;
  bool inited_some_sets = true;

  init_functions ();

  /* TODO: Temp fix. Needs to be tightened.  */
  if (num_vtable_map_nodes == 0)
    return false;;

  /* Add class hierarchy pairs to the vtable map data structure.  */
  registered_something = register_all_pairs (init_routine_body);

  /* Initialialize all vtable map variables (pointers to our data
     sets.  */
  inited_some_sets = init_all_sets (init_routine_body);

  if (registered_something || inited_some_sets)
  {
      /* If this function is going into the preinit_array, then we
         need to manually call __VLTChangePermission, rather than
         depending on initialization prioritys in vtv_init.  */
      if (flag_vtable_verify == VTV_PREINIT_PRIORITY)
        {
          /* Pass __VLTP_READ_WRITE value as defined in vtv_rts.h.  */
          tree arg_read_write = build_int_cst (integer_type_node, 1);
          tree arg_read_only = build_int_cst (integer_type_node, 0);

          tree call_rw_expr = build_call_expr (vlt_change_permission_fndecl,
                                               1, arg_read_write);
          tree call_r_expr = build_call_expr (vlt_change_permission_fndecl,
                                              1, arg_read_only);
          tree_stmt_iterator i = tsi_start (init_routine_body);
          /* Insert the call to make permissions read-write at the
             beginning of the init routine.  */
          tsi_link_before (&i, call_rw_expr, TSI_SAME_STMT);

          /* Append the call to make permissions read-only at the
             end of the init routine.  */
          append_to_statement_list (call_r_expr, &init_routine_body);
        }

      if (flag_vtable_verify == VTV_STANDARD_PRIORITY)
        create_undef_reference_to_vtv_init (init_routine_body);
  }

  return registered_something || inited_some_sets;
}

/* This function writes records data about the number of virtual calls
   we found and the number of verification calls we generated.  It is
   primarily for debugging purposes.  */

static void
write_out_counters (void)
{
  if (total_num_virtual_calls == 0)
    return;

  FILE *fp = fopen ("/tmp/vtable-verification-counters.log", "a");
  double pct_done = (total_num_verified_vcalls * 100) / total_num_virtual_calls;

  if (fp)
    {
      fprintf (fp, "%s %d %d (%.2f %%)\n", main_input_filename,
               total_num_virtual_calls, total_num_verified_vcalls,
               pct_done);
      fclose (fp);
    }
}

/* Generate the special constructor function that calls
   __VLTChangePermission and __VLTRegisterPairs, and give it a very
   high initialization priority.  */

void
vtv_generate_init_routine (void)
{
  tree init_routine_body;
  bool vtable_classes_found = false;
#ifdef VTV_COUNT
  bool debug_num_verified = true;
#else
  bool debug_num_verified = false;
#endif

  if (debug_num_verified)
    write_out_counters ();

  push_lang_context (lang_name_c);

  /* The priority for this init function (constructor) is carefully
     chosen so that it will happen after the calls to unprotect the
     memory used for vtable verification and before the memory is
     protected again.  */
  init_routine_body = vtv_start_verification_constructor_init_function ();

  vtable_classes_found =
                 vtv_register_class_hierarchy_information (init_routine_body);

  if (vtable_classes_found)
    {
      current_function_decl =
       vtv_finish_verification_constructor_init_function (init_routine_body);
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

      cgraph_process_new_functions ();
    }
  pop_lang_context ();
}

/* This funtion takes a tree containing a class type (BASE_TYPE), and
   it either finds the existing vtbl_map_node for that class in our
   data structure, or it creates a new node and adds it to the data
   structure if there is not one for the class already.  As part of
   this process it also creates the global vtable map variable for the
   class.  */

struct vtbl_map_node *
vtable_find_or_create_map_decl (tree base_type)
{
  tree base_decl = TREE_CHAIN (base_type);
  tree base_id;
  struct vtbl_map_node *vtable_map_node = NULL;
  tree base_decl_type;
  unsigned int save_quals;
  unsigned int null_quals = TYPE_UNQUALIFIED;

  /* Verify the type has an associated vtable.  */
  if (!TYPE_BINFO (base_type) || !BINFO_VTABLE (TYPE_BINFO (base_type)))
    return NULL;

  if (!base_decl)
    base_decl = TYPE_NAME (base_type);

  /* Temporarily remove any type qualifiers on the type. */
  base_decl_type = TREE_TYPE (base_decl);
  save_quals = TYPE_QUALS (base_decl_type);
  reset_type_qualifiers (null_quals, base_decl_type);

  base_id = DECL_ASSEMBLER_NAME (base_decl);

  /* Restore the type qualifiers. */
  reset_type_qualifiers (save_quals, base_decl_type);

  /* We've already created the variable; just look it.  */
  vtable_map_node = vtbl_map_get_node (base_type);

  if (!vtable_map_node || (vtable_map_node->vtbl_map_decl == NULL_TREE))
    {
      /* If we haven't already created the *__vtable_map global
         variable for this class, do so now, and add it to the
         varpool, to make sure it gets saved and written out.  */
      char *var_name = NULL;
      tree var_decl = NULL;
      tree var_type = build_pointer_type (void_type_node);
      tree initial_value = build_int_cst (make_node (INTEGER_TYPE), 0);

      /* Create map lookup symbol for base class */
      var_name = ACONCAT (("_ZN4_VTVI", IDENTIFIER_POINTER (base_id),
                           "E12__vtable_mapE", NULL));
      var_decl  = build_decl (UNKNOWN_LOCATION, VAR_DECL,
                              get_identifier (var_name), var_type);
      TREE_PUBLIC (var_decl) = 1;
      DECL_EXTERNAL (var_decl) = 0;
      TREE_STATIC (var_decl) = 1;
      DECL_VISIBILITY (var_decl) = VISIBILITY_HIDDEN;
      SET_DECL_ASSEMBLER_NAME (var_decl, get_identifier (var_name));
      DECL_ARTIFICIAL (var_decl) = 1;
      /* We cannot mark this variable as read-only otherwise the gold
         linker will not put it in the relro section. It seems if it
         is marked as read-only, gold will put it in the .text
         segment.  */
      TREE_READONLY (var_decl) = 0;
      DECL_IGNORED_P (var_decl) = 1;

      /* Put these mmap variables in to .vtable_map_vars sections, so
         we can find and protect them.  */

      DECL_SECTION_NAME (var_decl) = build_string (strlen (".vtable_map_vars"),
                                                   ".vtable_map_vars");
      DECL_HAS_IMPLICIT_SECTION_NAME_P (var_decl) = true;
      DECL_COMDAT_GROUP (var_decl) = get_identifier (var_name);
      DECL_INITIAL (var_decl) = initial_value;

      varpool_finalize_decl (var_decl);
      if (!vtable_map_node)
        vtable_map_node = find_or_create_vtbl_map_node (base_type);
      if (vtable_map_node->vtbl_map_decl == NULL_TREE)
        vtable_map_node->vtbl_map_decl = var_decl;
    }

  gcc_assert (vtable_map_node);
  return vtable_map_node;
}

/* This function is used to build up our class hierarchy data for a
   particular class.  TYPE is the record_type tree node for the
   class.  */

static void
vtv_save_base_class_info (tree type)
{
  if (flag_vtable_verify)
    {
      tree binfo =  TYPE_BINFO (type);
      tree base_binfo;
      struct vtbl_map_node *own_map;
      int i;

      /* First make sure to create the map for this record type.  */
      own_map = vtable_find_or_create_map_decl (type);
      if (own_map == NULL)
        return;

      /* Go through the list of all base classes for the current
         (derived) type, make sure the *__vtable_map global variable
         for the base class exists, and add the base class/derived
         class pair to the class hierarchy information we are
         accumulating (for vtable pointer verification).  */
      for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
        {
          tree tree_val = BINFO_TYPE (base_binfo);
          struct vtbl_map_node *vtable_map_node = NULL;

          vtable_map_node = vtable_find_or_create_map_decl (tree_val);

          if (vtable_map_node != NULL)
            update_class_hierarchy_information (tree_val, type);
        }
    }
}

/* This function adds classes we are interested in to a list of
   classes that is saved during pre-compiled header generation.
   RECORD is the record_type node for the class we are adding to the
   list.  */

void
vtv_save_class_info (tree record)
{
  if (!flag_vtable_verify || TREE_CODE (record) == UNION_TYPE)
    return;

  gcc_assert (TREE_CODE (record) == RECORD_TYPE);

  vlt_saved_class_info = tree_cons (NULL_TREE, record, vlt_saved_class_info);
}


/* This function goes through the list of classes we saved before the
   pre-compiled header generation and calls vtv_save_base_class_info
   on each one, to build up our class hierarchy data structure.  */

void
vtv_recover_class_info (void)
{
  tree current_class;
  tree class_chain = vlt_saved_class_info;
  while (class_chain != NULL_TREE)
    {
      current_class = TREE_VALUE (class_chain);
      gcc_assert (TREE_CODE (current_class) == RECORD_TYPE);

      vtv_save_base_class_info (current_class);
      class_chain = TREE_CHAIN (class_chain);
    }

  /* Let the garbabe collector collect the memory associated with the
     chain.  */
  vlt_saved_class_info = NULL_TREE;
}

#include "gt-cp-vtable-class-hierarchy.h"
