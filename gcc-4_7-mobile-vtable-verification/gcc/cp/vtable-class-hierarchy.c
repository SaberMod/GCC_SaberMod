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
  data sets, we call two of the functions we add to the runtime
  library, __VLTRegisterPair or __VLTRegisterSet.  __VLTRegisterPair
  takes four arguments, a vtable map variable, a set key and size hint
  (for initializing the data set, if not already initialized) and the
  vtable pointer to add to the set.  __VLTRegisterSet takes five
  arguments: the vtable map var, set key and size hint, and also an
  array of vtable pointers to add to the set and the size of the
  array.  __VLTRegisterPair is used to add a single vtable pointer to
  the set, while __VLTRegisterSet is used to add multiple pointers at
  a time.  If the vtable map variable is currently NULL when passed to
  either of these routines, they create a new data set (hash table),
  makes the vtable map variable point to the new data set, and inserts
  the vtable address into the data set.  If the vtable map variable is
  not NULL, they just inserts the vtable addresses into the data set.
  In order to make sure that our data sets are built before any
  verification calls happen, we create a special constructor
  initialization function for each compilation unit, give it a very
  high initialization priority, and insert all of our calls to
  __VLTRegisterSet and __VLTRegisterPair into our special constructor
  initialization function.

  The vtable verification feature is controlled by the flag
  '-fvtable-verify='.  There are three flavors of this:
  '-fvtable-verify=std', '-fvtable-verify=preinit', and
  '-fvtable-verify=none'.  If the option '-fvtable-verfy=preinit' is
  used, then our constructor initialization function gets put into the
  preinit array.  This is necessary if there are data sets that need
  to be built very early in execution.  If the '-fvtable-verify=std'
  option is used, the constructor initialization functions are
  executed at their normal time.  The option '-fvtable-verify=none'
  turns off vtable verification.

  This file contains code to find and record the class hierarchies for
  the virtual classes in a program, and all the vtables associated
  with each such class; to generate the vtable map variables; and to
  generate the constructor initialization function (with the calls to
  __VLTRegisterSet, and/or __VLTRegisterPair).  The main data
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

#define MAX_SET_SIZE 5000

static int num_calls_to_regset = 0;
static int num_calls_to_regpair = 0;
static int sub_vtt_count = 0;
static int current_set_size;

/* Mark these specially since they need to be stored in precompiled
   header IR.  */
static GTY (()) tree vlt_saved_class_info = NULL_TREE;
static GTY (()) tree vlt_register_set_fndecl = NULL_TREE;
static GTY (()) tree vlt_register_pairs_fndecl = NULL_TREE;

struct work_node {
  struct vtv_graph_node *node;
  struct work_node *next;
};

struct vtbl_map_node *vtable_find_or_create_map_decl (tree);

/* As part of vtable verification the compiler generates and inserts
   calls to __VLTVerifyVtablePointer, which is in libstdc++.  This
   function builds and initializes the function decl that is used
   in generating those function calls.

   In addition to __VLTVerifyVtablePointer there is also
   __VLTVerifyVtablePointerDebug which can be used in place of
   __VLTVerifyVtablePointer, and which takes extra parameters and
   outputs extra information, to help debug problems.  The debug
   version of this function is generated and used if VTV_DEBUG is
   defined.

   The signatures for these functions are:

   void * __VLTVerifyVtablePointer (void **, void*);
   void * __VLTVerifyVtablePointerDebug (void**, void *, char *, int, char *,
                                         int);
*/

void
vtv_build_vtable_verify_fndecl (void)
{
  tree void_ptr_type = build_pointer_type (void_type_node);
  tree arg_types = NULL_TREE;
  tree func_type = NULL_TREE;
  struct lang_decl *ld;
#ifdef VTV_DEBUG
  tree const_char_ptr_type = build_pointer_type
                                  (build_qualified_type (char_type_node,
                                                         TYPE_QUAL_CONST));
#endif

  if (verify_vtbl_ptr_fndecl != NULL_TREE)
    return;

  ld = ggc_alloc_cleared_lang_decl (sizeof (struct lang_decl_fn));
  ld->u.base.selector = 1;

  arg_types = build_tree_list (NULL_TREE, build_pointer_type (void_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   const_ptr_type_node));

#ifdef VTV_DEBUG
      /* Arg types for the debugging version of function.  */
      arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                       const_char_ptr_type));
      arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                       const_char_ptr_type));
#endif

  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_type_node));
  func_type = build_function_type (const_ptr_type_node, arg_types);

#ifdef VTV_DEBUG
  /* const void *
     __VLTVerifyVtablePointerDebug (void ** set_handle_ptr,
                                    const void * vtable_ptr,
                                    const char * set_symbol_name,
                                    const char * vtable_name)      */

    verify_vtbl_ptr_fndecl = build_fn_decl ("__VLTVerifyVtablePointerDebug",
                                            func_type);
#else
  /* const void *
     __VLTVerifyVtablePointerDebug (void ** set_handle_ptr,
                                    const void * vtable_ptr)            */

    verify_vtbl_ptr_fndecl = build_fn_decl ("__VLTVerifyVtablePointer",
                                            func_type);
#endif

  TREE_NOTHROW (verify_vtbl_ptr_fndecl) = 1;
  DECL_ATTRIBUTES (verify_vtbl_ptr_fndecl)
      = tree_cons (get_identifier ("leaf"), NULL,
                   DECL_ATTRIBUTES (verify_vtbl_ptr_fndecl));
  DECL_PURE_P (verify_vtbl_ptr_fndecl) = 1;
  TREE_PUBLIC (verify_vtbl_ptr_fndecl) = 1;
#ifdef VTV_STATIC_VERIFY
  DECL_VISIBILITY (verify_vtbl_ptr_fndecl) = 1;
#endif
  DECL_PRESERVE_P (verify_vtbl_ptr_fndecl) = 1;
  DECL_LANG_SPECIFIC (verify_vtbl_ptr_fndecl) = ld;
  SET_DECL_LANGUAGE (verify_vtbl_ptr_fndecl, lang_cplusplus);
}

/* As part of vtable verification the compiler generates and inserts
   calls to __VLTRegisterSet and __VLTRegisterPair, which are in
   libsupc++.  This function builds and initializes the function decls
   that are used in generating those function calls.

   The signatures for these functions are:

   void __VLTRegisterSetDebug (void **, const void *, std::size_t,
                               size_t, void **);

   void __VLTRegisterSet (void **, const void *, std::size_t,
                          size_t, void **);

   void __VLTRegisterPairDebug (void **, const void *, size_t,
                                const void *, const char *, const char *);

   void __VLTRegisterPair (void **, const void *, size_t, const void *);
*/

static void
init_functions (void)
{
  tree void_ptr_type = build_pointer_type (void_type_node);
  tree arg_types = NULL_TREE;
  tree register_set_type = void_type_node;
  tree register_pairs_type = void_type_node;
#ifdef VTV_DEBUG
  tree const_char_ptr_type = build_pointer_type
                                           (build_qualified_type
                                             (char_type_node, TYPE_QUAL_CONST));
#endif

  if (vlt_register_set_fndecl != NULL_TREE)
    return;

  gcc_assert (vlt_register_set_fndecl == NULL_TREE);
  gcc_assert (vlt_register_pairs_fndecl == NULL_TREE);

  /* Build arg types for __VLTRegisterSet[Debug], and initialize
     vlt_register_set_fndecl.  */

  /* Arg1: Set handle ptr.  */
  arg_types = build_tree_list (NULL_TREE, build_pointer_type (void_ptr_type));
  /* Arg2: Set symbol key. */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   const_ptr_type_node));
  /* Arg3: Size hint for the data set. */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   size_type_node));
  /* Arg4: Number of elements in the data array.  */
  arg_types = chainon (arg_types,
                       build_tree_list (NULL_TREE, size_type_node));
  /* Arg5: Array of vtable pointers. */
  arg_types = chainon (arg_types,
                       build_tree_list (NULL_TREE,
                                        build_pointer_type (void_ptr_type)));

  /* Add the final void_type_node, to mark the end of the parameters.  */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_type_node));

  register_set_type = build_function_type (register_set_type, arg_types);

#if VTV_DEBUG
  vlt_register_set_fndecl = build_fn_decl ("__VLTRegisterSetDebug",
                                           register_set_type);
#else
  vlt_register_set_fndecl = build_fn_decl ("__VLTRegisterSet",
                                           register_set_type);
#endif

  TREE_NOTHROW (vlt_register_set_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_register_set_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES
                                             (vlt_register_set_fndecl));
  TREE_PUBLIC (vlt_register_set_fndecl) = 1;
  DECL_PRESERVE_P (vlt_register_set_fndecl) = 1;
  retrofit_lang_decl (vlt_register_set_fndecl);
  SET_DECL_LANGUAGE (vlt_register_set_fndecl, lang_cplusplus);

  /* Build arg types for __VLTRegisterPair[Debug], and initialize
     vlt_register_set_fndecl.  */

  /* Arg1: Set handle ptr.  */
  arg_types = build_tree_list (NULL_TREE, build_pointer_type (void_ptr_type));

  /* Arg2: Set symbol key. */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   const_ptr_type_node));
  /* Arg3: Size hint for the data set. */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE,
                                                   size_type_node));
  /* Arg4: Vtable pointer.  */
  arg_types = chainon (arg_types,
                       build_tree_list (NULL_TREE, const_ptr_type_node));

#ifdef VTV_DEBUG
  /* Arg5: Name of the vtable map set.  */
  arg_types = chainon (arg_types,
                       build_tree_list (NULL_TREE, const_char_ptr_type));
  /* Arg6: Name of the vtable.  */
  arg_types = chainon (arg_types,
                       build_tree_list (NULL_TREE, const_char_ptr_type));
#endif

  /* Add the final void_type_node, to mark the end of the parameters.  */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_type_node));

  register_pairs_type = build_function_type (register_pairs_type,
                                             arg_types);
#if VTV_DEBUG
  vlt_register_pairs_fndecl = build_fn_decl ("__VLTRegisterPairDebug",
                                             register_pairs_type);
#else
  vlt_register_pairs_fndecl = build_fn_decl ("__VLTRegisterPair",
                                             register_pairs_type);
#endif

  TREE_NOTHROW (vlt_register_pairs_fndecl) = 1;
  DECL_ATTRIBUTES (vlt_register_pairs_fndecl) =
                    tree_cons (get_identifier ("leaf"), NULL,
                               DECL_ATTRIBUTES
                                             (vlt_register_pairs_fndecl));
  TREE_PUBLIC (vlt_register_pairs_fndecl) = 1;
  DECL_PRESERVE_P (vlt_register_pairs_fndecl) = 1;
  retrofit_lang_decl (vlt_register_pairs_fndecl);
  SET_DECL_LANGUAGE (vlt_register_pairs_fndecl, lang_cplusplus);
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

/* Keep track of which pairs we have already added to our list for
   __VLTRegisterSet or __VLTRegisterPair, to prevent creating
   duplicate entries within the same compilation unit.  VTABLE_DECL is
   the var decl for the vtable of the (descendant) class that we are
   adding to our class hierarchy data.  VPTR_ADDRESS is and expression
   for calculating the correct offset into the vtable (VTABLE_DECL).
   It is the actual vtable pointer address that will be stored in our
   list of valid vtable pointers for BASE_CLASS.  BASE_CLASS is the
   record_type node for the base class to whose hiearchy we want to
   add VPTR_ADDRESS. (VTABLE_DECL should be the vtable for BASE_CLASS
   or one of BASE_CLASS' descendents.  */

static bool
check_and_record_registered_pairs (tree vtable_decl, tree vptr_address,
                                   tree base_class)
{
  unsigned offset;
  struct vtbl_map_node *base_vtable_map_node;
  bool inserted_something = false;

  if (TREE_OPERAND_LENGTH (vptr_address) == 1)
    {
      tree tmp_address = TREE_OPERAND (vptr_address, 0);
      offset = TREE_INT_CST_LOW (TREE_OPERAND (tmp_address, 1));
    }
  else
    offset = TREE_INT_CST_LOW (TREE_OPERAND (vptr_address, 1));

  base_vtable_map_node = vtbl_map_get_node (base_class);

  inserted_something = vtbl_map_node_registration_insert (base_vtable_map_node,
                                                          vtable_decl,
                                                          offset);
  return !inserted_something;
}

/* A class may contain secondary vtables in it, for various reasons.
   This function goes through the decl chain of a class record looking
   for any fields that point to secondary vtables, and adding vtable
   addresses for secondary vtable pointers to set to be passed to
   __VLTRegisterSet.

   BASE_CLASS is the record_type node for the base class.
   RECORD_TYPE is the record_type node for the descendant class that
   we are possibly adding to BASE_CLASS's hierarchy.  VTABLE_PTR_ARRAY
   an array containing the vtable pointer addressees collected so far.
   NUM_ARGS is the number of elements in the VTABLE_PTR_ARRAY.  */

static void
register_construction_vtables (tree base_class, tree record_type,
                               tree *vtable_ptr_array, int *num_args)
{
  tree vtbl_var_decl;
  tree binfo;

  if (TREE_CODE (record_type) != RECORD_TYPE)
    return;

  binfo = TYPE_BINFO (record_type);
  vtbl_var_decl = CLASSTYPE_VTABLES (record_type);

  if (CLASSTYPE_VBASECLASSES (record_type))
    {
      tree vtt_decl;
      tree sub_vtt_addr = NULL_TREE;
      bool already_registered = false;
      tree val_vtbl_decl = NULL_TREE;
#ifdef VTV_DEBUG
      tree arg1 = NULL_TREE;
#endif

      vtt_decl = DECL_CHAIN (vtbl_var_decl);

      /* Check to see if we have found a constructor vtable.  Add its
         data if appropriate.  */
      if (vtt_decl)
        {
          tree values = DECL_INITIAL (vtt_decl);
          struct varpool_node *vp_node = varpool_node (vtt_decl);
          if (vp_node->finalized
              && TREE_ASM_WRITTEN (vtt_decl)
              && values != NULL_TREE
              && TREE_CODE (values) == CONSTRUCTOR
              && TREE_CODE (TREE_TYPE (values)) == ARRAY_TYPE)
            {
              unsigned HOST_WIDE_INT cnt;
              constructor_elt *ce;

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

                  /* We need to check value and find the bit where we have
                     something with 2 arguments, the first argument of which
                     is an ADDR_EXPR and the second argument of which is
                     an INTEGER_CST.  */

                  while (value && TREE_OPERAND_LENGTH (value) == 1
                         && TREE_CODE (TREE_OPERAND (value, 0)) == ADDR_EXPR)
                    value = TREE_OPERAND (value, 0);

                  /* The VAR_DECL for the vtable should be the first
                     argument of the ADDR_EXPR, which is the first
                     argument of value.*/

                  if (TREE_OPERAND (value, 0))
                    val_vtbl_decl = TREE_OPERAND (value, 0);

                  while (TREE_CODE (val_vtbl_decl) != VAR_DECL
                         && TREE_OPERAND (val_vtbl_decl, 0))
                    val_vtbl_decl = TREE_OPERAND (val_vtbl_decl, 0);

                  gcc_assert (TREE_CODE (val_vtbl_decl) == VAR_DECL);

                  /* Check to see if we already have this vtable pointer in
                     our valid set for this base class.  */
                  already_registered = check_and_record_registered_pairs
                                                             (val_vtbl_decl,
                                                              value,
                                                              base_class);

                  if (already_registered)
                    continue;

                  gcc_assert (*num_args < (MAX_SET_SIZE - 1));

                  vtable_ptr_array[*num_args] = value;
                  *num_args = *num_args + 1;
                  current_set_size++;
                }
            }

          if (BINFO_SUBVTT_INDEX (binfo))
            sub_vtt_addr = fold_build_pointer_plus (vtt_decl,
                                                    BINFO_SUBVTT_INDEX
                                                                      (binfo));

          if (sub_vtt_addr)
            {
              already_registered = check_and_record_registered_pairs
                                                                  (vtt_decl,
                                                                   sub_vtt_addr,
                                                                   record_type);

              if (!already_registered)
                {
                  vtable_ptr_array[*num_args] = sub_vtt_addr;
                  *num_args = *num_args + 1;
                  current_set_size++;
                  sub_vtt_count++;
                }
            }
        }
    }
}

/* This function iterates through all the vtables it can find from the
   BINFO of a class, to make sure we have found ALL of the vtables
   that an object of that class could point to.  Add entries for those
   vtable pointers that we find to the current array of vtable pointer
   addresses.

   BINFO is the tree_binfo node for the BASE_CLASS. BASE_CLASS is the
   record_type node for the base class whose set of valid vtable
   pointers we are updating. VTABLE_PTR_ARRAY is the current array of
   vtable pointer addresses we have found for BASE_CLASS so far.
   NUM_ARGS is the current number of entries in VTABLE_PTR_ARRAY.  */

static void
collect_other_binfo_vtables (tree binfo, tree base_class,
                             tree *vtable_ptr_array,
                             int *num_args)
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

          already_registered = check_and_record_registered_pairs
                                                                (vtable_decl,
                                                                 vtable_address,
                                                                 base_class);
          if (!already_registered)
            {
              gcc_assert (*num_args < (MAX_SET_SIZE - 1));

              vtable_ptr_array[*num_args] = vtable_address;
              *num_args = *num_args + 1;
              current_set_size++;
            }
        }

      collect_other_binfo_vtables (base_binfo,
                                   base_class, vtable_ptr_array,
                                   num_args);
    }
}

/* The set of valid vtable pointers for any given class are stored in
   a hash table.  For reasons of efficiency, that hash table size is
   always a power of two.  In order to try to prevent re-sizing the
   hash tables very often, we pass __VLTRegisterSet or
   __VLTRegisterPair an initial guess as to the number of entries the
   hashtable will eventually need (rounded up to the nearest power of
   two).  This function takes the class information we have collected
   for a particular class, CLASS_NODE, and calculates the hash table
   size guess.  */

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

/* A simple hash function on strings */
/* Be careful about changing this routine. The values generated will
   be stored in the calls to InitSet. So, changing this routine may
   cause a binary incompatibility.  */

static uint32_t
vtv_string_hash (const char *in)
{
  const char *s = in;
  uint32_t h = 0;

  gcc_assert (in != NULL);
  for ( ; *s; ++s)
    h = 5 * h + *s;
  return h;
}

static void
write_out_current_set_data (tree base_class, int set_size)
{
  static int class_data_log_fd = -1;
  char buffer[1024];
  int bytes_written;


  if (class_data_log_fd == -1)
    class_data_log_fd = open ("/tmp/vtv_class_set_sizes.log",
                              O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);

  if (class_data_log_fd == -1)
    return;

  snprintf (buffer, sizeof (buffer), "%s %d\n",
            IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TYPE_NAME (base_class))),
            set_size);
  bytes_written = write (class_data_log_fd, buffer, strlen (buffer));
}

static tree
build_key_buffer_arg (tree base_ptr_var_decl)
{
#define KEY_TYPE_FIXED_SIZE 8
  uint32_t len1 = IDENTIFIER_LENGTH (DECL_NAME (base_ptr_var_decl));
  uint32_t hash_value = vtv_string_hash (IDENTIFIER_POINTER
                                              (DECL_NAME (base_ptr_var_decl)));
  void *key_buffer = xmalloc (len1 + KEY_TYPE_FIXED_SIZE);
  uint32_t *value_ptr = (uint32_t *) key_buffer;
  tree ret_value;

  /* Set the len and hash for the string.  */
  *value_ptr = len1;
  value_ptr++;
  *value_ptr = hash_value;

  /* Now copy the string representation of the vtbl map name...  */
  memcpy ((char *) key_buffer + KEY_TYPE_FIXED_SIZE,
          IDENTIFIER_POINTER (DECL_NAME (base_ptr_var_decl)),
          len1);

  /* ... and build a string literal from it. This will make a copy
     so the key_bufffer is not needed anymore after this.  */
  ret_value = build_string_literal (len1 + KEY_TYPE_FIXED_SIZE,
                                    (char *) key_buffer);
  free (key_buffer);
  return ret_value;
}

static void
insert_call_to_register_set (tree class_name, int num_args,
                             tree *vtbl_ptr_array, tree body, tree arg1,
                             tree arg2, tree size_hint_arg)
{
  tree call_expr;
  char *array_arg_name = ACONCAT (("__vptr_array_",
                                   IDENTIFIER_POINTER (class_name), NULL));
  tree array_arg_type = build_array_type_nelts (build_pointer_type
                                                  (build_pointer_type
                                                     (void_type_node)),
                                                num_args);
  tree array_arg = build_decl (UNKNOWN_LOCATION, VAR_DECL,
                               get_identifier (array_arg_name),
                               array_arg_type);
  int k;

  VEC(constructor_elt,gc) *array_elements = VEC_alloc  (constructor_elt, gc,
                                                        num_args);
  constructor_elt *celt;
  tree initial = NULL_TREE;
  tree arg3 = NULL_TREE;

  TREE_PUBLIC (array_arg) = 0;
  DECL_EXTERNAL (array_arg) = 0;
  TREE_STATIC (array_arg) = 1;
  DECL_ARTIFICIAL (array_arg) = 0;
  TREE_READONLY (array_arg) = 1;
  DECL_IGNORED_P (array_arg) = 0;
  DECL_PRESERVE_P (array_arg) = 0;
  DECL_VISIBILITY (array_arg) = VISIBILITY_HIDDEN;

  for (k = 0; k < num_args; ++k)
    {
      celt = VEC_safe_push (constructor_elt, gc, array_elements, NULL);
      celt->index = build_int_cst (integer_type_node, k);
      celt->value = vtbl_ptr_array[k];
    }

  initial = build_constructor (TREE_TYPE (array_arg), array_elements);

  TREE_CONSTANT (initial) = 1;
  TREE_STATIC (initial) = 1;
  DECL_INITIAL (array_arg) = initial;
  relayout_decl (array_arg);
  varpool_finalize_decl (array_arg);

  arg3 = build1 (ADDR_EXPR, TYPE_POINTER_TO (TREE_TYPE (array_arg)), array_arg);

  TREE_TYPE (arg3) = build_pointer_type (TREE_TYPE (array_arg));

  call_expr = build_call_expr (vlt_register_set_fndecl, 5, arg1,
                               arg2, /* set_symbol_key */
                               size_hint_arg, build_int_cst (size_type_node,
                                                             num_args),
                               arg3);
  append_to_statement_list (call_expr, &body);
  num_calls_to_regset++;
}

#ifdef VTV_DEBUG

static void
insert_call_to_register_pair (tree vtable_address, int num_args, tree arg1,
                              tree arg2, tree size_hint_arg, tree str1,
                              tree str2, tree body)
{
  tree call_expr;

  if (num_args == 0)
    vtable_address = build_int_cst (build_pointer_type (void_type_node), 0);

  call_expr = build_call_expr (vlt_register_pairs_fndecl, 6, arg1, arg2,
                               size_hint_arg, vtable_address, str1, str2);
  append_to_statement_list (call_expr, &body);
  num_calls_to_regpair++;
}

#else

static void
insert_call_to_register_pair (tree vtable_address, int num_args, tree arg1,
                              tree arg2, tree size_hint_arg, tree body)
{
  tree call_expr;

  if (num_args == 0)
    vtable_address = build_int_cst (build_pointer_type (void_type_node), 0);

  call_expr = build_call_expr (vlt_register_pairs_fndecl, 4, arg1, arg2,
                               size_hint_arg, vtable_address);
  append_to_statement_list (call_expr, &body);
  num_calls_to_regpair++;
}

#endif

/* This function goes through our internal class hierarchy & vtable
   pointer data structure and outputs calls to __VLTRegisterSet for
   every class.  These calls get put into our constructor
   initialization function.  BODY is the function body, so far, of our
   constructor initialization function, to which we add the calls.  */

static bool
register_all_pairs (tree body)
{
  struct vtbl_map_node *current;
  bool registered_at_least_one = false;
  tree vtbl_ptr_array[MAX_SET_SIZE];
  int num_vtable_args = 0;

  for (current = vtbl_map_nodes; current; current = current->next)
    {
      unsigned i = 0;
      tree new_type;
      tree arg1;
      tree base_class = current->class_info->class_type;
      tree base_ptr_var_decl = current->vtbl_map_decl;
      sbitmap_iterator sbi;

      tree arg2;
      size_t size_hint;
      tree size_hint_arg;
#ifdef VTV_DEBUG
      tree str1 = NULL_TREE;
      tree str2 = NULL_TREE;
#endif

      current_set_size = 0;
      gcc_assert (current->class_info != NULL);

#ifdef VTV_DEBUG
        str1 = build_string_literal
                         (IDENTIFIER_LENGTH (DECL_NAME (base_ptr_var_decl)) + 1,
                          IDENTIFIER_POINTER (DECL_NAME (base_ptr_var_decl)));
#endif

      new_type = build_pointer_type (TREE_TYPE (base_ptr_var_decl));
      arg1 = build1 (ADDR_EXPR, new_type, base_ptr_var_decl);

      num_vtable_args = 0;

      /* for (i = 0; i < num_vtable_map_nodes; ++i) */
      /* if (TEST_BIT (current->class_info->descendants, i)) */
      EXECUTE_IF_SET_IN_SBITMAP (current->class_info->descendants, 0, i, sbi)
          {
            struct vtbl_map_node *vtbl_class_node = vtbl_map_nodes_array[i];
            tree class_type = vtbl_class_node->class_info->class_type;

            if (class_type
                && (TREE_CODE (class_type) == RECORD_TYPE))
            {
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

                  already_registered = check_and_record_registered_pairs
                                                                (vtable_decl,
                                                                 vtable_address,
                                                                 base_class);

                  if (!already_registered)
                    {
#ifdef VTV_DEBUG
                      str2 = build_string_literal (IDENTIFIER_LENGTH
                                                   (DECL_NAME (vtable_decl))
                                                                            + 1,
                                                   IDENTIFIER_POINTER
                                                    (DECL_NAME (vtable_decl)));
#endif
                      vtbl_ptr_array[num_vtable_args++] = vtable_address;

                      /* Find and handle any 'extra' vtables associated
                         with this class, via virtual inheritance.   */
                      register_construction_vtables (base_class, class_type,
                                                     vtbl_ptr_array,
                                                     &num_vtable_args);

                      /* Find and handle any 'extra' vtables associated
                         with this class, via multiple inheritance.   */
                      collect_other_binfo_vtables (binfo, base_class,
                                                   vtbl_ptr_array,
                                                   &num_vtable_args);
                    }
                }
            }
          }

      current_set_size = num_vtable_args;

      /* Sometimes we need to initialize the set symbol even if we are
         not adding any vtable pointers to the set in the current
         compilation unit.  In that case, we need to initialize the
         set to our best guess as to what the eventual size of the set
         hash table will be (to prevent having to re-size the hash
         table later).  */

      size_hint = guess_num_vtable_pointers (current->class_info);
      /* If we have added vtable pointers to the set in this
         compilation unit, adjust the size hint for the set's hash
         table appropriately.  */
      if (num_vtable_args > 0)
        while ((size_t) num_vtable_args > size_hint)
          size_hint <<= 1;
      size_hint_arg = build_int_cst (size_type_node, size_hint);

      /* Get the key-buffer argument.  */
      arg2 = build_key_buffer_arg (base_ptr_var_decl);

      if (num_vtable_args > 1)
        {
          insert_call_to_register_set (current->class_name, num_vtable_args,
                                       vtbl_ptr_array, body, arg1, arg2,
                                       size_hint_arg);
          registered_at_least_one = true;
        }
      else if (num_vtable_args >= 0)
        {

          if (num_vtable_args > 0
              || (current->is_used
                  || (htab_elements (current->registered) > 0)))
            {
#ifdef VTV_DEBUG
	      if (str2 == NULL_TREE)
		str2 = build_string_literal (strlen ("unknown") + 1,
					     "unknown");
              insert_call_to_register_pair (vtbl_ptr_array[0], num_vtable_args,
                                            arg1, arg2, size_hint_arg, str1,
                                            str2, body);
#else
              insert_call_to_register_pair (vtbl_ptr_array[0], num_vtable_args,
                                            arg1, arg2, size_hint_arg, body);

#endif
              registered_at_least_one = true;
            }
        }
      if (flag_vtv_counts && current_set_size > 0)
        write_out_current_set_data (base_class, current_set_size);
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
   __VLTRegisterSet or __VLTRegisterPair).

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

static void
write_out_vtv_count_data (void)
{
  static int vtv_count_log_fd = -1;
  char buffer[1024];
  struct vtbl_map_node *current;
  int unused_vtbl_map_vars = 0;
  int bytes_written;

  if (vtv_count_log_fd == -1)
    vtv_count_log_fd = open ("/tmp/vtv_count_data.log",
                             O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);
  if (vtv_count_log_fd == -1)
    return;

  for (current = vtbl_map_nodes; current; current = current->next)
    {
      if (!current->is_used
          && htab_elements (current->registered) == 0)
        unused_vtbl_map_vars++;
    }

  snprintf (buffer, sizeof (buffer), "%s %d %d %d %d %d\n",
            main_input_filename, total_num_virtual_calls,
            total_num_verified_vcalls, num_calls_to_regset,
            num_calls_to_regpair, unused_vtbl_map_vars);

  bytes_written = write (vtv_count_log_fd, buffer, strlen (buffer));
}

/* This function calls register_all_pairs, which actually generates
   all the calls to __VLTRegisterSet (in the verification constructor
   init function).  INIT_ROUTINE_BODY is the body of our constructior
   initialization function, to which we add our function calls.*/

static bool
vtv_register_class_hierarchy_information (tree init_routine_body)
{
  bool registered_something = false;

  init_functions ();

  /* TODO: Temp fix. Needs to be tightened.  */
  if (num_vtable_map_nodes == 0)
    return false;

  /* Add class hierarchy pairs to the vtable map data structure.  */
  registered_something = register_all_pairs (init_routine_body);

  if (registered_something)
    {
      if (flag_vtable_verify == VTV_STANDARD_PRIORITY)
        create_undef_reference_to_vtv_init (init_routine_body);
    }

  if (flag_vtv_counts)
    {
      write_out_vtv_count_data ();
    }

  return registered_something;
}

/* Generate the special constructor function that calls
   __VLTRegisterPair and __VLTRegisterSet, and give it a very
   high initialization priority.  */

void
vtv_generate_init_routine (void)
{
  tree init_routine_body;
  bool vtable_classes_found = false;

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
  struct vtbl_map_node *vtable_map_node = NULL;

  /* Verify the type has an associated vtable.  */
  if (!TYPE_BINFO (base_type) || !BINFO_VTABLE (TYPE_BINFO (base_type)))
    return NULL;

  /* If we've already created the variable, just look for it.  */
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
      var_name = get_mangled_vtable_map_var_name (base_type);
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
