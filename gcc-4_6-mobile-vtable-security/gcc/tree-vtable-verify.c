/* Virtual Table Pointer Verification
   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Virtual Table Pointer Security Pass.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp/cp-tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "output.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "cfgloop.h"
#include "flags.h"
#include "tree-inline.h"
#include "tree-scalar-evolution.h"
#include "diagnostic-core.h"
#include "gimple-pretty-print.h"
#include "toplev.h"
#include "langhooks.h"

#include "tree-vtable-verify.h"

unsigned num_vtable_map_nodes = 0;
bool any_verification_calls_generated = false;

static GTY(()) tree verify_vtbl_ptr_fndecl = NULL_TREE;

unsigned int vtable_verify_main (void);
static bool gate_tree_vtable_verify (void);
static void build_vtable_verify_fndecl (void);
static tree my_build1 (enum tree_code, tree, tree);

bool
vtbl_map_node_registration_find (struct vtbl_map_node *node,
                                 tree vtable_decl,
                                 unsigned offset)
{
  struct vtable_registration key;
  struct vtable_registration **slot;

  gcc_assert (node);
  gcc_assert (node->registered);

  key.vtable_decl = vtable_decl;
  slot = (struct vtable_registration **) htab_find_slot (node->registered,
                                                         &key, NO_INSERT);

  if (slot && (*slot) && (*slot)->offsets)
    {
      unsigned i;
      for (i = 0; i < (*slot)->cur_offset; ++i)
        if ((*slot)->offsets[i] == offset)
          return true;
    }

  return false;
}

void
vtbl_map_node_registration_insert (struct vtbl_map_node *node,
                                   tree vtable_decl,
                                   unsigned offset)
{
  struct vtable_registration key;
  struct vtable_registration **slot;

  if (!node || !(node->registered))
    return;

  key.vtable_decl = vtable_decl;
  slot = (struct vtable_registration **) htab_find_slot (node->registered,
                                                         &key, INSERT);

  if (!(*slot))
    {
      unsigned i;
      struct vtable_registration *node;
      node = (struct vtable_registration *)
                                 xmalloc (sizeof (struct vtable_registration));
      node->vtable_decl = vtable_decl;
      node->offsets = (unsigned *) xmalloc (10 * sizeof (unsigned));
      for (i= 0; i < 10; ++i)
        node->offsets[i] = 0;
      node->offsets[0] = offset;
      node->cur_offset = 1;
      node->max_offsets = 10;
      *slot = node;
    }
  else
    {
      /* We found the vtable_decl slot; we need to see if it already
         contains the offset.  If not, we need to add the offset.  */
      unsigned i;
      bool found = false;
      for (i = 0; (i < (*slot)->cur_offset) && !found; ++i)
        if ((*slot)->offsets[i] == offset)
          found = true;

      if (!found)
        {
          if ((*slot)->cur_offset == (*slot)->max_offsets)
            {
              unsigned new_max = 2 * (*slot)->max_offsets;
              (*slot)->offsets = (unsigned *)
                  xrealloc ((*slot)->offsets, new_max * sizeof (unsigned));

              for (i = (*slot)->max_offsets; i < new_max; ++i)
                (*slot)->offsets[i] = 0;
              (*slot)->max_offsets = new_max;
            }
          (*slot)->offsets[(*slot)->cur_offset] = offset;
          (*slot)->cur_offset = (*slot)->cur_offset + 1;
        }
    }
}

/* Hashtable functions for vtable_registration hashtables.  */

static hashval_t
hash_vtable_registration (const void * p)
{
  const struct vtable_registration *n = (const struct vtable_registration *) p;
  return (hashval_t) (DECL_UID (n->vtable_decl));
}

static int
eq_vtable_registration (const void *p1, const void *p2)
{
  const struct vtable_registration *n1 =
                                    (const struct vtable_registration *) p1;
  const struct vtable_registration *n2 =
                                    (const struct vtable_registration *) p2;
  return (DECL_UID (n1->vtable_decl) == DECL_UID (n2->vtable_decl));
}

/* End of hashtable functions for "registered" hashtables*/

/* Hashtable functions for vtable_map variables hashtable.  */

static htab_t vtbl_map_hash = NULL;
struct vtbl_map_node *vtbl_map_nodes = NULL;
struct vtbl_map_node **vtbl_map_nodes_array = NULL;

static void
vtable_map_array_insert (struct vtbl_map_node *node)
{
  static unsigned array_size = 0;
  unsigned i;

  if (vtbl_map_nodes_array == NULL
      || array_size == 0)
    {
      array_size = 16;
      vtbl_map_nodes_array = (struct vtbl_map_node **)
                       xmalloc (array_size * sizeof (struct vtbl_map_node *));
      memset (vtbl_map_nodes_array, 0,
              array_size * sizeof (struct vtbl_map_node *));
    }
  else if (node->uid >= array_size)
    {
      unsigned new_size = 2 * array_size;
      vtbl_map_nodes_array = (struct vtbl_map_node **)
          xrealloc (vtbl_map_nodes_array,
                    new_size * sizeof (struct vtbl_map_node *));

      for (i = array_size; i < new_size; ++i)
        vtbl_map_nodes_array[i] = NULL;

      array_size = new_size;
    }

  gcc_assert (node->uid < array_size);
  gcc_assert (vtbl_map_nodes_array[node->uid] == NULL);

  vtbl_map_nodes_array[node->uid] = node;
}

/* Returns a hash code for P.  */
static hashval_t
hash_vtbl_map_node (const void *p)
{
  const struct vtbl_map_node *n = (const struct vtbl_map_node *) p;
  return (hashval_t) IDENTIFIER_HASH_VALUE (n->class_name);
}

/* Returns nonzero if P1 and P2 are equal.  */
static int
eq_vtbl_map_node (const void *p1, const void *p2)
{
  const struct vtbl_map_node *n1 = (const struct vtbl_map_node *) p1;
  const struct vtbl_map_node *n2 = (const struct vtbl_map_node *) p2;
  return (IDENTIFIER_HASH_VALUE (n1->class_name) ==
          IDENTIFIER_HASH_VALUE (n2->class_name));
}

/* Return vtbl_map node assigned to DECL without creating a new one.  */
struct vtbl_map_node *
vtbl_map_get_node (const_tree class_name)
{
  struct vtbl_map_node key;
  struct vtbl_map_node **slot;

  if (!vtbl_map_hash)
    return NULL;

  key.class_name = CONST_CAST2 (tree, const_tree, class_name);
  slot = (struct vtbl_map_node **) htab_find_slot (vtbl_map_hash, &key,
                                                   NO_INSERT);
  if (!slot)
    return NULL;
  return *slot;
}

/* Return vtbl_map node assigned to BASE_CLASS_TYPE.  Create new one
 * when needed.  */
struct vtbl_map_node *
find_or_create_vtbl_map_node (tree base_class_type)
{
  struct vtbl_map_node key;
  struct vtbl_map_node *node;
  struct vtbl_map_node **slot;
  unsigned i;

  if (!vtbl_map_hash)
    vtbl_map_hash = htab_create (10, hash_vtbl_map_node,
                                 eq_vtbl_map_node, NULL);

  if (TREE_CHAIN (base_class_type))
    key.class_name = DECL_ASSEMBLER_NAME (TREE_CHAIN (base_class_type));
  else
    key.class_name = DECL_ASSEMBLER_NAME (TYPE_NAME (base_class_type));
  slot = (struct vtbl_map_node **) htab_find_slot (vtbl_map_hash, &key,
                                                   INSERT);
  if (*slot)
    return *slot;

  node = (struct vtbl_map_node *) xmalloc (sizeof (struct vtbl_map_node));
  node->vtbl_map_decl = NULL_TREE;
  node->class_name = key.class_name;
  node->uid = num_vtable_map_nodes++;

  node->class_info = (struct vtv_graph_node *)
                                      xmalloc (sizeof (struct vtv_graph_node));
  node->class_info->class_type = base_class_type;
  node->class_info->class_uid = node->uid;
  node->class_info->max_parents = 4;
  node->class_info->max_children = 4;
  node->class_info->num_parents = 0;
  node->class_info->num_children = 0;
  node->class_info->num_processed_children = 0;
  node->class_info->parents = (struct vtv_graph_node **)
                                xmalloc (4 * sizeof (struct vtv_graph_node *));
  node->class_info->children = (struct vtv_graph_node **)
                                xmalloc (4 * sizeof (struct vtv_graph_node *));
  for (i = 0; i < 4; ++i)
    {
      node->class_info->parents[i] = NULL;
      node->class_info->children[i] = NULL;
    }

  node->registered = htab_create (16, hash_vtable_registration,
                                  eq_vtable_registration, NULL);
  node->is_used = false;
  node->next = vtbl_map_nodes;
  if (vtbl_map_nodes)
    vtbl_map_nodes->prev = node;

  vtable_map_array_insert (node);

  vtbl_map_nodes = node;
  *slot = node;
  return node;
}

/* End of hashtable functions for vtable_map variables hash table.   */

static tree
my_build1 (enum tree_code code, tree type, tree node MEM_STAT_DECL)
{
  int length = sizeof (struct tree_exp);
#ifdef GATHER_STATISTICS
  tree_node_kind kind;
#endif
  tree t;

#ifdef GATHER_STATISTICS
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_statement:  /* an expression with side effects */
      kind = s_kind;
      break;
    case tcc_reference:  /* a reference */
      kind = r_kind;
      break;
    default:
      kind = e_kind;
      break;
    }

  tree_node_counts[(int) kind]++;
  tree_node_sizes[(int) kind] += length;
#endif

  gcc_assert (TREE_CODE_LENGTH (code) == 1);

  t = ggc_alloc_zone_tree_node_stat (&tree_zone, length PASS_MEM_STAT);

  memset (t, 0, sizeof (struct tree_common));

  TREE_SET_CODE (t, code);

  TREE_TYPE (t) = type;
  SET_EXPR_LOCATION (t, UNKNOWN_LOCATION);
  TREE_OPERAND (t, 0) = node;
  TREE_BLOCK (t) = NULL_TREE;
  if (node && !TYPE_P (node))
    {
      TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (node);
      TREE_READONLY (t) = TREE_READONLY (node);
    }

  if (TREE_CODE_CLASS (code) == tcc_statement)
    TREE_SIDE_EFFECTS (t) = 1;
  else switch (code)
    {
    case VA_ARG_EXPR:
      /* All of these have side-effects, no matter what their
         operands are.  */
      TREE_SIDE_EFFECTS (t) = 1;
      TREE_READONLY (t) = 0;
      break;

    case INDIRECT_REF:
      /* Whether a dereference is readonly has nothing to do with whether
         its operand is readonly.  */
      TREE_READONLY (t) = 0;
      break;

    case ADDR_EXPR:
      if (node)
        recompute_tree_invariant_for_addr_expr (t);
      break;

    default:
      if ((TREE_CODE_CLASS (code) == tcc_unary || code == VIEW_CONVERT_EXPR)
          && node && !TYPE_P (node)
          && TREE_CONSTANT (node))
        TREE_CONSTANT (t) = 1;
      if (TREE_CODE_CLASS (code) == tcc_reference
          && node && TREE_THIS_VOLATILE (node))
        TREE_THIS_VOLATILE (t) = 1;
      break;
    }

  return t;
}

static int
type_name_is_vtable_pointer (tree node)
{

  if (TYPE_NAME (node))
  {
    if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
      return (strcmp (IDENTIFIER_POINTER (TYPE_NAME (node)),
                      "__vtbl_ptr_type") == 0);
    else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
             && DECL_NAME (TYPE_NAME (node)))
      return (strcmp (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node))),
                      "__vtbl_ptr_type") == 0);
    else
      return 0;
  }

  return 0;
}

static int
is_vtable_assignment_stmt (gimple stmt)
{

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return 0;
  else
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (lhs) != SSA_NAME)
        return 0;

      if (TREE_CODE (TREE_TYPE (lhs)) != POINTER_TYPE)
        return 0;

      if (TREE_CODE (TREE_TYPE (TREE_TYPE (lhs))) != POINTER_TYPE)
        return 0;

      if (! type_name_is_vtable_pointer (TREE_TYPE (TREE_TYPE (lhs))))
        return 0;


      if (TREE_CODE (rhs) != COMPONENT_REF)
        return 0;

      if (! (TREE_OPERAND (rhs, 1))
          || (TREE_CODE (TREE_OPERAND (rhs, 1)) != FIELD_DECL))
        return 0;

      if (! (DECL_NAME (TREE_OPERAND (rhs, 1)))
          || (strncmp (IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (rhs, 1))),
                       "_vptr.", 6) != 0))
        return 0;

    }

    return 1;
}

/* This is a copy of the function in cp/class.c.  For some reason that file
   does not get linked with this one during the gcc build process, so we
   need to reproduce the function here in order to be able to call it.  */

static tree
my_get_vtbl_decl_for_binfo (tree binfo)
{
  tree decl;

  decl = BINFO_VTABLE (binfo);
  if (decl && TREE_CODE (decl) == POINTER_PLUS_EXPR)
  {
    gcc_assert (TREE_CODE (TREE_OPERAND (decl, 0)) == ADDR_EXPR);
    decl = TREE_OPERAND (TREE_OPERAND (decl, 0), 0);
  }
  if (decl)
    gcc_assert (TREE_CODE (decl) == VAR_DECL);

  return decl;
}

static void
verify_bb_vtables (basic_block bb)
{
  gimple_seq stmts;
  gimple stmt = NULL;
  gimple_stmt_iterator gsi_vtbl_assign;
  gimple_stmt_iterator gsi_virtual_call;
  tree this_object;

  /* Search the basic block to see if it contains a virtual method
     call, i.e. a call with the tree code OBJ_TYPE_REF  */

  stmts = bb_seq (bb);
  gsi_virtual_call = gsi_start (stmts);
  this_object = NULL_TREE;
  for (; !gsi_end_p (gsi_virtual_call); gsi_next (&gsi_virtual_call))
    {
      stmt = gsi_stmt (gsi_virtual_call);
      if (gimple_code (stmt) == GIMPLE_CALL)
        {
          tree fncall = gimple_call_fn (stmt);
          if (TREE_CODE (fncall) == OBJ_TYPE_REF)
            {
              bool found = false;
              tree vtable_offset_var = NULL_TREE;
              gimple def_stmt;
              gimple prev_use = NULL;

              /* The first argument to the function must be "this", a pointer
                 to the object itself.  */

              this_object = gimple_call_arg (stmt, 0);

              /* Get the SSA variable that contains the dereferenced _vptr
                 field + table start offset.  */

              if (TREE_OPERAND (fncall, 0)
                  && TREE_CODE (TREE_OPERAND (fncall, 0)) == SSA_NAME)
                {
                  tree rhs = NULL_TREE;

                  vtable_offset_var = TREE_OPERAND (fncall, 0);

                  prev_use = stmt;
                  def_stmt = SSA_NAME_DEF_STMT (vtable_offset_var);

                  /* This first def_stmt should be a de-reference of the
                     vtable-pointer-plus-offset, something like this:

                     D.2203_6 = *D.2202_5;

                     So we need to find the ssa variable being dereferenced,
                     then find it's def_stmt. (This is not in the loop
                     below because of the need to dereference it.)  */

                  if (gimple_assign_lhs (def_stmt)
                      && TREE_CODE (gimple_assign_lhs (def_stmt)) == SSA_NAME
                      && get_gimple_rhs_class (gimple_expr_code (def_stmt))
                                                          == GIMPLE_SINGLE_RHS)
                    rhs = gimple_assign_rhs1 (def_stmt);

                  if (rhs
                      && TREE_CODE (rhs) == MEM_REF
                      && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
                    {
                      prev_use = def_stmt;
                      def_stmt = SSA_NAME_DEF_STMT (TREE_OPERAND (rhs, 0));
                    }

                  /* Search backwards through the def_stmt chain, to try
                     to find the assignment statement where the rhs of
                     the assignment contains the "._vptr" field (the vtable
                     pointer). */

                  while (def_stmt
                         && !is_vtable_assignment_stmt (def_stmt))
                    {
                      tree lhs = gimple_assign_lhs (def_stmt);
                      if (!lhs
                          || !TREE_CODE (lhs) == SSA_NAME)
                        {
                          def_stmt = NULL;
                          break;
                        }

                      if (gimple_assign_rhs1 (def_stmt)
                          && TREE_CODE (gimple_assign_rhs1 (def_stmt))
                                                                   == SSA_NAME)
                        {
                          prev_use = def_stmt;
                          rhs = gimple_assign_rhs1 (def_stmt);
                          def_stmt = SSA_NAME_DEF_STMT (rhs);
                        }
                      else if ((get_gimple_rhs_class
                                           (gimple_expr_code (def_stmt))
                                   != GIMPLE_SINGLE_RHS)
                               && gimple_assign_rhs2 (def_stmt)
                               && TREE_CODE (gimple_assign_rhs2 (def_stmt))
                                                                   == SSA_NAME)
                        {
                          prev_use = def_stmt;
                          rhs = gimple_assign_rhs2 (def_stmt);
                          def_stmt = SSA_NAME_DEF_STMT (rhs);
                        }
                      else
                        {
                          def_stmt = NULL;
                          break;
                        }
                    }

                  /* If we found the vtable pointer assignment statement by
                     itself, we also need to find it within the basic block
                     statement sequence, so that we can insert our statements
                     into the sequence.

                     The following loop looks for the assignment statement
                     within the basic block's sequence of statements.  */

                  if (def_stmt)
                    {
                      basic_block def_bb;
                      gimple_seq def_bb_stmts;

                      def_bb = gimple_bb (def_stmt);
                      def_bb_stmts = bb_seq (def_bb);

                      gsi_vtbl_assign = gsi_start (def_bb_stmts);

                      for (; !gsi_end_p (gsi_vtbl_assign) && !found;
                           gsi_next (&gsi_vtbl_assign))
                        {
                          stmt = gsi_stmt (gsi_vtbl_assign);
                          if (stmt == def_stmt)
                            {
                              found = true;
                              break;
                            }
                        }

                      if (found)
                        {
                          tree object_rhs = TREE_TYPE (this_object);
                          tree lhs;
                          tree vtbl_var_decl = NULL_TREE;
                          tree vtbl = NULL_TREE;
                          tree var_id;
                          gimple_seq pre_p = NULL;
                          struct vtbl_map_node *vtable_map_node = NULL;
                          tree vtbl_decl = NULL_TREE;
                          tree expr_tree = NULL_TREE;
                          struct gimplify_ctx gctx;
                          const char *vtable_name = "<unknown>";
                          int len1 = 0;
                          int len2 = 0;

                          lhs = gimple_assign_lhs (stmt);

                          /* Now we have found the virtual method dispatch
                             and the preceding access of the _vptr.*
                             field... Now we need to find the vtable for
                             the base class (statically declared type) of
                             the object, so we can use the right vtable
                             map variable.  */

                          /* First try to get the type out of the 'this'
                             object. */
                          if (TREE_CODE (object_rhs) == POINTER_TYPE
                              && TREE_CODE (TREE_TYPE (object_rhs))
                                                                == RECORD_TYPE)
                            rhs = TREE_TYPE (object_rhs);
                          else if (TREE_CODE (object_rhs) == REFERENCE_TYPE
                                   && TREE_CODE (TREE_TYPE (object_rhs))
                                                                == RECORD_TYPE)
                            rhs = TREE_TYPE (object_rhs);


                          /* Check to see if the type from the 'this'
                             object will work or not.  Sometimes the
                             type of the 'this' object is not usable
                             (usually due to optimizations which
                             change the type of the 'this' object to
                             'void *'); try to get the type out of the
                             rhs of the vtable pointer assignment
                             statement.  */

                          if ((TREE_CODE (rhs) != RECORD_TYPE)
                              || (! TYPE_BINFO (rhs))
                              || (! BINFO_VTABLE (TYPE_BINFO (rhs)))
                              || (! my_get_vtbl_decl_for_binfo (TYPE_BINFO
                                                                       (rhs))))
                            {
                              /* The type of the 'this' object did not work,
                                 so try to find the type from the rhs of the
                                 def_stmt.

                                 The def_stmt usually looks something like
                                 this:

                                 D.2201_4 =
                                   MEM[(struct Event *)this_1(D)]._vptr.Event;

                                We're trying to find and extract the type cast
                                from that stmt.  */

                              rhs = gimple_assign_rhs1 (def_stmt);
                              if (TREE_CODE (rhs) == COMPONENT_REF)
                                {
                                  while (TREE_CODE (rhs) == COMPONENT_REF
                                         && (TREE_CODE (TREE_OPERAND (rhs, 0))
                                                             == COMPONENT_REF))
                                    rhs = TREE_OPERAND (rhs, 0);

                                  if (TREE_CODE (rhs) == COMPONENT_REF
                                      && (TREE_CODE (TREE_OPERAND (rhs, 0))
                                                                    == MEM_REF)
                                      && (TREE_CODE (TREE_TYPE
                                                    (TREE_OPERAND (rhs, 0)))
                                                               == RECORD_TYPE))
                                    rhs = TREE_TYPE (TREE_OPERAND (rhs, 0));
                                  else
                                    rhs = NULL_TREE;
                                }
                              else
                                rhs = NULL_TREE;
                            }

                          /* Make sure we found a valid type...*/
                          gcc_assert (rhs);
                          gcc_assert (TREE_CODE (rhs) == RECORD_TYPE);
                          gcc_assert (TYPE_BINFO (rhs));

                          /* Get the vtable for the type.  */
                          vtbl_var_decl = my_get_vtbl_decl_for_binfo
                                                            (TYPE_BINFO (rhs));
                          vtbl = BINFO_VTABLE (TYPE_BINFO (rhs));

                          gcc_assert (vtbl_var_decl && vtbl);

                          vtbl_decl = vtbl_var_decl;

                          if (TREE_CODE (TREE_TYPE (vtbl)) == POINTER_TYPE)
                            force_gimple_operand (vtbl, &pre_p, 1, NULL);

                          if (TREE_CHAIN (rhs))
                            var_id = DECL_ASSEMBLER_NAME (TREE_CHAIN (rhs));
                          else
                            var_id = DECL_ASSEMBLER_NAME (TYPE_NAME (rhs));

                          vtable_map_node = vtbl_map_get_node (var_id);

                          /* Build  verify_vtbl_ptr_fndecl */

                          build_vtable_verify_fndecl ();
                          gcc_assert(verify_vtbl_ptr_fndecl);
                          /* Given the vtable pointer for the base
                             class of the object, build the call to
                             __VLTVerifyVtablePointer to verify that
                             the object's vtable pointer (contained in
                             lhs) is in the set of valid vtable
                             pointers for the base class.  */

                          /* Have problems with following assert. It shows we 
                             are not protecting everything */
                          /* gcc_assert(verify_vtbl_ptr_fndecl && vtbl_var_decl); */
                          if (vtable_map_node && vtable_map_node->vtbl_map_decl)
                            {
                              vtable_map_node->is_used = true;
                              vtbl_var_decl = vtable_map_node->vtbl_map_decl;

                              if (TREE_CODE (vtbl_decl) == VAR_DECL)
                                vtable_name = IDENTIFIER_POINTER
                                    (DECL_NAME (vtbl_decl));

                              push_gimplify_context (&gctx);
                              len1 = strlen (IDENTIFIER_POINTER
                                                  (DECL_NAME (vtbl_var_decl)));
                              len2 = strlen (vtable_name);

                              /* Call different routines if we are
                                 interested in trace information to
                                 triage problems */
#ifdef VTV_DEBUG
                              expr_tree = build_call_expr
                                  (verify_vtbl_ptr_fndecl, 4,
                                   my_build1 (ADDR_EXPR,
                                              TYPE_POINTER_TO
                                              (TREE_TYPE (vtbl_var_decl)),
                                              vtbl_var_decl),
                                   SSA_NAME_VAR (lhs),
                                   build_string_literal
                                   (len1 + 1,
                                    IDENTIFIER_POINTER
                                    (DECL_NAME
                                     (vtbl_var_decl))),
                                   build_string_literal(
                                       len2 + 1, vtable_name));
#else
                              expr_tree = build_call_expr
                                  (verify_vtbl_ptr_fndecl, 2,
                                   my_build1 (ADDR_EXPR,
                                              TYPE_POINTER_TO
                                              (TREE_TYPE (vtbl_var_decl)),
                                              vtbl_var_decl),
                                   SSA_NAME_VAR (lhs));
#endif
                              /* Assign the result of the call to the
                                 original variable receiving the
                                 assignment of the object's vtable
                                 pointer; mark that variable to be
                                 updated by update_ssa.  */

                              mark_sym_for_renaming (SSA_NAME_VAR (lhs));
                              force_gimple_operand (expr_tree, &pre_p, 1,
                                                    SSA_NAME_VAR (lhs));

                              /* Insert the new call just after the
                                 original assignment of the object's
                                 vtable pointer.  */

                              pop_gimplify_context (NULL);
                              gsi_insert_seq_after (&gsi_vtbl_assign, pre_p,
                                                    GSI_NEW_STMT);
                              any_verification_calls_generated = true;
                            }
                        }
                    }
                }
            }
        }
    }
}

static void
build_vtable_verify_fndecl (void)
{
  tree void_ptr_type = build_pointer_type (void_type_node);
  tree arg_types = NULL_TREE;
  tree func_type = NULL_TREE;
  struct lang_decl *ld;
#ifdef VTV_DEBUG
  tree const_char_ptr_type = build_pointer_type (
      build_qualified_type(char_type_node, TYPE_QUAL_CONST));
#endif

  if (verify_vtbl_ptr_fndecl != NULL_TREE)
    return;

  ld = ggc_alloc_cleared_lang_decl (sizeof (struct lang_decl_fn));
  ld->u.base.selector = 1;

  arg_types = build_tree_list (NULL_TREE, build_pointer_type (void_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, 
                                                   const_ptr_type_node));

#ifdef VTV_DEBUG
  /* Start: Arg types to be removed when we remove debugging parameters from
     the library function. */
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, 
                                                   const_char_ptr_type));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, 
                                                   const_char_ptr_type));
  /* End: Arg types to be removed...*/
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
  DECL_PURE_P(verify_vtbl_ptr_fndecl) = 1;
  TREE_PUBLIC (verify_vtbl_ptr_fndecl) = 1;
#ifdef VTV_STATIC_VERIFY
  DECL_VISIBILITY(verify_vtbl_ptr_fndecl) = VISIBILITY_HIDDEN;
#endif
  DECL_PRESERVE_P (verify_vtbl_ptr_fndecl) = 1;
  DECL_LANG_SPECIFIC (verify_vtbl_ptr_fndecl) = ld;
  SET_DECL_LANGUAGE (verify_vtbl_ptr_fndecl, lang_cplusplus);
}

unsigned int
vtable_verify_main (void)
{
  unsigned int ret = 1;
  basic_block bb;

  FOR_ALL_BB (bb)
      verify_bb_vtables (bb);

  return ret;
}

static bool
gate_tree_vtable_verify (void)
{
  return (flag_vtable_verify
          && (strcmp (lang_hooks.name, "GNU C++") == 0));
}

struct gimple_opt_pass pass_vtable_verify =
{
 {
  GIMPLE_PASS,
  "vtable-verify",                      /* name */
  gate_tree_vtable_verify,              /* gate */
  vtable_verify_main,                   /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_VTABLE_VERIFICATION,               /* tv_id */
  PROP_cfg | PROP_ssa,                  /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func | TODO_update_ssa
    | TODO_ggc_collect                  /* todo_flags_finish */
 }
};
