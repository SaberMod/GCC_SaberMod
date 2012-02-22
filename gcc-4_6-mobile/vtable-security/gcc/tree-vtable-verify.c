/* Interprocedural constant propagation
   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

   Contributed by Razya Ladelsky <RAZYA@il.ibm.com> and Martin Jambor
   <mjambor@suse.cz>

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

static GTY(()) tree verify_vtbl_ptr_fndecl = NULL_TREE;

unsigned int vtable_verify_main (void);
static bool gate_tree_vtable_verify (void);
static void build_vtable_verify_fndecls (void);

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

      while ((TREE_OPERAND (rhs, 0))
             && (TREE_CODE (TREE_OPERAND (rhs, 0)) == COMPONENT_REF))
        rhs = TREE_OPERAND (rhs, 0);

      if (! (TREE_OPERAND (rhs, 0))
          || (TREE_CODE (TREE_OPERAND (rhs, 0)) != MEM_REF))
        return 0;
    }

    return 1;
}

/* This is a copy of the function in cp/class.c.  For some reason that file
   does not get linked with this one during the gcc build process, so we
   need to reproduce the function here in order to be able to call it.  */

static tree
get_vtbl_decl_for_binfo (tree binfo)
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
  bool bb_contains_virtual_call = false;
  gimple_stmt_iterator gsi_vtbl_assign;
  gimple_stmt_iterator gsi_virtual_call;

  /* Search the basic block to see if it contains a virtual method
     call, i.e. a call with the tree code OBJ_TYPE_REF  */

  stmts = bb_seq (bb);
  gsi_virtual_call = gsi_start (stmts);
  for (; !gsi_end_p (gsi_virtual_call); gsi_next (&gsi_virtual_call))
    {
      stmt = gsi_stmt (gsi_virtual_call);
      if (gimple_code (stmt) == GIMPLE_CALL)
        {
          tree fncall = gimple_call_fn (stmt);
          if (TREE_CODE (fncall) == OBJ_TYPE_REF)
            {
              bb_contains_virtual_call = true;
              break;
            }
        }
    }

  if (bb_contains_virtual_call)
    {
      /* If we found a virtual method call, it must be preceded by assigning
         the vtable pointer in the object to something.  Search backwards
         from the virtual method call to the assignment statement whose
         right hand side is a vtable pointer.  */

      bool found = false;
      gsi_vtbl_assign = gsi_virtual_call;
      for (; !gsi_end_p (gsi_vtbl_assign) && !found;
           gsi_prev (&gsi_vtbl_assign))
        {
          stmt = gsi_stmt (gsi_vtbl_assign);
          if (is_vtable_assignment_stmt (stmt))
            {
              tree rhs = gimple_assign_rhs1 (stmt);
              tree lhs = gimple_assign_lhs (stmt);
              tree vtbl_var_decl = NULL_TREE;
              tree vtbl = NULL_TREE;
              tree vtbl_ptr = NULL_TREE;
              gimple_seq pre_p = NULL;

              /* Now we have found the virtual method dispatch and the
                 preceding access of the _vptr.* field... Now we need to
                 find the vtable for the base class (statically declared type)
                 of the object.  */

              found = true;
              if (TREE_CODE (rhs) == COMPONENT_REF)
                {
                  while (TREE_CODE (TREE_OPERAND (rhs, 0)) == COMPONENT_REF)
                    rhs = TREE_OPERAND (rhs, 0);
                  if (TREE_OPERAND (rhs, 0) != NULL)
                    rhs = TREE_OPERAND (rhs, 0);
                  if ((TREE_CODE (TREE_TYPE (rhs)) == RECORD_TYPE)
                      && TYPE_BINFO (TREE_TYPE (rhs)))
                    {
                      vtbl_var_decl = get_vtbl_decl_for_binfo
                                                (TYPE_BINFO (TREE_TYPE (rhs)));
                      vtbl = BINFO_VTABLE (TYPE_BINFO (TREE_TYPE (rhs)));

                      if (TREE_CODE (TREE_TYPE (vtbl)) == POINTER_TYPE)
                        vtbl_ptr = force_gimple_operand (vtbl, &pre_p, 1,
                                                         NULL);
                    }

                  /* Build  verify_vtbl_ptr_fndecl */

                  build_vtable_verify_fndecls ();

                  /* Given the vtable pointer for the base class of the object,
                     build the call to __VerifyVtablePointer to verify that the
                     object's vtable pointer (contained in lhs) is in the set
                     of valid vtable pointers for the base class.  */

                  if (verify_vtbl_ptr_fndecl && vtbl_ptr)
                    {
                      tree expr_tree = NULL_TREE;
                      tree t = NULL_TREE;
                      struct gimplify_ctx gctx;

                      push_gimplify_context (&gctx);
                      expr_tree = build_call_expr (verify_vtbl_ptr_fndecl, 2,
                                                   /* vtbl_ptr, */
                                                   SSA_NAME_VAR (lhs),
                                                   SSA_NAME_VAR(lhs));

                      /* Assign the result of the call to the original
                         variable receiving the assignment of the object's
                         vtable pointer; mark that variable to be updated by
                         update_ssa.  */

                      mark_sym_for_renaming (SSA_NAME_VAR (lhs));
                      t = force_gimple_operand (expr_tree, &pre_p, 1,
                                                SSA_NAME_VAR (lhs));

                      /* Insert the new call just after the original assignment
                         of the object's vtable pointer.  */

                      pop_gimplify_context (NULL);
                      gsi_insert_seq_after (&gsi_vtbl_assign, pre_p,
                                            GSI_NEW_STMT);
                    }
                }
            }
        }
    }
}

static void
build_vtable_verify_fndecls (void)
{
  tree void_ptr_type = build_pointer_type (void_type_node);

  tree decls = NULL_TREE;
  tree parms = NULL_TREE;
  tree arg_types = NULL_TREE;
  tree parm = NULL_TREE;
  tree type = build_pointer_type (void_type_node);
  tree name = get_identifier ("__VerifyVtablePointer");

  parm = build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL_TREE, void_ptr_type);
  decls = build_tree_list (NULL_TREE, parm);
  arg_types = build_tree_list (NULL_TREE, void_ptr_type);

  parm = build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL_TREE, void_ptr_type);
  decls = chainon (decls, build_tree_list (NULL_TREE, parm));
  arg_types = chainon (arg_types, build_tree_list (NULL_TREE, void_ptr_type));

  type = build_function_type (type, arg_types);

  verify_vtbl_ptr_fndecl = build_fn_decl ("__VerifyVtablePointer", type);
  TREE_NOTHROW (verify_vtbl_ptr_fndecl) = 1;
  DECL_ATTRIBUTES (verify_vtbl_ptr_fndecl)
      = tree_cons (get_identifier ("leaf"), NULL,
                   DECL_ATTRIBUTES (verify_vtbl_ptr_fndecl));

  /* Do this so we don't use the mangled name in function calls.  */
  DECL_LANG_SPECIFIC (verify_vtbl_ptr_fndecl) = NULL;
  TREE_PUBLIC (verify_vtbl_ptr_fndecl) = 1;
  DECL_PRESERVE_P (verify_vtbl_ptr_fndecl) = 1;
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
