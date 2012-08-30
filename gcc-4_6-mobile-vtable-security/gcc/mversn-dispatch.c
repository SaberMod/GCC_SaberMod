/* Mulitversion Dispatch Pass.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Sriraman Tallam (tmsriram@google.com)

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
<http://www.gnu.org/licenses/>. */


/* This pass processes __builtin_dispatch calls to call multi-versioned
   functions. Only two versions are supported now. Example use :

  int popcnt_sse4(unsigned int x) __attribute__((__target__("popcnt")));
  int popcnt_sse4(unsigned int x)
  {
    int count = __builtin_popcount(x);
    return count;
  }

  int popcnt(unsigned int x) __attribute__((__target__("no-popcnt")));
  int popcnt(unsigned int x)
  {
    int count = __builtin_popcount(x);
    return count;
  }

  int testsse() __attribute__((version_selector));
  int main ()
  {
    ...
    ret = __builtin_dispatch (testsse,  (void*)popcnt_sse4, (void*)popcnt, 25);
    ...
  }

  There are two passes that are run to achieve multi-versioning.
  "pass_ipa_multiversion_dispatch" is an ipa pass that decides which functions
  have to be cloned and hoists the feature-test calls appropriately.  This
  pass can be enabled with the flag "-fclone-hot-version-paths" and disabled
  with "-fno-clone-hot-version-paths".

  "pass_tree_convert_builtin_dispatch" does the lowering.  It is a
  function-level pass.  Functions marked with attribute "version_selector" are
  also handled by this pass.  This pass is always on.

  How to use __builtin_dispatch ?
  -----------------------------

  __builtin_dispatch takes 3 mandatory arguments :

  __builtin_dispatch (arg1, arg2, arg3, <arg4>, <arg5>, ...);

  arg1 is the pointer to the feature-test function.
  arg2 is the ( void *) cast pointer to the versioned function that is
  executed when the feature test returns 1.
  arg3 is the ( void *) cast pointer to the versioned function that is
  executed when the feature test returns 0.
  arg4, arg5, ... are optional. They are the arguments to the versioned
  functions.  Both versions must accept the same number of arguments.
  The __builtin_dispatch function returns the value returned by the
  versioned function that gets executed.  The versioned function arg2
  is executed when the feature_test function arg1 returns 1 and arg3
  is executed when the feature_test function arg1 returns 0. arg1
  could be marked as a "version_selector" function if it is a pure
  function with no side-effects, returns a constant at run-time and
  can be evaluated at any point in the execution.

  When to use the "version_selector" attribute ?
  -----------------------------------------------

  Functions are marked with attribute "version_selector" only if
  they are run-time constants.  Example of such functions would
  be those that test if a specific feature is available on a
  particular architecture.  Such functions must return a positive
  integer. For two-way functions, those that test if a feature
  is present or not must return 1 or 0 respectively.


  The code is organized into five parts.  The first part has the functionality
  to detect and handle functions marked with attribute "version_selector".  The
  second part is the analysis phase where we find calls to __builtin_dispatch
  and mark all functions that are hot and have a call-graph path to a
  __builtin_dispatch call.  The third part decides which functions
  to clone.  This is based on the number of clones that have to be created for
  the functions marked in the analysis phase. Only two clones are allowed for
  a function currently.  The fourth part is where the actual cloning happens.
  The fifth part contains the implementation to lower the __builtin_dispatch
  calls.

  Flags : -fclone-hot-version-paths does function unswitching via cloning.
          --param=num-mversn-clones=<num> allows to specify the number of
          functions that should be cloned.
	  --param=mversn-clone-depth=<num> allows to specify the length of
          the call graph path that should be cloned.  num = 0 implies only
          leaf node that contains the __builtin_dispatch statement must be
          cloned. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "flags.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "toplev.h"
#include "timevar.h"
#include "params.h"
#include "fibheap.h"
#include "intl.h"
#include "tree-pass.h"
#include "hashtab.h"
#include "coverage.h"
#include "ggc.h"
#include "tree-flow.h"
#include "rtl.h"
#include "ipa-prop.h"
#include "basic-block.h"
#include "toplev.h"
#include "dbgcnt.h"
#include "tree-dump.h"
#include "output.h"
#include "vecprim.h"
#include "gimple-pretty-print.h"
#include "target.h"
#include "cfgloop.h"

typedef struct cgraph_node* NODEPTR;
DEF_VEC_P (NODEPTR);
DEF_VEC_ALLOC_P (NODEPTR, heap);

/* Store the decl of __builtin_dispatch */
static tree builtin_function_decl = NULL;

/* Hash to map name to a decl.  Used for variables and functions. */
static htab_t name_decl_htab = NULL;

/* Hashtable helpers for name_decl_htab. */

static hashval_t
name_decl_htab_hash_descriptor (const void *p)
{
  const_tree t = (const_tree) p;
  const char *name
    = (IDENTIFIER_POINTER (DECL_NAME (t)));
  return htab_hash_string(name);
}

/* Hashtable helper for name_decl_htab. */

static int
name_decl_htab_eq_descriptor (const void *p1, const void *p2)
{
  const_tree t1 = (const_tree) p1;
  const char *c1 = IDENTIFIER_POINTER (DECL_NAME (t1));
  const char *c2 = (const char *)p2;

  return (strcmp (c1, c2) == 0);
}

/* Return true if NODE is a hot function.  It is a hot function
   if its execution frequency is determined to be hot or
   if any of its incoming or outgoing call-graph edges is hot.  */

static bool
hot_function_p (struct cgraph_node *node)
{
  struct cgraph_edge *edge;

  if (node->frequency == NODE_FREQUENCY_HOT)
    return true;

  for (edge = node->callees; edge; edge = edge->next_callee)
    if (cgraph_maybe_hot_edge_p (edge))
      return true;

  for (edge = node->callers; edge; edge = edge->next_caller)
    if (cgraph_maybe_hot_edge_p (edge))
      return true;

  return false;
}

/* Return the number of arguments that a function has.  */

static int
function_args_count (tree fntype)
{
  function_args_iterator args_iter;
  tree t;
  int num = 0;

  if (fntype)
    {
      FOREACH_FUNCTION_ARGS(fntype, t, args_iter)
	{
	  num++;
	}
    }

  return num;
}

/* Return a new name by appending SUFFIX to the DECL name. */

static char *
make_name (tree decl, const char *suffix)
{
  char *global_var_name;
  int name_len;
  const char *name;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  name_len = strlen (name) + strlen (suffix) + 2;
  global_var_name = (char *) xmalloc (name_len);
  /* Use '.' to concatenate names as it is demangler friendly.  */
  snprintf (global_var_name, name_len, "%s.%s", name, suffix);
  return global_var_name;
}

/* Code for handling version_selector attribute functions.  Such functions are
   run-time constants and need to be executed only once.  They are hoisted
   to a static constructor and their result is stored in a global.
 */


/* This function returns the global variable / constructor name created
   for feature-test functions marked with attribute "version_selector".
   The name returned is the DECL name appended with
   "version_selector_global" for the variable and
   "version_selector_constructor" for the constructor. */

static char*
make_feature_test_global_name (tree decl, bool is_constructor)
{
  if (is_constructor)
    return make_name (decl, "version.selector.constructor");

  return make_name (decl, "version.selector.global");
}

/* This function creates a new VAR_DECL with attributes set
   using the parameters.  PUBLIK corresponds to TREE_PUBLIC,
   EXTERNAL corresponds to DECL_EXTERNAL and comdat is
   for DECL_ONE_ONLY.  The global variable will have the
   same status as the version_selector function.*/

static tree
allocate_new_var (const char *name, int publik,
                  int external, int comdat)
{
  tree new_global_var;
  struct varpool_node *vnode;

  new_global_var = build_decl (UNKNOWN_LOCATION,
			       VAR_DECL,
	  	               get_identifier (name),
		               integer_type_node);

  DECL_EXTERNAL (new_global_var) = external;
  TREE_STATIC (new_global_var) = 1;
  TREE_PUBLIC (new_global_var) = publik;
  DECL_INITIAL (new_global_var) = 0;
  DECL_ARTIFICIAL (new_global_var) = 1;
  DECL_PRESERVE_P (new_global_var) = 1;

  if (comdat)
    make_decl_one_only (new_global_var, DECL_ASSEMBLER_NAME (new_global_var));
  assemble_variable (new_global_var, 0, 0, 0);

  vnode = varpool_node (new_global_var);
  gcc_assert (vnode != NULL);
  /* Set finalized to 1, otherwise it asserts in function "write_symbol" in
     lto-streamer-out.c. */
  vnode->finalized = 1;
  
  return new_global_var;
}

/* Make a new constructor function here to call a feature-test function
   and set its body to CONSTRUCTOR_BODY.  Its public and comdat
   attributes are set from the parameters, PUBLIK, and COMDAT.
   VERSION_SELECTOR_VAR is the global decl that saves the result of the
   feature-test function in the constructor. */

static tree
make_constructor_function (char *name, gimple constructor_body, int publik,
			   int comdat, tree version_selector_var)
{
  tree decl, type, t;
  gimple_seq seq;
  basic_block new_bb;
  tree old_current_function_decl;

  type = build_function_type_list (void_type_node, NULL_TREE);

  if (dump_file)
    fprintf (dump_file, "Name of new constructor function = %s\n", name);

  decl = build_fn_decl (name, type);

  DECL_NAME (decl) = get_identifier (name);
  SET_DECL_ASSEMBLER_NAME (decl, DECL_NAME (decl));
  gcc_assert (cgraph_node (decl) != NULL);

  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 0;
  TREE_PUBLIC (decl) = publik;
  DECL_UNINLINABLE (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  DECL_STATIC_CONSTRUCTOR (decl) = 1;
  TREE_READONLY (decl) = 0;
  DECL_PURE_P (decl) = 0;

  if (comdat)
    make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));

  /* Build result decl and add to function_decl. */
  t = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (decl) = t;

  gimplify_function_tree (decl);

  /* Build CFG for this function. */

  old_current_function_decl = current_function_decl;
  push_cfun (DECL_STRUCT_FUNCTION (decl));
  current_function_decl = decl;
  init_empty_tree_cfg_for_function (DECL_STRUCT_FUNCTION (decl));
  cfun->curr_properties |=
    (PROP_gimple_lcf | PROP_gimple_leh | PROP_cfg | PROP_referenced_vars |
     PROP_ssa);
  new_bb = create_empty_bb (ENTRY_BLOCK_PTR);
  make_edge (ENTRY_BLOCK_PTR, new_bb, EDGE_FALLTHRU);

  /* XXX: Not sure if the edge commented below is necessary.  If I add this
     edge, it fails in gimple_verify_flow_info in tree-cfg.c in condition :
     " if (e->flags & EDGE_FALLTHRU)"
     during -fprofile-generate.
     Otherwise, it is fine.  Deleting this edge does not break anything.
     Commenting this so that it is clear I am intentionally not doing this.*/
  /* make_edge (new_bb, EXIT_BLOCK_PTR, EDGE_FALLTHRU); */

  seq = gimple_seq_alloc_with_stmt (constructor_body);

  set_bb_seq (new_bb, seq);
  gimple_set_bb (constructor_body, new_bb);

  /* Set the lexical block of the constructor body. Fails the inliner
     other wise. */
  gimple_set_block (constructor_body, DECL_INITIAL (decl));

  /* This call is very important if this pass runs when the IR is in
     SSA form.  It breaks things in strange ways otherwise. */
  init_tree_ssa (DECL_STRUCT_FUNCTION (decl));
  add_referenced_var (version_selector_var);

  cgraph_add_new_function (decl, true);
  cgraph_call_function_insertion_hooks (cgraph_node (decl));
  cgraph_mark_needed_node (cgraph_node (decl));

  if (dump_file)
    dump_function_to_file (decl, dump_file, TDF_BLOCKS);

  pop_cfun ();
  current_function_decl = old_current_function_decl;
  return decl;
}

/* If  the current function is marked with attribute
   "version_selector" then it is the predicate (feature-test) function
   for multi-versioning.  Call this function in a constructor and assign
   the return value to a global variable.
   The constructor's name is the decl name suffixed
   "version_selector_constructor" and the global variable's name is the
   decl name suffixed with "version_selector_global"

   For example, feature-test function isSSE4 marked with attribute
   version_selector is converted to

   void isSSE4_version_selector_constructor ()
   {
     isSSE4_version_selector_global = isSSE4 ();
   }

   This function returns the decl of the global variable.

   THIS_DECL is the function decl of the "version_selector" function.
   */

static tree
handle_version_selector_attr_function (tree this_decl)
{
  char *global_var_name;
  tree version_selector_var = NULL;
  void **slot;

  gcc_assert (!flag_lto);

  if (dump_file)
    fprintf (dump_file, "Creating constructor/global for function %s\n",
	     IDENTIFIER_POINTER (DECL_NAME (this_decl)));

  global_var_name = make_feature_test_global_name (this_decl,
                                                   false);

  slot = htab_find_slot_with_hash (name_decl_htab, global_var_name,
                                   htab_hash_string (global_var_name),
                                   INSERT);
  if (*slot == NULL)
    {
      if (dump_file)
        fprintf (dump_file, "Creating global variable %s\n",
	         global_var_name);
      *slot = allocate_new_var (global_var_name,
                                TREE_PUBLIC (this_decl),
                                DECL_EXTERNAL (this_decl),
                                DECL_ONE_ONLY (this_decl));
    }
  else
    {
      free (global_var_name);
      return (tree) *slot;
    }

  version_selector_var = (tree) *slot;

  /* If the feature-test function is not external, create a constructor and
     call this function in the constructor. */

  if (!DECL_EXTERNAL (this_decl))
    {
      char *constructor_name;
      gimple constructor_body;
      tree constructor_decl;

      constructor_name
        = make_feature_test_global_name (this_decl, true);

      constructor_body = gimple_build_call (this_decl, 0);

      gimple_call_set_lhs (constructor_body, version_selector_var);

      if (dump_file)
        print_gimple_stmt (dump_file, constructor_body, 0, TDF_VOPS);

      constructor_decl =
        make_constructor_function (constructor_name, constructor_body,
				   TREE_PUBLIC (this_decl),
				   DECL_ONE_ONLY (this_decl),
			           version_selector_var);

      gcc_assert (constructor_decl != NULL_TREE);
      free (constructor_name);
    }

  free (global_var_name);
  return version_selector_var;
}

/* Start Analysis phase.  Mark all functions that are hot and have a call-graph
   path to a __builtin_dispatch call. */

/* This function returns the address of the feature test function.
   If the address of the function is saved to a temporary,
   this function traverses the gimple statements before BUILTIN_STMT
   and finds an assignment whose rhs is the feature test function.
   If the feature test function is specified as a function pointer
   whose function value is unknown, this funcition returns NULL. */

static tree
find_version_selector_func_addr (gimple builtin_stmt)
{
  tree cond_func_addr = NULL;
  gimple def_stmt = NULL;

  cond_func_addr = gimple_call_arg (builtin_stmt, 0);

  gcc_assert (TREE_CODE (cond_func_addr) == ADDR_EXPR
	      || TREE_CODE (cond_func_addr) == SSA_NAME);

  if (TREE_CODE (cond_func_addr) == ADDR_EXPR)
    return cond_func_addr;

  /* TREE_CODE (cond_func_addr) ==  SSA_NAME
     This means a new function pointer variable is created and assigned the
     address of the feature-test function. Traverse the statements backwards
     and find the assignment to get the RHS. */

  def_stmt = SSA_NAME_DEF_STMT (cond_func_addr);

  gcc_assert (def_stmt
              && gimple_assign_lhs (def_stmt) == cond_func_addr);

  cond_func_addr = gimple_assign_rhs1 (def_stmt);

  /* If the cond_func_addr is still not an ADDR_EXPR, it means that the
     feature-test function is specified as a pointer.  In this case, we
     return NULL, since the feature-test function decl is not known. */

  if (cond_func_addr == NULL
      || TREE_CODE (cond_func_addr) != ADDR_EXPR)
    return NULL;

  /* If the operand of the ADDR_EXPR is not a function_decl, return NULL
     as this still means the feature-test function is specified as a
     function pointer. */

  if (TREE_CODE (TREE_OPERAND (cond_func_addr, 0)) != FUNCTION_DECL)
    return NULL;

  return cond_func_addr;
}

/* Finds the gimple calls  to __builtin_dispatch in function pointed
   to by the call graph NODE and populates the vector VEC.  Returns
   true if at least one statement was found where the feature test
   function is marked as "version_selector".  Otherwise, there is no
   question of hoisting it. */

static bool
is_builtin_dispatch_stmt_present (struct cgraph_node *node,
			          VEC (tree,heap) **vec)
{
  struct cgraph_edge *edge;
  bool present = false;

  gcc_assert (!flag_lto);

  for (edge = node->callees; edge; edge = edge->next_callee)
    {
      if (edge->callee->decl == builtin_function_decl)
        {
	  tree cond_func_decl;
	  tree cond_func_addr;
	  gcc_assert (*vec != NULL);
	  cond_func_addr = find_version_selector_func_addr (edge->call_stmt);

	  if (cond_func_addr == NULL)
            continue;

	  cond_func_decl = TREE_OPERAND (cond_func_addr, 0);

	  /* Do not consider for hoisting if "version_selector" attribute is
	     not set. */
	  if (lookup_attribute ("version_selector",
			        DECL_ATTRIBUTES (cond_func_decl)) == NULL)
            {
              if (dump_file)
                {
                  fprintf (dump_file, "Not hoisting builtin_dispatch as "
                           "feature_test function not version_selector :\n");
                  print_gimple_stmt (dump_file, edge->call_stmt, 0, TDF_VOPS);
		}
              continue;
            }

	  present = true;
	  VEC_safe_push (tree, heap, *vec, cond_func_decl);
        }
    }
  return present;
}

/* Updates the list of feature-test function decls reaching the cgraph
   function NODE. */

static void
update_reachable_decls_list (struct cgraph_node *node,
                             VEC (tree, heap) *predicate_decls)
{
  VEC (tree, heap) **decl_list = NULL;
  tree cond_func_decl;
  int ix;

  if (node->aux == NULL)
    {
      decl_list = (VEC (tree, heap) **) xmalloc (sizeof (VEC (tree, heap) *));
      *decl_list = VEC_alloc (tree, heap, 1);
      node->aux = decl_list;
    }
  else
    decl_list = (VEC (tree, heap) **) node->aux;

  for (ix = 0; VEC_iterate (tree, predicate_decls, ix, cond_func_decl); ++ix)
    VEC_safe_push (tree, heap, *decl_list, cond_func_decl);
}

/* Propagate the __builtin_dispatch stmt (s) called from node to its
   callers, PREDICATE_DECLS is the decls list of the predicate functions. */

static unsigned int
mark_reachable_functions (struct cgraph_node *this_node,
			  VEC (tree, heap) *predicate_decls)
{
  VEC (NODEPTR, heap) *work_list;
  VEC (int, heap) *depth_list;
  struct cgraph_edge *e;
  htab_t node_htab = NULL;
  void **slot = NULL;

  /* Use a work-list style algorithm to mark functions in any call-graph
     path to the current function. */

  work_list = VEC_alloc (NODEPTR, heap, 8);
  depth_list = VEC_alloc (int, heap, 8);

  VEC_safe_push (NODEPTR, heap, work_list, this_node);
  VEC_safe_push (int, heap, depth_list, 0);

  node_htab = htab_create (10, htab_hash_pointer,
  			   htab_eq_pointer, NULL);

  slot = htab_find_slot (node_htab, this_node, INSERT);

  gcc_assert (*slot == NULL);
  *slot = this_node;

  while (!VEC_empty (NODEPTR, work_list))
    {
      struct cgraph_node *node = VEC_pop (NODEPTR, work_list);
      int depth = VEC_pop (int, depth_list);

      if (dump_file)
        fprintf (dump_file, "%s has a depth = %d callgraph path to %s\n",
                 cgraph_node_name (node), depth,
                 cgraph_node_name (this_node));

      update_reachable_decls_list (node, predicate_decls);

      gcc_assert (node->aux != NULL);

      if (depth >= PARAM_VALUE (PARAM_MVERSN_CLONE_CGRAPH_DEPTH))
        {
          if (dump_file)
            fprintf (dump_file, "Not propogating __builtin_dispatch... "
                     "maximum cloning depth  = %d reached\n",
		     PARAM_VALUE (PARAM_MVERSN_CLONE_CGRAPH_DEPTH));
          continue;
        }

      for (e = node->callers; e; e = e->next_caller)
        {
          slot = htab_find_slot (node_htab, e->caller, INSERT);
	  if (*slot != NULL)
	    continue;
          *slot = e->caller;
          if (!hot_function_p (e->caller))
            continue;

          VEC_safe_push (NODEPTR, heap, work_list, e->caller);
          VEC_safe_push (int, heap, depth_list, (depth + 1));
        }
    }

  htab_delete (node_htab);
  VEC_free (NODEPTR, heap, work_list);
  VEC_free (int, heap, depth_list);
  return 0;
}

/* Scan the call graph and detect hot functions that have __builtin_dispatch
   calls. Then, propogate this information to its callers. Returns true if
   a suitable __builtin_dispatch was found. */

static bool
perform_analysis_phase (void)
{
  struct cgraph_node *node;
  VEC(tree, heap) *builtin_predicates_vec = NULL;
  bool flag = false;

  builtin_predicates_vec = VEC_alloc (tree, heap, 1);

  for (node = cgraph_nodes; node; node = node->next)
    {
      /* if the body of this decl is from outside, do nothing. */
      if (DECL_EXTERNAL (node->decl))
        continue;

      if (!hot_function_p (node))
        continue;

      if (!is_builtin_dispatch_stmt_present (node, &builtin_predicates_vec))
        continue;

      if (dump_file)
        {
          fprintf (dump_file, "%s calls __builtin_dispatch atleast once.\n",
                   cgraph_node_name (node));

          fprintf (dump_file, "%s is a hot function, consider cloning ...\n",
        	   cgraph_node_name (node));
        }

      flag = true;
      mark_reachable_functions (node, builtin_predicates_vec);
      VEC_truncate (tree, builtin_predicates_vec, 0);
    }

  VEC_free (tree, heap, builtin_predicates_vec);
  return flag;
}

/* End Analysis phase. */

/* Decide Cloning Phase.

   In this phase, we go through each function and decide if it should be
   cloned or not. */

/* This function counts the number of unique decls in the DECL_LIST.*/

static int
count_predicate_functions (VEC (tree,heap) *decl_list)
{
  int ix;
  int count = 0;
  tree cond_func_decl = NULL;
  htab_t dup_decl_htab = NULL;

  if (VEC_length (tree, decl_list) == 1)
    return 1;

  dup_decl_htab = htab_create (2, htab_hash_pointer, htab_eq_pointer, NULL);

  for (ix = 0; VEC_iterate (tree, decl_list, ix, cond_func_decl); ++ix)
    {
      void **slot = NULL;
      slot = htab_find_slot (dup_decl_htab, cond_func_decl, INSERT);

      if (*slot != NULL)
        continue;
      count++;
      *slot = cond_func_decl;
    }

  htab_delete (dup_decl_htab);
  return count;
}

/* This function decides which functions to clone based on the number of
   feature_test decls reaching it.  Currently, only one feature_test decl
   is allowed. */

static bool
decide_cloning_phase (void)
{
  struct cgraph_node *node;
  int count;
  bool run_cloning_phase = false;
  int num_funcs_cloned = 0;

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree cond_func_decl = NULL;
      VEC (tree, heap) *vec;
      if (node->aux == NULL)
        continue;

      if (num_funcs_cloned >= PARAM_VALUE (PARAM_NUMBER_OF_MVERSN_CLONES))
        {
          if (dump_file)
            fprintf (dump_file, "Reached cloning limit specified "
                     "by \"num-mversn-clones\" for %s\n",
	             cgraph_node_name (node));

          free (node->aux);
	  node->aux = NULL;
	  continue;
        }

      vec = *(VEC (tree,heap) **) node->aux;
      count = count_predicate_functions (vec);
      gcc_assert (count >= 1);
      cond_func_decl = VEC_index (tree, vec, 0);
      gcc_assert (cond_func_decl != NULL);
      VEC_free (tree, heap, vec);
      free (node->aux);
      node->aux = NULL;

      if (count > 1)
        {
          if (dump_file)
            fprintf (dump_file, "%s has %d predicates, Not cloning for > 1\n",
	             cgraph_node_name (node), count);
	  continue;
        }
      /* Set the node's aux value to be that of the predicate decl. */
      node->aux = cond_func_decl;
      run_cloning_phase = true;
      num_funcs_cloned++;
    }
  return run_cloning_phase;
}

/* End Decide Cloning Phase. */

/* Cloning Phase. */

/* Deletes all basic-blocks and leaves function with :
   ENTRY_BLOCK ---> (new empty basic block) ---> EXIT_BLOCK
*/

static basic_block
empty_function_body (tree fndecl)
{
  basic_block bb, new_bb;
  edge e;
  tree old_current_function_decl;

  old_current_function_decl = current_function_decl;
  push_cfun (DECL_STRUCT_FUNCTION (fndecl));
  current_function_decl = fndecl;

  clear_edges ();
  for (bb = ENTRY_BLOCK_PTR; bb != NULL;)
    {
      basic_block bb_next;
      bb_next = bb->next_bb;
      if (bb != EXIT_BLOCK_PTR
          && bb != ENTRY_BLOCK_PTR)
        {
          if (bb_seq (bb) != NULL)
            {
              gimple_stmt_iterator i;
              for (i = gsi_start_bb (bb); !gsi_end_p (i);)
	        {
		  gimple stmt = gsi_stmt (i);
		  unlink_stmt_vdef (stmt);
                  gsi_remove (&i, true);
		  release_defs (stmt);
		}
            }
          bb->il.gimple = NULL;
          bb->prev_bb = NULL;
          bb->next_bb = NULL;
          SET_BASIC_BLOCK (bb->index, NULL);
          n_basic_blocks--;
        }
      bb = bb_next;
    }
  ENTRY_BLOCK_PTR->next_bb = EXIT_BLOCK_PTR;
  new_bb = create_empty_bb (ENTRY_BLOCK_PTR);
  e = make_edge (ENTRY_BLOCK_PTR, new_bb, EDGE_FALLTHRU);
  gcc_assert (e != NULL);
  /* XXX:Is this edge necessary ? */
  e = make_edge (new_bb, EXIT_BLOCK_PTR, 0);
  gcc_assert (e != NULL);

  if (dump_file)
    dump_function_to_file (current_function_decl, dump_file, TDF_BLOCKS);

  current_function_decl = old_current_function_decl;
  pop_cfun ();
  return new_bb;
}

/* Takes function with decl ORIG_FNDECL and clones it.  The
   name of the clone is the original name suffixed with
   NAME_SUFFIX.  Code is adapted from cgraph_function_versioning
   in cgraphunit.c */

static tree
clone_function (tree orig_fndecl, const char *name_suffix)
{
  tree new_decl;
  char *new_name;
  struct cgraph_node *new_version;
  struct cgraph_node *old_version;
  void **slot;
  tree old_current_function_decl;

  new_name = make_name (orig_fndecl, name_suffix);
  new_decl = copy_node (orig_fndecl);


  slot = htab_find_slot_with_hash (name_decl_htab, new_name,
                                   htab_hash_string (new_name), INSERT);

  gcc_assert (*slot == NULL);
  *slot = new_decl;

  /* Code adapted from cgraph_function_versioning in cgraphuinit.c */

  new_version = cgraph_node (new_decl);
  old_version = cgraph_node (orig_fndecl);

  new_version->local = old_version->local;
  new_version->global = old_version->global;
  new_version->rtl = old_version->rtl;
  new_version->reachable = true;
  new_version->count = old_version->count;

  /* Set the name of the new function. */
  DECL_NAME (new_decl) = get_identifier (new_name);
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  tree_function_versioning (orig_fndecl, new_decl, NULL /*tree_map*/,
                            false, NULL /*args_to_skip*/,
			    NULL /* blocks_to_copy */ ,
                            NULL /* new_entry */);


  old_current_function_decl = current_function_decl;
  push_cfun (DECL_STRUCT_FUNCTION (new_decl));
  current_function_decl = new_decl;

  TREE_READONLY (new_decl) = TREE_READONLY (orig_fndecl);
  TREE_STATIC (new_decl) = TREE_STATIC (orig_fndecl);
  TREE_USED (new_decl) = TREE_USED (orig_fndecl);
  DECL_ARTIFICIAL (new_decl) = 1;
  DECL_IGNORED_P (new_decl) = 0;
  TREE_PUBLIC (new_decl) = TREE_PUBLIC (orig_fndecl);
  DECL_CONTEXT (new_decl) = DECL_CONTEXT (orig_fndecl);

  DECL_EXTERNAL (new_decl) = DECL_EXTERNAL (orig_fndecl);
  DECL_COMDAT (new_decl) = DECL_COMDAT (orig_fndecl);
  DECL_COMDAT_GROUP (new_decl) = DECL_COMDAT_GROUP (orig_fndecl);
  DECL_VIRTUAL_P (new_decl) = DECL_VIRTUAL_P (orig_fndecl);
  DECL_WEAK (new_decl) = DECL_WEAK (orig_fndecl);

  /* Always inline the clones. Why are we cloning otherwise? */
  DECL_DECLARED_INLINE_P (new_decl) = 1;
  DECL_UNINLINABLE (new_decl) = 0;
  new_version->local.externally_visible
  = old_version->local.externally_visible;
  new_version->local.local
  = old_version->local.local;

  new_version->analyzed = true;
  new_version->lowered = true;

  if (dump_file)
    dump_function_to_file (new_decl, dump_file, TDF_BLOCKS);

  cgraph_add_new_function (new_decl, true);

  cgraph_call_function_insertion_hooks (new_version);
  cgraph_mark_needed_node (new_version);

  
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS); 
  calculate_dominance_info (CDI_POST_DOMINATORS);

  pop_cfun ();
  current_function_decl = old_current_function_decl;

  return new_decl;
}

/* This function populates the vector *VEC with the args in the gimple
   call statement STMT. SKIP_ARGS is the number of args to skip.*/

static void
get_function_args (gimple stmt, int num_args, VEC (tree, heap) **vec,
                   int skip_args)
{
  int i;

  if (num_args == 0) return;

  *vec = VEC_alloc (tree, heap, num_args);
  /* The number of args in a function is 1 plus the actual number of
     args.  Also, there are 3 special args reserved, so the first arg
     starts from 3. */
  for (i = 0; i <= num_args - 2; ++i)
    VEC_quick_push (tree, *vec, gimple_call_arg (stmt, (skip_args + i)));
}

/* Given ret = __builtin_dispatch (pred, fn1, fn2, arg1, ....)
   get ret = fn1 (arg1, ...) or ret = fn2 (arg1, ....)
   depending on the value of SIDE == 0 or 1. */

static gimple
make_specialized_call_from_builtin (gimple builtin_stmt, int side)
{
  tree func_addr;
  int num_func_args = 0;
  VEC (tree, heap) *nargs = NULL;
  tree lhs_stmt;
  gimple specialized_call_stmt;

  if (side == 0)
    func_addr = gimple_call_arg (builtin_stmt, 1);
  else
    func_addr = gimple_call_arg (builtin_stmt, 2);

  num_func_args
    =  function_args_count (TREE_TYPE (TREE_OPERAND (func_addr, 0)));

  get_function_args (builtin_stmt, num_func_args, &nargs, 3);

  specialized_call_stmt = gimple_build_call_vec (func_addr, nargs);

  lhs_stmt = gimple_call_lhs (builtin_stmt);

  if (lhs_stmt != NULL_TREE)
    gimple_call_set_lhs (specialized_call_stmt, lhs_stmt);

  if (nargs != NULL)
    VEC_free (tree, heap, nargs);

  return specialized_call_stmt;
}

/* Given a call (GENERIC_STMT) to a function that is cloned, substitute
   with a call to the correct clone. */

static gimple
make_specialized_call_to_clone (gimple generic_stmt, int side)
{
  tree new_decl;
  char *new_name;
  tree generic_fndecl;
  gimple specialized_call_stmt;
  void **slot;
  int num_func_args;
  tree lhs_stmt;
  VEC (tree, heap) *nargs= NULL;

  generic_fndecl = gimple_call_fndecl (generic_stmt);
  gcc_assert (generic_fndecl != NULL);

  if (side == 0)
    new_name = make_name (generic_fndecl, "clone.0");
  else
    new_name = make_name (generic_fndecl, "clone.1");

  slot = htab_find_slot_with_hash (name_decl_htab, new_name,
                                   htab_hash_string (new_name), NO_INSERT);
  gcc_assert (slot != NULL);
  new_decl = (tree) *slot;
  gcc_assert (new_decl);

  num_func_args = function_args_count (TREE_TYPE (generic_fndecl));
  get_function_args (generic_stmt, num_func_args, &nargs, 0);
  specialized_call_stmt = gimple_build_call_vec (new_decl, nargs);

  lhs_stmt = gimple_call_lhs (generic_stmt);

  if (lhs_stmt != NULL_TREE)
    gimple_call_set_lhs (specialized_call_stmt, lhs_stmt);

  if (nargs != NULL)
    VEC_free (tree, heap, nargs);

  return specialized_call_stmt;
}

/* Returns true if STMT is a call to __builtin_dispatch and its
   predicate feature-test function is marked with attribute
   "version_selector". */

static bool
is_builtin_with_predicate_version_selector (gimple stmt)
{
  tree cond_func_addr, cond_func_decl;

  gcc_assert (!flag_lto);
 
  if (gimple_call_fndecl (stmt) != builtin_function_decl)
    return false;

  cond_func_addr = find_version_selector_func_addr (stmt);

  if (cond_func_addr == NULL)
    return false;

  cond_func_decl = TREE_OPERAND (cond_func_addr, 0);
  if (lookup_attribute ("version_selector",
			DECL_ATTRIBUTES (cond_func_decl)) != NULL)
    return true;

  return false;
}

/* Find calls to __builtin_dispatch or to functions that are versioned
   in CLONE_DECL and substitute the call with the correct version based
   on the value of SIDE. */

static void
specialize_call (tree clone_decl, int side)
{
  basic_block bb;
  tree old_current_function_decl;

  old_current_function_decl = current_function_decl;
  push_cfun (DECL_STRUCT_FUNCTION (clone_decl));
  current_function_decl = clone_decl;

  /* Iterate over call edges and find out if there is
     a call to __builtin_dispatch or a cloned function.
     We cannot iterate over call graph edges as there are
     no edges for the clones yet. */

  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (clone_decl))
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
	  tree callee_decl;
	  struct cgraph_node *callee_node;
	  gimple specialized_call_stmt;
	  gimple stmt = gsi_stmt (gsi);

	  if (!is_gimple_call (stmt))
            continue;

	  callee_decl = gimple_call_fndecl (stmt);

	  if (callee_decl == NULL)
            continue;

	  callee_node = cgraph_node (callee_decl);

	  /* For a __builtin_dispatch stmt, only specialize if
             version_selector attribute is set. Otherwise, it is
             not hoisted, so no specialization. */

	  if (is_builtin_with_predicate_version_selector (stmt))
	    {
	      specialized_call_stmt =
	        make_specialized_call_from_builtin (stmt, side);
	    }
	  else if  (callee_node->aux != NULL)
            {
	      specialized_call_stmt =
	        make_specialized_call_to_clone (stmt, side);
            }
	  else
	    continue;

          if (dump_file)
            {
	      fprintf (dump_file, "Specialize stmt : \n");
              print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
	      fprintf (dump_file, "Specialized stmt : \n");
              print_gimple_stmt (dump_file, specialized_call_stmt,
			         0, TDF_VOPS);
            }

	  gimple_set_block (specialized_call_stmt, gimple_block (stmt));
	  gsi_insert_before_without_update (&gsi, specialized_call_stmt,
                                            GSI_SAME_STMT);

	  
          unlink_stmt_vdef (stmt);
	  gsi_remove (&gsi, true); 
	  mark_symbols_for_renaming (specialized_call_stmt);

	  /* After removing make sure gsi is set correctly to not skip
	     a statememt. */
          gsi = gsi_for_stmt (specialized_call_stmt);
	}
    }
  current_function_decl = old_current_function_decl;
  pop_cfun ();
}

/* When a function is version cloned, its body is replaced to call one
   of the versions with the feature-test function acting as a predicate.
   This is done with __builtin_dispatch which is later expanded. */

static gimple
make_builtin_call_to_clones (tree orig_fndecl, tree clone_0_addr,
                             tree clone_1_addr, tree cond_func_addr)
{
  gimple new_builtin_call;
  VEC(tree, heap) *vargs = VEC_alloc (tree, heap, 4);
  tree arg;

  VEC_quick_push (tree, vargs, cond_func_addr);
  VEC_quick_push (tree, vargs, clone_0_addr);
  VEC_quick_push (tree, vargs, clone_1_addr);

  for (arg = DECL_ARGUMENTS (orig_fndecl); arg; arg = TREE_CHAIN (arg))
    {
      VEC_safe_push (tree, heap, vargs, arg);
      /* Again, this add_referenced_var is very very important.  It broke
         a build where a cloned function's arguments where never
         referenced.  Missing this statement in places asserts at
         tree-dfa.c:589, in function referenced_var_lookup at
         "gcc_assert (h || uid == 0);" and is very difficult to triage. */
      add_referenced_var (arg);
    }

  new_builtin_call = gimple_build_call_vec (builtin_function_decl, vargs);
  mark_symbols_for_renaming (new_builtin_call);


  if (dump_file)
    print_gimple_stmt (dump_file, new_builtin_call, 0, TDF_VOPS);

  VEC_free (tree, heap, vargs);

  return new_builtin_call;
}

/* This clones a dispatch function whose callee-graph path has a function
   which calls __builtin_dispatch.  This function is cloned and the
   original function branches to the right clone. */

static int
clone_and_dispatch_function (struct cgraph_node *orig_node, tree *clone_0,
			     tree *clone_1)
{
  tree clone_0_decl, clone_1_decl;
  gimple new_builtin_call = NULL;
  gimple new_return_stmt = NULL;
  gimple_seq seq = NULL;
  basic_block new_bb;
  tree orig_fndecl;
  tree return_var = NULL;
  tree return_type;
  tree old_current_function_decl;

  old_current_function_decl = current_function_decl;
  orig_fndecl = orig_node->decl;
  push_cfun (DECL_STRUCT_FUNCTION (orig_fndecl));
  current_function_decl = orig_fndecl;

  /* Make 2 clones for true and false function. */
  clone_0_decl = clone_function (orig_fndecl, "clone.0");
  clone_1_decl = clone_function (orig_fndecl, "clone.1");
  *clone_0 = clone_0_decl;
  *clone_1 = clone_1_decl;

  new_bb = empty_function_body (orig_fndecl);

  new_builtin_call = make_builtin_call_to_clones (
		       orig_fndecl,
                       build_fold_addr_expr (clone_0_decl),
		       build_fold_addr_expr (clone_1_decl),
		       build_fold_addr_expr ((tree)orig_node->aux));

  return_type = TREE_TYPE (TREE_TYPE (orig_fndecl));

  if (!TREE_ADDRESSABLE (return_type) && COMPLETE_TYPE_P (return_type))
    {
      tree tmp_var;
      tmp_var = create_tmp_var (return_type, NULL);
      add_referenced_var (tmp_var);
      return_var = make_ssa_name (tmp_var, new_builtin_call);
      gimple_call_set_lhs (new_builtin_call, return_var);
    }

  mark_symbols_for_renaming (new_builtin_call);
  new_return_stmt = gimple_build_return (return_var);
  mark_symbols_for_renaming (new_return_stmt);
  gimple_seq_add_stmt (&seq, new_builtin_call);
  gimple_seq_add_stmt (&seq, new_return_stmt);
  set_bb_seq (new_bb, seq);
  gimple_set_bb (new_builtin_call, new_bb);
  gimple_set_bb (new_return_stmt, new_bb);

  gimple_set_block (new_builtin_call, DECL_INITIAL (orig_fndecl));
  gimple_set_block (new_return_stmt, DECL_INITIAL (orig_fndecl));

  if (dump_file)
    dump_function_to_file (orig_fndecl, dump_file, TDF_BLOCKS);

  /* This update_ssa is necessary here for the following reason.  SSA uses
     a global syms_to_rename bitmap that stores syms that must be renamed.
     So, if we accumulate the syms from one function in IPA but move to
     a different function without updating SSA, then we could be
     accumulating syms from many functions.  This would assert in
     referenced_var_lookup because the hashtab storing the syms is
     function local. This is horrible. gcc-4.6 makes this bitmap a
     global. */
  update_ssa (TODO_update_ssa);

  compute_inline_parameters (cgraph_node (orig_fndecl));
  DECL_DECLARED_INLINE_P (orig_fndecl) = 1;
  DECL_UNINLINABLE (orig_fndecl) = 0;
  current_function_decl = old_current_function_decl;
  pop_cfun ();
  return 0;
}

/* Clone all functions marked for cloning by the earlier phase. */

static void
perform_cloning_phase (void)
{
  struct cgraph_node *node;
  int ix;
  VEC (tree, heap) *cloned_decl_list = NULL;
  tree cloned_decl = NULL;

  cloned_decl_list = VEC_alloc (tree, heap, 2);

  /* First clone, then specialize the clones. */
  for (node = cgraph_nodes; node; node = node->next)
    {
      tree clone_0_decl, clone_1_decl;
      if (node->aux == NULL)
        continue;
      if (dump_file)
      {
        fprintf (dump_file, "%s will be cloned\n", cgraph_node_name (node));
        dump_function_to_file (node->decl, dump_file, TDF_BLOCKS);
      }
      clone_and_dispatch_function (node, &clone_0_decl, &clone_1_decl);
      VEC_safe_push (tree, heap, cloned_decl_list, clone_0_decl);
      VEC_safe_push (tree, heap, cloned_decl_list, clone_1_decl);
      continue;
    }

  /* Specialize the clones now. */
  for (ix = 0; VEC_iterate (tree, cloned_decl_list, ix, cloned_decl); ++ix)
    {
      int which_clone = ix % 2;
      specialize_call (cloned_decl, which_clone);
    }

  VEC_free (tree, heap, cloned_decl_list);
}

/* End Cloning phase. */

/* Checks if there is atleast one call to __builtin_dispatch. */

static bool
find_builtin_decl (void)
{
  struct cgraph_node *node;
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (strstr (cgraph_node_name (node), "__builtin_dispatch") != NULL)
        {
          builtin_function_decl = node->decl;
          return true;
        }
    }
  return false;
}

/* Set the aux fields of all nodes and edges in the call graph to be NULL. */

static void
cleanup_aux_field (void)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;

  for (node = cgraph_nodes; node; node = node->next)
    {
      node->aux = NULL;
      for (edge = node->callees; edge; edge = edge->next_callee)
        edge->aux = NULL;
    }
}

/* Main driver function. It scans the __builtin_dispatch calls and
   figures out which functions to clone.  It then clones the functions. */

static unsigned int
builtin_dispatch_ipa_clone (void)
{
  cleanup_aux_field ();

  /* Allocate hashtab mapping name to decl. */
  name_decl_htab = htab_create (10, name_decl_htab_hash_descriptor,
			        name_decl_htab_eq_descriptor, NULL);

  /* Turn it on for O1 and above.  At -O0, there is a SSA alias bug
     with create_tmp_var.  Cloning and hoisting is not necessary at
     -O0 anyways.  Also, guard it with the flag
     "-fclone-hot-version-paths". 
     Disabled for LTO as it needs more work. */
  if (optimize == 0
      || profile_arc_flag
      || !flag_clone_hot_version_paths
      || flag_lto)
    return 0;

  if (!find_builtin_decl ())
    return 0;

  gcc_assert (builtin_function_decl != NULL);

  if (!perform_analysis_phase ())
    {
      cleanup_aux_field ();
      return 0;
    }

  if (decide_cloning_phase ())
    perform_cloning_phase ();

  cleanup_aux_field ();

  return 0;
}

static bool
gate_handle_builtin_dispatch (void)
{
  return true;
}

struct simple_ipa_opt_pass pass_ipa_multiversion_dispatch =
{
 {
  SIMPLE_IPA_PASS,
  "multiversion_dispatch",		/* name */
  gate_handle_builtin_dispatch,		/* gate */
  builtin_dispatch_ipa_clone,           /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_MVERSN_DISPATCH,			/* tv_id */
  0,	                                /* properties_required */
  PROP_cfg,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func |			/* todo_flags_finish */
  TODO_update_ssa
 }
};

/* Lowering of the __builtin_dispatch calls. */


/* This function converts STMT which is a __builtin_dispatch
   call of the form :
   ret = __builtin_dispatch (predicate, foo, bar, arg1, ...)
   into :
   var_1 = predicate
   if  (var_1)
     var_2 = foo (arg1, ...);
   else
     var_3 = bar (arg1, ...);
   var_4 = phi (var_2, var_3)
   ret = var_4

   var_? are ssa names for variable var.
*/

static unsigned int
convert_builtin_dispatch (gimple stmt)
{
  tree cond_func_addr, if_func_addr, else_func_addr;
  tree cond_func_decl = NULL;
  gimple if_part, else_part, if_else_stmt;
  basic_block bb1, bb2, bb3, bb4;
  gimple bb1end, bb2end, bb3end;
  edge e12, e13, e23, e24, e34;
  VEC(tree, heap) *nargs = NULL;
  int num_func_args = 0, i;
  tree version_selector_var;
  tree lhs_result;
  gimple_stmt_iterator gsi;
  basic_block bb;
  gimple feature_test_call = NULL;
  tree tmp_var = NULL;
  gimple init_stmt = NULL;
  tree ssa_if_name, ssa_else_name;
  gimple phinode = NULL;
  tree tmp_result_var, ssa_result_var;

  gsi = gsi_for_stmt (stmt);
  bb = gsi_bb (gsi);

  cond_func_addr = find_version_selector_func_addr (stmt);
  if (cond_func_addr != NULL)
    {
      cond_func_decl = TREE_OPERAND (cond_func_addr, 0);
      gcc_assert (cond_func_decl);
    }

  if (dump_file)
    {
      fprintf (dump_file, "Before Converting __builtin_dispatch :\n");
      dump_function_to_file (current_function_decl, dump_file, TDF_BLOCKS);
    }

  if_func_addr = gimple_call_arg (stmt, 1);
  else_func_addr = gimple_call_arg (stmt, 2);

  tmp_result_var = create_tmp_var (integer_type_node, NULL);
  add_referenced_var (tmp_result_var);

  if (flag_lto
      || cond_func_decl == NULL
      || lookup_attribute ("version_selector",
                           DECL_ATTRIBUTES (cond_func_decl)) == NULL)
    {
      tree arg = gimple_call_arg (stmt, 0);
      /* This means the feature-test function is not set with attribute
         version_selector or it is a function pointer or in LTO. So,
         explicitly call it. */
      feature_test_call = gimple_build_call (arg, 0);
      ssa_result_var = make_ssa_name (tmp_result_var, feature_test_call);
      gimple_call_set_lhs (feature_test_call, ssa_result_var);
      mark_symbols_for_renaming (feature_test_call);
      version_selector_var = ssa_result_var;
    }
  else
    {
      /* Get the global corresponding to the "version_selector" function. */
      version_selector_var
        = handle_version_selector_attr_function (cond_func_decl);
      gcc_assert (version_selector_var);
      add_referenced_var (version_selector_var);
      feature_test_call = gimple_build_assign (tmp_result_var,
                                               version_selector_var);
      ssa_result_var = make_ssa_name (tmp_result_var, feature_test_call);
      gimple_assign_set_lhs (feature_test_call, ssa_result_var);
      mark_symbols_for_renaming (feature_test_call);
      version_selector_var = ssa_result_var;
    }

  if_else_stmt = gimple_build_cond (GT_EXPR,
                                    version_selector_var,
                                    integer_zero_node,
                                    NULL_TREE, NULL_TREE);

  mark_symbols_for_renaming (if_else_stmt);

  num_func_args =  function_args_count (
    TREE_TYPE (TREE_OPERAND (if_func_addr, 0)));

  nargs = VEC_alloc (tree, heap, num_func_args);

  /* The arguments to the feature test function start from the 4th argument
     in __builtin_dispatch.  The first 3 arguments are mandatory. */

  for (i = 0; i <= num_func_args - 2; ++i)
    VEC_quick_push (tree, nargs,
                    gimple_call_arg (stmt, (3 + i)));

  if_part = gimple_build_call_vec (if_func_addr, nargs);
  else_part = gimple_build_call_vec (else_func_addr, nargs);

  lhs_result = gimple_call_lhs (stmt);

  if (lhs_result != NULL_TREE)
    {
      tree ssa_var;
      tree return_type;
      return_type = TREE_TYPE (lhs_result);
      tmp_var = create_tmp_var (return_type, NULL);
      add_referenced_var (tmp_var);

      init_stmt = gimple_build_assign (tmp_var, build_zero_cst (return_type));
      ssa_var = make_ssa_name (tmp_var, init_stmt);
      gimple_assign_set_lhs (init_stmt, ssa_var);
      mark_symbols_for_renaming (init_stmt);

      ssa_if_name = make_ssa_name (tmp_var, init_stmt);
      ssa_else_name = make_ssa_name (tmp_var, init_stmt);
      gimple_call_set_lhs (if_part, ssa_if_name);
      gimple_call_set_lhs (else_part, ssa_else_name);
    }
  mark_symbols_for_renaming (if_part);
  mark_symbols_for_renaming (else_part);

  /* Set the lexical block to be the same as the dispatch call. */
  gcc_assert (feature_test_call);
  gimple_set_block (feature_test_call, gimple_block (stmt));

  if (init_stmt)
    gimple_set_block (init_stmt, gimple_block (stmt));

  gimple_set_block (if_else_stmt, gimple_block (stmt));
  gimple_set_block (if_part, gimple_block (stmt));
  gimple_set_block (else_part, gimple_block (stmt));

  gsi_insert_before_without_update (&gsi, feature_test_call, GSI_SAME_STMT);
  gimple_set_bb (feature_test_call, bb);

  if (init_stmt)
    {
      gsi_insert_before_without_update (&gsi, init_stmt,
                                        GSI_SAME_STMT);
      gimple_set_bb (init_stmt, bb);
    }

  gsi_insert_before_without_update (&gsi, if_else_stmt, GSI_SAME_STMT);
  gsi_insert_before_without_update (&gsi, if_part, GSI_SAME_STMT);
  gsi_insert_before_without_update (&gsi, else_part, GSI_SAME_STMT);

  /* Remove the builtin_dispatch call after the expansion. */
  unlink_stmt_vdef (stmt);
  gsi_remove (&gsi, true);

  bb1end = if_else_stmt;
  bb2end = if_part;
  bb3end = else_part;
  bb1 = bb;
  e12 = split_block (bb1, bb1end);
  bb2 = e12->dest;
  e23 = split_block (bb2, bb2end);
  bb3 = e23->dest;
  e34 = split_block (bb3, bb3end);
  bb4 = e34->dest;

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_TRUE_VALUE;
  e13 = make_edge (bb1, bb3, EDGE_FALSE_VALUE);
  gcc_assert (e13);
  e24 = make_edge (bb2, bb4, EDGE_FALLTHRU);
  gcc_assert (e24);
  remove_edge (e23);

  if (tmp_var)
    {
      gimple assign_stmt;
      phinode = create_phi_node (tmp_var, bb4);
      add_phi_arg (phinode, ssa_if_name, e24, UNKNOWN_LOCATION);
      add_phi_arg (phinode, ssa_else_name, e34, UNKNOWN_LOCATION);
      mark_symbols_for_renaming (phinode);
      gcc_assert (lhs_result);
      assign_stmt
        = gimple_build_assign (lhs_result, gimple_phi_result (phinode));
      mark_symbols_for_renaming (assign_stmt);
      gsi = gsi_start_bb (bb4);
      gsi_insert_before_without_update (&gsi, assign_stmt, GSI_SAME_STMT);
      gimple_set_bb (assign_stmt, bb4);
    }

  if (dump_file)
    {
      fprintf (dump_file, "Converted __builtin_dispatch :\n");
      dump_function_to_file (current_function_decl, dump_file, TDF_BLOCKS);
    }

  return 0;
}

/* This function does two things.

   1) For a feature-test function marked with attribute "version_selector",
      it creates a constructor that calls the feature-test function and a
      global that holds the result.  The global's result will be used
      to lower any __builtin_dispatch statement that refers to this feature
      test function.  The __builtin_dispatch statement and the feature test
      function can be in different modules.

   2) It lowers __builtin_dispatch statements. */

static unsigned int
do_convert_builtin_dispatch (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  VEC (gimple, heap) *builtin_stmt_list = NULL;
  int ix;
  gimple builtin_stmt;

  /* Allocate hashtab mapping name to decl. */
  if (name_decl_htab == NULL)
    name_decl_htab = htab_create (10, name_decl_htab_hash_descriptor,
		                  name_decl_htab_eq_descriptor, NULL);

  /* Look for functions with attribute "version_selector" and make a
     constructor which calls the function and saves the result in a
     global.  Disabled for LTO as it needs more work. */

  if (!flag_lto
      && lookup_attribute ("version_selector",
                           DECL_ATTRIBUTES (current_function_decl)) != NULL)
    {
      if (dump_file)
        fprintf (dump_file, "Function with version_selector attribute found :"
                 " %s.  Making constructor for it.\n",
		 current_function_name ());

      handle_version_selector_attr_function (current_function_decl);
      /* Assume there are no __builtin_dispatch calls in feature test
	 functions.  So it is safe to return. */
      return 0;
    }

  /* Find and lower __builtin_dispatch calls. */

  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (current_function_decl))
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
          gimple stmt = gsi_stmt (gsi);
          tree call_decl;

          if (!is_gimple_call (stmt))
            continue;

          call_decl = gimple_call_fndecl (stmt);

	  if (call_decl == NULL)
            continue;

	  if (DECL_NAME (call_decl) == NULL_TREE)
	    continue;

	  if (strstr (IDENTIFIER_POINTER (DECL_NAME (call_decl)),
                      "__builtin_dispatch") == NULL)
            continue;

	  if (dump_file)
            {
	      fprintf (dump_file, "Converting __builtin_dispatch stmt in:%s\n",
		       current_function_name ());
              print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS);
            }

	  if (builtin_stmt_list == NULL)
              builtin_stmt_list = VEC_alloc (gimple, heap, 2);

          gcc_assert (builtin_stmt_list != NULL);
	  VEC_safe_push (gimple, heap, builtin_stmt_list, stmt);
        }
    }

  if (!builtin_stmt_list)
    return 0;
 
  for (ix = 0; VEC_iterate (gimple, builtin_stmt_list, ix, builtin_stmt);
       ++ix)
    convert_builtin_dispatch (builtin_stmt);

  compute_inline_parameters (cgraph_node (current_function_decl));
 
  VEC_free (gimple, heap, builtin_stmt_list); 
  
  return 0;
}

static bool
gate_convert_builtin_dispatch (void)
{
  return true;
}

struct gimple_opt_pass pass_tree_convert_builtin_dispatch =
{
 {
  GIMPLE_PASS,
  "convert_builtin_dispatch",	        /* name */
  gate_convert_builtin_dispatch,	/* gate */
  do_convert_builtin_dispatch,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_MVERSN_DISPATCH,			/* tv_id */
  PROP_cfg,				/* properties_required */
  PROP_cfg,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func |			/* todo_flags_finish */
  TODO_cleanup_cfg | TODO_dump_cgraph |
  TODO_update_ssa | TODO_verify_ssa
 }
};

/* This function generates gimple code in NEW_BB to check if COND_VAR
   is equal to WHICH_VERSION and return FN_VER pointer if it is equal.
   The basic block returned is the block where the control flows if
   the equality is false.  */

static basic_block
make_bb_flow (basic_block new_bb, tree cond_var, tree fn_ver,
	      int which_version, tree bindings)
{
  tree result_var;
  tree convert_expr;

  basic_block bb1, bb2, bb3;
  edge e12, e23;

  gimple if_else_stmt;
  gimple if_stmt;
  gimple return_stmt;
  gimple_seq gseq = bb_seq (new_bb);

  /* Check if the value of cond_var is equal to which_version.  */
  if_else_stmt = gimple_build_cond (EQ_EXPR, cond_var,
				    build_int_cst (NULL, which_version),
				    NULL_TREE, NULL_TREE);

  mark_symbols_for_renaming (if_else_stmt);
  gimple_seq_add_stmt (&gseq, if_else_stmt);
  gimple_set_block (if_else_stmt, bindings);
  gimple_set_bb (if_else_stmt, new_bb);

  result_var = create_tmp_var (ptr_type_node, NULL);
  add_referenced_var (result_var);

  convert_expr = build1 (CONVERT_EXPR, ptr_type_node, fn_ver);
  if_stmt = gimple_build_assign (result_var, convert_expr);
  mark_symbols_for_renaming (if_stmt);
  gimple_seq_add_stmt (&gseq, if_stmt);
  gimple_set_block (if_stmt, bindings);

  return_stmt = gimple_build_return (result_var);
  mark_symbols_for_renaming (return_stmt);
  gimple_seq_add_stmt (&gseq, return_stmt);

  set_bb_seq (new_bb, gseq);

  bb1 = new_bb;
  e12 = split_block (bb1, if_else_stmt);
  bb2 = e12->dest;
  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_TRUE_VALUE;

  e23 = split_block (bb2, return_stmt);
  gimple_set_bb (if_stmt, bb2);
  gimple_set_bb (return_stmt, bb2);
  bb3 = e23->dest;
  make_edge (bb1, bb3, EDGE_FALSE_VALUE); 

  remove_edge (e23);
  make_edge (bb2, EXIT_BLOCK_PTR, 0);

  return bb3;
}

/* Given the pointer to the condition function COND_FUNC_ARG, whose return
   value decides the version that gets executed, and the pointers to the
   function versions, FN_VER_LIST, this function generates control-flow to
   return the appropriate function version pointer based on the return value
   of the conditional function.   The condition function is assumed to return
   values 0, 1, 2, ... */

static gimple_seq
get_selector_gimple_seq (tree cond_func_arg, tree fn_ver_list, tree default_ver,
			 basic_block new_bb, tree bindings)
{
  basic_block final_bb;

  gimple return_stmt, default_stmt;
  gimple_seq gseq = NULL;
  gimple_seq gseq_final = NULL;
  gimple call_cond_stmt;

  tree result_var;
  tree convert_expr;
  tree p;
  tree cond_var;

  int which_version;

  /* Call the condition function once and store the outcome in cond_var.  */
  cond_var = create_tmp_var (integer_type_node, NULL);
  call_cond_stmt = gimple_build_call (cond_func_arg, 0);
  gimple_call_set_lhs (call_cond_stmt, cond_var);
  add_referenced_var (cond_var);
  mark_symbols_for_renaming (call_cond_stmt);

  gimple_seq_add_stmt (&gseq, call_cond_stmt);
  gimple_set_block (call_cond_stmt, bindings);
  gimple_set_bb (call_cond_stmt, new_bb);

  set_bb_seq (new_bb, gseq);

  final_bb = new_bb;

  which_version = 0; 
  for (p = fn_ver_list; p != NULL_TREE; p = TREE_CHAIN (p))
    {
      tree ver = TREE_PURPOSE (p);
      /* Return this version's pointer, VER, if the value returned by the
	 condition funciton is equal to WHICH_VERSION.  */
      final_bb = make_bb_flow (final_bb, cond_var, ver, which_version,
			       bindings);
      which_version++;
    }

  result_var = create_tmp_var (ptr_type_node, NULL);
  add_referenced_var (result_var);

  /* Return the default version function pointer as the default.  */
  convert_expr = build1 (CONVERT_EXPR, ptr_type_node, default_ver);
  default_stmt = gimple_build_assign (result_var, convert_expr);
  mark_symbols_for_renaming (default_stmt);
  gimple_seq_add_stmt (&gseq_final, default_stmt);
  gimple_set_block (default_stmt, bindings);
  gimple_set_bb (default_stmt, final_bb);

  return_stmt = gimple_build_return (result_var);
  mark_symbols_for_renaming (return_stmt);
  gimple_seq_add_stmt (&gseq_final, return_stmt);
  gimple_set_bb (return_stmt, final_bb);

  set_bb_seq (final_bb, gseq_final);

  return gseq; 
}

/* Make the ifunc selector function which calls function pointed to by
   COND_FUNC_ARG and checks the value to return the appropriate function
   version pointer.  */

static tree
make_selector_function (const char *name, tree cond_func_arg,
			tree fn_ver_list, tree default_ver)
{
  tree decl, type, t;
  basic_block new_bb;
  tree old_current_function_decl;
  tree decl_name;

  /* The selector function should return a (void *). */
  type = build_function_type_list (ptr_type_node, NULL_TREE);
 
  decl = build_fn_decl (name, type);

  decl_name = get_identifier (name);
  SET_DECL_ASSEMBLER_NAME (decl, decl_name);
  DECL_NAME (decl) = decl_name;
  gcc_assert (cgraph_node (decl) != NULL);

  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  DECL_STATIC_CONSTRUCTOR (decl) = 0;
  TREE_READONLY (decl) = 0;
  DECL_PURE_P (decl) = 0;
 
  /* Build result decl and add to function_decl. */
  t = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, ptr_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (decl) = t;

  gimplify_function_tree (decl);

  old_current_function_decl = current_function_decl;
  push_cfun (DECL_STRUCT_FUNCTION (decl));
  current_function_decl = decl;
  init_empty_tree_cfg_for_function (DECL_STRUCT_FUNCTION (decl));

  cfun->curr_properties |=
    (PROP_gimple_lcf | PROP_gimple_leh | PROP_cfg | PROP_referenced_vars |
     PROP_ssa);

  new_bb = create_empty_bb (ENTRY_BLOCK_PTR);
  make_edge (ENTRY_BLOCK_PTR, new_bb, EDGE_FALLTHRU);
  make_edge (new_bb, EXIT_BLOCK_PTR, 0);

  /* This call is very important if this pass runs when the IR is in
     SSA form.  It breaks things in strange ways otherwise. */
  init_tree_ssa (DECL_STRUCT_FUNCTION (decl));
  init_ssa_operands ();

  /* Make the body of thr selector function.  */
  get_selector_gimple_seq (cond_func_arg, fn_ver_list, default_ver, new_bb,
			   DECL_INITIAL (decl));

  cgraph_add_new_function (decl, true);
  cgraph_call_function_insertion_hooks (cgraph_node (decl));
  cgraph_mark_needed_node (cgraph_node (decl));

  if (dump_file)
    dump_function_to_file (decl, dump_file, TDF_BLOCKS);

  pop_cfun ();
  current_function_decl = old_current_function_decl;
  return decl;
}

/* Makes a function attribute of the form NAME(ARG_NAME) and chains
   it to CHAIN.  */

static tree
make_attribute (const char *name, const char *arg_name, tree chain)
{
  tree attr_name;
  tree attr_arg_name;
  tree attr_args;
  tree attr;

  attr_name = get_identifier (name);
  attr_arg_name = build_string (strlen (arg_name), arg_name);
  attr_args = tree_cons (NULL_TREE, attr_arg_name, NULL_TREE);
  attr = tree_cons (attr_name, attr_args, chain);
  return attr;
}

/* This creates the ifunc function IFUNC_NAME whose selector function is
   SELECTOR_NAME. */

static tree
make_ifunc_function (const char* ifunc_name, const char *selector_name,
		     tree fn_type)
{
  tree type;
  tree decl;

  /* The signature of the ifunc function is set to the
     type of any version.  */
  type = build_function_type (TREE_TYPE (fn_type), TYPE_ARG_TYPES (fn_type));
  decl = build_fn_decl (ifunc_name, type);

  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = error_mark_node;

  /* Set ifunc attribute */
  DECL_ATTRIBUTES (decl)
    = make_attribute ("ifunc", selector_name, DECL_ATTRIBUTES (decl));

  assemble_alias (decl, get_identifier (selector_name)); 

  return decl;
}

/* Copy the decl attributes from from_decl to to_decl, except
   DECL_ARTIFICIAL and TREE_PUBLIC.  */

static void
copy_decl_attributes (tree to_decl, tree from_decl)
{
  TREE_READONLY (to_decl) = TREE_READONLY (from_decl);
  TREE_USED (to_decl) = TREE_USED (from_decl);
  DECL_ARTIFICIAL (to_decl) = 1;
  DECL_IGNORED_P (to_decl) = DECL_IGNORED_P (from_decl);
  TREE_PUBLIC (to_decl) = 0;
  DECL_CONTEXT (to_decl) = DECL_CONTEXT (from_decl);
  DECL_EXTERNAL (to_decl) = DECL_EXTERNAL (from_decl);
  DECL_COMDAT (to_decl) = DECL_COMDAT (from_decl);
  DECL_COMDAT_GROUP (to_decl) = DECL_COMDAT_GROUP (from_decl);
  DECL_VIRTUAL_P (to_decl) = DECL_VIRTUAL_P (from_decl);
  DECL_WEAK (to_decl) = DECL_WEAK (from_decl);
}

/* This function does the mult-version run-time dispatch using IFUNC.  Given
   NUM_VERSIONS versions of a function with the decls in FN_VER_LIST along
   with a default version in DEFAULT_VER.  Also given is a condition function,
   COND_FUNC_ADDR, whose return value decides the version that gets executed.
   This function generates the necessary code to dispatch the right function
   version and returns this a GIMPLE_SEQ. The decls of the ifunc function and
   the selector function that are created are stored in IFUNC_DECL and
   SELECTOR_DECL.  */

static gimple_seq
dispatch_using_ifunc (int num_versions, tree orig_func_decl,
		      tree cond_func_addr, tree fn_ver_list,
		      tree default_ver, tree *selector_decl,
		      tree *ifunc_decl)
{
  char *selector_name;
  char *ifunc_name;
  tree ifunc_function;
  tree selector_function;
  tree return_type;
  VEC (tree, heap) *nargs = NULL;
  tree arg;
  gimple ifunc_call_stmt;
  gimple return_stmt;
  gimple_seq gseq = NULL;

  gcc_assert (cond_func_addr != NULL
	      && num_versions > 0
	      && orig_func_decl != NULL
	      && fn_ver_list != NULL);

  /* The return type of any function version.  */
  return_type = TREE_TYPE (TREE_TYPE (orig_func_decl));

  nargs = VEC_alloc (tree, heap, 4);

  for (arg = DECL_ARGUMENTS (orig_func_decl);
       arg; arg = TREE_CHAIN (arg))
    {
      VEC_safe_push (tree, heap, nargs, arg);
      add_referenced_var (arg);
    }

  /* Assign names to ifunc and ifunc_selector functions. */
  selector_name = make_name (orig_func_decl, "ifunc.selector");
  ifunc_name = make_name (orig_func_decl, "ifunc");

  /* Make a selector function which returns the appropriate function
     version pointer based on the outcome of the condition function
     execution.  */
  selector_function = make_selector_function (selector_name, cond_func_addr,
					      fn_ver_list, default_ver);
  *selector_decl = selector_function;

  /* Make a new ifunc function.  */
  ifunc_function = make_ifunc_function  (ifunc_name, selector_name,
					 TREE_TYPE (orig_func_decl));
  *ifunc_decl = ifunc_function;

  /* Make selector and ifunc shadow the attributes of the original function.  */
  copy_decl_attributes (ifunc_function, orig_func_decl);
  copy_decl_attributes (selector_function, orig_func_decl);
 
  ifunc_call_stmt = gimple_build_call_vec (ifunc_function, nargs);
  gimple_seq_add_stmt (&gseq, ifunc_call_stmt); 

  /* Make function return the value of it is a non-void type.  */
  if (TREE_CODE (return_type) != VOID_TYPE)
    {
      tree lhs_var;
      tree lhs_var_ssa_name;
      tree result_decl;

      result_decl = DECL_RESULT (orig_func_decl);

      if (result_decl
	  && aggregate_value_p (result_decl, orig_func_decl)
	  && !TREE_ADDRESSABLE (result_decl))
 	{
	  /* Build a RESULT_DECL rather than a VAR_DECL for this case.
	     See tree-nrv.c: tree_nrv. It checks if the DECL_RESULT and the
	     return value are the same.  */
	  lhs_var = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL,
			        return_type);
	  DECL_ARTIFICIAL (lhs_var) = 1;
	  DECL_IGNORED_P (lhs_var) = 1;
	  TREE_READONLY (lhs_var) = 0;
	  DECL_EXTERNAL (lhs_var) = 0;
	  TREE_STATIC (lhs_var) = 0;
	  TREE_USED (lhs_var) = 1;

          add_referenced_var (lhs_var);
          DECL_RESULT (orig_func_decl) = lhs_var;
	}
     else if (!TREE_ADDRESSABLE (return_type)
              && COMPLETE_TYPE_P (return_type))
        {
          lhs_var = create_tmp_var (return_type, NULL);
          add_referenced_var (lhs_var);
        }
      else
	{
          lhs_var = create_tmp_var_raw (return_type, NULL);
	  TREE_ADDRESSABLE (lhs_var) = 1;
	  gimple_add_tmp_var (lhs_var);
          add_referenced_var (lhs_var);
	}

      if (AGGREGATE_TYPE_P (return_type)
	  || TREE_CODE (return_type) == COMPLEX_TYPE)
        {
          gimple_call_set_lhs (ifunc_call_stmt, lhs_var);
          return_stmt = gimple_build_return (lhs_var);
	}
      else
	{
	  lhs_var_ssa_name = make_ssa_name (lhs_var, ifunc_call_stmt);
	  gimple_call_set_lhs (ifunc_call_stmt, lhs_var_ssa_name);
	  return_stmt = gimple_build_return (lhs_var_ssa_name);
	}
    }
  else
    {
      return_stmt = gimple_build_return (NULL_TREE);
    }

  mark_symbols_for_renaming (ifunc_call_stmt);
  mark_symbols_for_renaming (return_stmt);
  gimple_seq_add_stmt (&gseq, return_stmt); 

  VEC_free (tree, heap, nargs);
  return gseq;
}

/* Empty the function body of function fndecl.  Retain just one basic block
   along with the ENTRY and EXIT block.  Return the retained basic block.  */

static basic_block
purge_function_body (tree fndecl)
{
  basic_block bb, new_bb;
  edge first_edge, last_edge;
  tree old_current_function_decl;

  old_current_function_decl = current_function_decl;
  push_cfun (DECL_STRUCT_FUNCTION (fndecl));
  current_function_decl = fndecl;

  /* Set new_bb to be the first block after ENTRY_BLOCK_PTR. */

  first_edge  = VEC_index (edge, ENTRY_BLOCK_PTR->succs, 0);
  new_bb = first_edge->dest;
  gcc_assert (new_bb != NULL);

  for (bb = ENTRY_BLOCK_PTR; bb != NULL;)
    {
      edge_iterator ei;
      edge e;
      basic_block bb_next;
      bb_next = bb->next_bb;
      if (bb == EXIT_BLOCK_PTR)
        VEC_truncate (edge, EXIT_BLOCK_PTR->preds, 0);
      else if (bb == ENTRY_BLOCK_PTR)
        VEC_truncate (edge, ENTRY_BLOCK_PTR->succs, 0);
      else
        {
          remove_phi_nodes (bb);
          if (bb_seq (bb) != NULL)
            {
              gimple_stmt_iterator i;
              for (i = gsi_start_bb (bb); !gsi_end_p (i);)
	        {
		  gimple stmt = gsi_stmt (i);
		  unlink_stmt_vdef (stmt);
		  reset_debug_uses (stmt);
                  gsi_remove (&i, true);
		  release_defs (stmt);
		}
            }
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      n_edges--;
	      ggc_free (e);
	    }
	  VEC_truncate (edge, bb->succs, 0);
	  VEC_truncate (edge, bb->preds, 0);
          bb->prev_bb = NULL;
          bb->next_bb = NULL;
	  if (bb == new_bb)
	    {
	      bb = bb_next;
	      continue;
	    }
          bb->il.gimple = NULL;
          SET_BASIC_BLOCK (bb->index, NULL);
          n_basic_blocks--;
        }
      bb = bb_next;
    }


  /* This is to allow iterating over the basic blocks. */
  new_bb->next_bb = EXIT_BLOCK_PTR;
  EXIT_BLOCK_PTR->prev_bb = new_bb;

  new_bb->prev_bb = ENTRY_BLOCK_PTR;
  ENTRY_BLOCK_PTR->next_bb = new_bb;

  gcc_assert (find_edge (new_bb, EXIT_BLOCK_PTR) == NULL);
  last_edge = make_edge (new_bb, EXIT_BLOCK_PTR, 0);
  gcc_assert (last_edge);

  gcc_assert (find_edge (ENTRY_BLOCK_PTR, new_bb) == NULL);
  last_edge = make_edge (ENTRY_BLOCK_PTR, new_bb, EDGE_FALLTHRU);
  gcc_assert (last_edge);

  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS); 
  calculate_dominance_info (CDI_POST_DOMINATORS);

  current_function_decl = old_current_function_decl;
  pop_cfun ();

  return new_bb;
}

/* Returns true if function FUNC_DECL contains abnormal goto statements.  */

static bool
function_can_make_abnormal_goto (tree func_decl)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (func_decl))
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
	  gimple stmt = gsi_stmt (gsi);
	  if (stmt_can_make_abnormal_goto (stmt))
	    return true;
	}
    }
  return false;
}

/* Has an entry for every cloned function and auxiliaries that have been
   generated by auto cloning.  These cannot be further cloned.  */

htab_t cloned_function_decls_htab = NULL;

/* Adds function FUNC_DECL to the cloned_function_decls_htab.  */

static void
mark_function_not_cloneable (tree func_decl)
{
  void **slot;

  slot = htab_find_slot_with_hash (cloned_function_decls_htab, func_decl,
				   htab_hash_pointer (func_decl), INSERT);
  gcc_assert (*slot == NULL);
  *slot = func_decl;
}

/* Entry point for the auto clone pass.  Calls the target hook to determine if
   this function must be cloned.  */

static unsigned int
do_auto_clone (void)
{
  tree opt_node = NULL_TREE;
  int num_versions = 0;
  int i = 0;
  tree fn_ver_addr_chain = NULL_TREE;
  tree default_ver = NULL_TREE;
  tree cond_func_decl = NULL_TREE;
  tree cond_func_addr;
  tree default_decl;
  basic_block empty_bb;
  gimple_seq gseq = NULL;
  gimple_stmt_iterator gsi;
  tree selector_decl;
  tree ifunc_decl;
  void **slot;
  struct cgraph_node *node;

  node = cgraph_node (current_function_decl);

  if (lookup_attribute ("noclone", DECL_ATTRIBUTES (current_function_decl))
      != NULL)
    {
      if (dump_file)
	fprintf (dump_file, "Not cloning, noclone attribute set\n");
      return 0;
    }

  if (lookup_attribute ("target", DECL_ATTRIBUTES (current_function_decl))
      != NULL)
    {
      if (dump_file)
	fprintf (dump_file, "Not cloning, target attribute set\n");
      return 0;
    }

  /* No cloning of constructors and destructors.  */
  if (DECL_STATIC_CONSTRUCTOR (current_function_decl)
      || DECL_STATIC_DESTRUCTOR (current_function_decl))
    return 0;

  /* Check if function size is within permissible limits for cloning.  */
  if (node->global.size
      > PARAM_VALUE (PARAM_MAX_FUNCTION_SIZE_FOR_AUTO_CLONING))
    {
      if (dump_file)
        fprintf (dump_file, "Function size exceeds auto cloning threshold.\n");
      return 0;
    }

  if (cloned_function_decls_htab == NULL)
    cloned_function_decls_htab = htab_create (10, htab_hash_pointer,
					      htab_eq_pointer, NULL);


  /* If this function is a clone or other, like the selector function, pass.  */
  slot = htab_find_slot_with_hash (cloned_function_decls_htab,
				   current_function_decl,
  			           htab_hash_pointer (current_function_decl),
				   INSERT);

  if (*slot != NULL)
    return 0;

  if (profile_status == PROFILE_READ
      && !hot_function_p (cgraph_node (current_function_decl)))
    return 0;

  /* Ignore functions with abnormal gotos, not correct to clone them.  */
  if (function_can_make_abnormal_goto (current_function_decl))
    return 0;

  if (!targetm.mversion_function)
    return 0;

  /* Call the target hook to see if this function needs to be versioned.  */
  num_versions = targetm.mversion_function (current_function_decl, &opt_node,
					    &cond_func_decl);
      
  /* Nothing more to do if versions are not to be created.  */
  if (num_versions == 0)
    return 0;

  mark_function_not_cloneable (cond_func_decl);
  copy_decl_attributes (cond_func_decl, current_function_decl);

  /* Make as many clones as requested.  */
  for (i = 0; i < num_versions; ++i)
    {
      tree cloned_decl;
      char clone_name[100];

      sprintf (clone_name, "autoclone.%d", i);
      cloned_decl = clone_function (current_function_decl, clone_name);
      fn_ver_addr_chain = tree_cons (build_fold_addr_expr (cloned_decl),
				     NULL, fn_ver_addr_chain);
      gcc_assert (cloned_decl != NULL);
      mark_function_not_cloneable (cloned_decl);
      DECL_FUNCTION_SPECIFIC_TARGET (cloned_decl)
        = TREE_PURPOSE (opt_node);
      opt_node = TREE_CHAIN (opt_node);
    }

  /* The current function is replaced by an ifunc call to the right version.
     Make another clone for the default.  */
  default_decl = clone_function (current_function_decl, "autoclone.original");
  mark_function_not_cloneable (default_decl);
  /* Empty the body of the current function.  */
  empty_bb = purge_function_body (current_function_decl);
  default_ver = build_fold_addr_expr (default_decl);
  cond_func_addr = build_fold_addr_expr (cond_func_decl);

  /* Get the gimple sequence to replace the current function's body with a
     ifunc dispatch call to the right version.  */
  gseq = dispatch_using_ifunc (num_versions, current_function_decl,
			       cond_func_addr, fn_ver_addr_chain,
			       default_ver, &selector_decl, &ifunc_decl);

  mark_function_not_cloneable (selector_decl);
  mark_function_not_cloneable (ifunc_decl);

  for (gsi = gsi_start (gseq); !gsi_end_p (gsi); gsi_next (&gsi))
    gimple_set_bb (gsi_stmt (gsi), empty_bb);
   
  set_bb_seq (empty_bb, gseq);

  if (dump_file)
    dump_function_to_file (current_function_decl, dump_file, TDF_BLOCKS);

  update_ssa (TODO_update_ssa_no_phi);
  
  return 0;
}

static bool
gate_auto_clone (void)
{
  /* Turned on at -O2 and above.  */
  return optimize >= 2;
}

struct gimple_opt_pass pass_auto_clone =
{
 {
  GIMPLE_PASS,
  "auto_clone",			        /* name */
  gate_auto_clone,			/* gate */
  do_auto_clone,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_MVERSN_DISPATCH,			/* tv_id */
  PROP_cfg,				/* properties_required */
  PROP_cfg,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func |			/* todo_flags_finish */
  TODO_cleanup_cfg | TODO_dump_cgraph |
  TODO_update_ssa | TODO_verify_ssa
 }
};
