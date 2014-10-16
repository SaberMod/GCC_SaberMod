/* Perform the semantic phase of constexpr parsing, i.e., the process of
   building tree structure, checking semantic consistency, and
   building RTL.  These routines are used both during actual parsing
   and during the instantiation of template functions.

   Copyright (C) 1998-2014 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "varasm.h"
#include "cp-tree.h"
#include "c-family/c-objc.h"
#include "tree-iterator.h"
#include "gimplify.h"
#include "builtins.h"

static bool verify_constant (tree, bool, bool *, bool *);
#define VERIFY_CONSTANT(X)						\
do {									\
  if (verify_constant ((X), allow_non_constant, non_constant_p, overflow_p)) \
    return t;								\
 } while (0)

/* Returns true iff FUN is an instantiation of a constexpr function
   template or a defaulted constexpr function.  */

bool
is_instantiation_of_constexpr (tree fun)
{
  return ((DECL_TEMPLOID_INSTANTIATION (fun)
	   && DECL_DECLARED_CONSTEXPR_P (DECL_TI_TEMPLATE (fun)))
	  || (DECL_DEFAULTED_FN (fun)
	      && DECL_DECLARED_CONSTEXPR_P (fun)));
}

/* Return true if T is a literal type.   */

bool
literal_type_p (tree t)
{
  if (SCALAR_TYPE_P (t)
      || TREE_CODE (t) == VECTOR_TYPE
      || TREE_CODE (t) == REFERENCE_TYPE)
    return true;
  if (CLASS_TYPE_P (t))
    {
      t = complete_type (t);
      gcc_assert (COMPLETE_TYPE_P (t) || errorcount);
      return CLASSTYPE_LITERAL_P (t);
    }
  if (TREE_CODE (t) == ARRAY_TYPE)
    return literal_type_p (strip_array_types (t));
  return false;
}

/* If DECL is a variable declared `constexpr', require its type
   be literal.  Return the DECL if OK, otherwise NULL.  */

tree
ensure_literal_type_for_constexpr_object (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (VAR_P (decl)
      && (DECL_DECLARED_CONSTEXPR_P (decl)
	  || var_in_constexpr_fn (decl))
      && !processing_template_decl)
    {
      tree stype = strip_array_types (type);
      if (CLASS_TYPE_P (stype) && !COMPLETE_TYPE_P (complete_type (stype)))
	/* Don't complain here, we'll complain about incompleteness
	   when we try to initialize the variable.  */;
      else if (!literal_type_p (type))
	{
	  if (DECL_DECLARED_CONSTEXPR_P (decl))
	    error ("the type %qT of constexpr variable %qD is not literal",
		   type, decl);
	  else
	    error ("variable %qD of non-literal type %qT in %<constexpr%> "
		   "function", decl, type);
	  explain_non_literal_class (type);
	  return NULL;
	}
    }
  return decl;
}

/* Representation of entries in the constexpr function definition table.  */

struct GTY((for_user)) constexpr_fundef {
  tree decl;
  tree body;
};

struct constexpr_fundef_hasher : ggc_hasher<constexpr_fundef *>
{
  static hashval_t hash (constexpr_fundef *);
  static bool equal (constexpr_fundef *, constexpr_fundef *);
};

/* This table holds all constexpr function definitions seen in
   the current translation unit.  */

static GTY (()) hash_table<constexpr_fundef_hasher> *constexpr_fundef_table;

/* Utility function used for managing the constexpr function table.
   Return true if the entries pointed to by P and Q are for the
   same constexpr function.  */

inline bool
constexpr_fundef_hasher::equal (constexpr_fundef *lhs, constexpr_fundef *rhs)
{
  return lhs->decl == rhs->decl;
}

/* Utility function used for managing the constexpr function table.
   Return a hash value for the entry pointed to by Q.  */

inline hashval_t
constexpr_fundef_hasher::hash (constexpr_fundef *fundef)
{
  return DECL_UID (fundef->decl);
}

/* Return a previously saved definition of function FUN.   */

static constexpr_fundef *
retrieve_constexpr_fundef (tree fun)
{
  constexpr_fundef fundef = { NULL, NULL };
  if (constexpr_fundef_table == NULL)
    return NULL;

  fundef.decl = fun;
  return constexpr_fundef_table->find (&fundef);
}

/* Check whether the parameter and return types of FUN are valid for a
   constexpr function, and complain if COMPLAIN.  */

static bool
is_valid_constexpr_fn (tree fun, bool complain)
{
  bool ret = true;

  if (DECL_INHERITED_CTOR_BASE (fun)
      && TREE_CODE (fun) == TEMPLATE_DECL)
    {
      ret = false;
      if (complain)
	error ("inherited constructor %qD is not constexpr",
	       get_inherited_ctor (fun));
    }
  else
    {
      for (tree parm = FUNCTION_FIRST_USER_PARM (fun);
	   parm != NULL_TREE; parm = TREE_CHAIN (parm))
	if (!literal_type_p (TREE_TYPE (parm)))
	  {
	    ret = false;
	    if (complain)
	      {
		error ("invalid type for parameter %d of constexpr "
		       "function %q+#D", DECL_PARM_INDEX (parm), fun);
		explain_non_literal_class (TREE_TYPE (parm));
	      }
	  }
    }

  if (!DECL_CONSTRUCTOR_P (fun))
    {
      tree rettype = TREE_TYPE (TREE_TYPE (fun));
      if (!literal_type_p (rettype))
	{
	  ret = false;
	  if (complain)
	    {
	      error ("invalid return type %qT of constexpr function %q+D",
		     rettype, fun);
	      explain_non_literal_class (rettype);
	    }
	}

      if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fun)
	  && !CLASSTYPE_LITERAL_P (DECL_CONTEXT (fun)))
	{
	  ret = false;
	  if (complain)
	    {
	      error ("enclosing class of constexpr non-static member "
		     "function %q+#D is not a literal type", fun);
	      explain_non_literal_class (DECL_CONTEXT (fun));
	    }
	}
    }
  else if (CLASSTYPE_VBASECLASSES (DECL_CONTEXT (fun)))
    {
      ret = false;
      if (complain)
	error ("%q#T has virtual base classes", DECL_CONTEXT (fun));
    }

  return ret;
}

/* Subroutine of build_data_member_initialization.  MEMBER is a COMPONENT_REF
   for a member of an anonymous aggregate, INIT is the initializer for that
   member, and VEC_OUTER is the vector of constructor elements for the class
   whose constructor we are processing.  Add the initializer to the vector
   and return true to indicate success.  */

static bool
build_anon_member_initialization (tree member, tree init,
				  vec<constructor_elt, va_gc> **vec_outer)
{
  /* MEMBER presents the relevant fields from the inside out, but we need
     to build up the initializer from the outside in so that we can reuse
     previously built CONSTRUCTORs if this is, say, the second field in an
     anonymous struct.  So we use a vec as a stack.  */
  auto_vec<tree, 2> fields;
  do
    {
      fields.safe_push (TREE_OPERAND (member, 1));
      member = TREE_OPERAND (member, 0);
    }
  while (ANON_AGGR_TYPE_P (TREE_TYPE (member))
	 && TREE_CODE (member) == COMPONENT_REF);

  /* VEC has the constructor elements vector for the context of FIELD.
     If FIELD is an anonymous aggregate, we will push inside it.  */
  vec<constructor_elt, va_gc> **vec = vec_outer;
  tree field;
  while (field = fields.pop(),
	 ANON_AGGR_TYPE_P (TREE_TYPE (field)))
    {
      tree ctor;
      /* If there is already an outer constructor entry for the anonymous
	 aggregate FIELD, use it; otherwise, insert one.  */
      if (vec_safe_is_empty (*vec)
	  || (*vec)->last().index != field)
	{
	  ctor = build_constructor (TREE_TYPE (field), NULL);
	  CONSTRUCTOR_APPEND_ELT (*vec, field, ctor);
	}
      else
	ctor = (*vec)->last().value;
      vec = &CONSTRUCTOR_ELTS (ctor);
    }

  /* Now we're at the innermost field, the one that isn't an anonymous
     aggregate.  Add its initializer to the CONSTRUCTOR and we're done.  */
  gcc_assert (fields.is_empty());
  CONSTRUCTOR_APPEND_ELT (*vec, field, init);

  return true;
}

/* Subroutine of  build_constexpr_constructor_member_initializers.
   The expression tree T represents a data member initialization
   in a (constexpr) constructor definition.  Build a pairing of
   the data member with its initializer, and prepend that pair
   to the existing initialization pair INITS.  */

static bool
build_data_member_initialization (tree t, vec<constructor_elt, va_gc> **vec)
{
  tree member, init;
  if (TREE_CODE (t) == CLEANUP_POINT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == EXPR_STMT)
    t = TREE_OPERAND (t, 0);
  if (t == error_mark_node)
    return false;
  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      tree_stmt_iterator i;
      for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	{
	  if (! build_data_member_initialization (tsi_stmt (i), vec))
	    return false;
	}
      return true;
    }
  if (TREE_CODE (t) == CLEANUP_STMT)
    {
      /* We can't see a CLEANUP_STMT in a constructor for a literal class,
	 but we can in a constexpr constructor for a non-literal class.  Just
	 ignore it; either all the initialization will be constant, in which
	 case the cleanup can't run, or it can't be constexpr.
	 Still recurse into CLEANUP_BODY.  */
      return build_data_member_initialization (CLEANUP_BODY (t), vec);
    }
  if (TREE_CODE (t) == CONVERT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == INIT_EXPR
      || TREE_CODE (t) == MODIFY_EXPR)
    {
      member = TREE_OPERAND (t, 0);
      init = break_out_target_exprs (TREE_OPERAND (t, 1));
    }
  else if (TREE_CODE (t) == CALL_EXPR)
    {
      member = CALL_EXPR_ARG (t, 0);
      /* We don't use build_cplus_new here because it complains about
	 abstract bases.  Leaving the call unwrapped means that it has the
	 wrong type, but cxx_eval_constant_expression doesn't care.  */
      init = break_out_target_exprs (t);
    }
  else if (TREE_CODE (t) == BIND_EXPR)
    return build_data_member_initialization (BIND_EXPR_BODY (t), vec);
  else if (TREE_CODE (t) == DECL_EXPR
	   || TREE_CODE (t) == USING_STMT)
    /* Declaring a temporary, don't add it to the CONSTRUCTOR.
       Likewise for using directives.  */
    return true;
  else
    gcc_unreachable ();
  if (INDIRECT_REF_P (member))
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == NOP_EXPR)
    {
      tree op = member;
      STRIP_NOPS (op);
      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  gcc_assert (same_type_ignoring_top_level_qualifiers_p
		      (TREE_TYPE (TREE_TYPE (op)),
		       TREE_TYPE (TREE_TYPE (member))));
	  /* Initializing a cv-qualified member; we need to look through
	     the const_cast.  */
	  member = op;
	}
      else if (op == current_class_ptr
	       && (same_type_ignoring_top_level_qualifiers_p
		   (TREE_TYPE (TREE_TYPE (member)),
		    current_class_type)))
	/* Delegating constructor.  */
	member = op;
      else
	{
	  /* This is an initializer for an empty base; keep it for now so
	     we can check it in cxx_eval_bare_aggregate.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (member))));
	}
    }
  if (TREE_CODE (member) == ADDR_EXPR)
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == COMPONENT_REF)
    {
      tree aggr = TREE_OPERAND (member, 0);
      if (TREE_CODE (aggr) != COMPONENT_REF)
	/* Normal member initialization.  */
	member = TREE_OPERAND (member, 1);
      else if (ANON_AGGR_TYPE_P (TREE_TYPE (aggr)))
	/* Initializing a member of an anonymous union.  */
	return build_anon_member_initialization (member, init, vec);
      else
	/* We're initializing a vtable pointer in a base.  Leave it as
	   COMPONENT_REF so we remember the path to get to the vfield.  */
	gcc_assert (TREE_TYPE (member) == vtbl_ptr_type_node);
    }

  CONSTRUCTOR_APPEND_ELT (*vec, member, init);
  return true;
}

/* Subroutine of check_constexpr_ctor_body_1 and constexpr_fn_retval.
   In C++11 mode checks that the TYPE_DECLs in the BIND_EXPR_VARS of a 
   BIND_EXPR conform to 7.1.5/3/4 on typedef and alias declarations.  */

static bool
check_constexpr_bind_expr_vars (tree t)
{
  gcc_assert (TREE_CODE (t) == BIND_EXPR);

  if (cxx_dialect >= cxx14)
    return true;

  for (tree var = BIND_EXPR_VARS (t); var; var = DECL_CHAIN (var))
    if (TREE_CODE (var) == TYPE_DECL
	&& DECL_IMPLICIT_TYPEDEF_P (var))
      return false;
  return true;
}

/* Subroutine of check_constexpr_ctor_body.  */

static bool
check_constexpr_ctor_body_1 (tree last, tree list)
{
  switch (TREE_CODE (list))
    {
    case DECL_EXPR:
      if (TREE_CODE (DECL_EXPR_DECL (list)) == USING_DECL)
	return true;
      if (cxx_dialect >= cxx14)
	return true;
      return false;

    case CLEANUP_POINT_EXPR:
      return check_constexpr_ctor_body (last, TREE_OPERAND (list, 0),
					/*complain=*/false);

    case BIND_EXPR:
       if (!check_constexpr_bind_expr_vars (list)
	   || !check_constexpr_ctor_body (last, BIND_EXPR_BODY (list),
					  /*complain=*/false))
	 return false;
       return true;

    case USING_STMT:
    case STATIC_ASSERT:
      return true;

    default:
      return false;
    }
}

/* Make sure that there are no statements after LAST in the constructor
   body represented by LIST.  */

bool
check_constexpr_ctor_body (tree last, tree list, bool complain)
{
  bool ok = true;
  if (TREE_CODE (list) == STATEMENT_LIST)
    {
      tree_stmt_iterator i = tsi_last (list);
      for (; !tsi_end_p (i); tsi_prev (&i))
	{
	  tree t = tsi_stmt (i);
	  if (t == last)
	    break;
	  if (!check_constexpr_ctor_body_1 (last, t))
	    {
	      ok = false;
	      break;
	    }
	}
    }
  else if (list != last
	   && !check_constexpr_ctor_body_1 (last, list))
    ok = false;
  if (!ok)
    {
      if (complain)
	error ("constexpr constructor does not have empty body");
      DECL_DECLARED_CONSTEXPR_P (current_function_decl) = false;
    }
  return ok;
}

/* V is a vector of constructor elements built up for the base and member
   initializers of a constructor for TYPE.  They need to be in increasing
   offset order, which they might not be yet if TYPE has a primary base
   which is not first in the base-clause or a vptr and at least one base
   all of which are non-primary.  */

static vec<constructor_elt, va_gc> *
sort_constexpr_mem_initializers (tree type, vec<constructor_elt, va_gc> *v)
{
  tree pri = CLASSTYPE_PRIMARY_BINFO (type);
  tree field_type;
  unsigned i;
  constructor_elt *ce;

  if (pri)
    field_type = BINFO_TYPE (pri);
  else if (TYPE_CONTAINS_VPTR_P (type))
    field_type = vtbl_ptr_type_node;
  else
    return v;

  /* Find the element for the primary base or vptr and move it to the
     beginning of the vec.  */
  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    if (TREE_TYPE (ce->index) == field_type)
      break;

  if (i > 0 && i < vec_safe_length (v))
    {
      vec<constructor_elt, va_gc> &vref = *v;
      constructor_elt elt = vref[i];
      for (; i > 0; --i)
	vref[i] = vref[i-1];
      vref[0] = elt;
    }

  return v;
}

/* Build compile-time evalable representations of member-initializer list
   for a constexpr constructor.  */

static tree
build_constexpr_constructor_member_initializers (tree type, tree body)
{
  vec<constructor_elt, va_gc> *vec = NULL;
  bool ok = true;
  if (TREE_CODE (body) == MUST_NOT_THROW_EXPR
      || TREE_CODE (body) == EH_SPEC_BLOCK)
    body = TREE_OPERAND (body, 0);
  if (TREE_CODE (body) == STATEMENT_LIST)
    body = STATEMENT_LIST_HEAD (body)->stmt;
  body = BIND_EXPR_BODY (body);
  if (TREE_CODE (body) == CLEANUP_POINT_EXPR)
    {
      body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == EXPR_STMT)
	body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == INIT_EXPR
	  && (same_type_ignoring_top_level_qualifiers_p
	      (TREE_TYPE (TREE_OPERAND (body, 0)),
	       current_class_type)))
	{
	  /* Trivial copy.  */
	  return TREE_OPERAND (body, 1);
	}
      ok = build_data_member_initialization (body, &vec);
    }
  else if (TREE_CODE (body) == STATEMENT_LIST)
    {
      tree_stmt_iterator i;
      for (i = tsi_start (body); !tsi_end_p (i); tsi_next (&i))
	{
	  ok = build_data_member_initialization (tsi_stmt (i), &vec);
	  if (!ok)
	    break;
	}
    }
  else if (TREE_CODE (body) == TRY_BLOCK)
    {
      error ("body of %<constexpr%> constructor cannot be "
	     "a function-try-block");
      return error_mark_node;
    }
  else if (EXPR_P (body))
    ok = build_data_member_initialization (body, &vec);
  else
    gcc_assert (errorcount > 0);
  if (ok)
    {
      if (vec_safe_length (vec) > 0)
	{
	  /* In a delegating constructor, return the target.  */
	  constructor_elt *ce = &(*vec)[0];
	  if (ce->index == current_class_ptr)
	    {
	      body = ce->value;
	      vec_free (vec);
	      return body;
	    }
	}
      vec = sort_constexpr_mem_initializers (type, vec);
      return build_constructor (type, vec);
    }
  else
    return error_mark_node;
}

/* Subroutine of register_constexpr_fundef.  BODY is the body of a function
   declared to be constexpr, or a sub-statement thereof.  Returns the
   return value if suitable, error_mark_node for a statement not allowed in
   a constexpr function, or NULL_TREE if no return value was found.  */

static tree
constexpr_fn_retval (tree body)
{
  switch (TREE_CODE (body))
    {
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	tree expr = NULL_TREE;
	for (i = tsi_start (body); !tsi_end_p (i); tsi_next (&i))
	  {
	    tree s = constexpr_fn_retval (tsi_stmt (i));
	    if (s == error_mark_node)
	      return error_mark_node;
	    else if (s == NULL_TREE)
	      /* Keep iterating.  */;
	    else if (expr)
	      /* Multiple return statements.  */
	      return error_mark_node;
	    else
	      expr = s;
	  }
	return expr;
      }

    case RETURN_EXPR:
      return break_out_target_exprs (TREE_OPERAND (body, 0));

    case DECL_EXPR:
      if (TREE_CODE (DECL_EXPR_DECL (body)) == USING_DECL)
	return NULL_TREE;
      if (cxx_dialect >= cxx14)
	return NULL_TREE;
      return error_mark_node;

    case CLEANUP_POINT_EXPR:
      return constexpr_fn_retval (TREE_OPERAND (body, 0));

    case BIND_EXPR:
      if (!check_constexpr_bind_expr_vars (body))
	return error_mark_node;
      return constexpr_fn_retval (BIND_EXPR_BODY (body));

    case USING_STMT:
      return NULL_TREE;

    default:
      return error_mark_node;
    }
}

/* Subroutine of register_constexpr_fundef.  BODY is the DECL_SAVED_TREE of
   FUN; do the necessary transformations to turn it into a single expression
   that we can store in the hash table.  */

static tree
massage_constexpr_body (tree fun, tree body)
{
  if (DECL_CONSTRUCTOR_P (fun))
    body = build_constexpr_constructor_member_initializers
      (DECL_CONTEXT (fun), body);
  else
    {
      if (TREE_CODE (body) == EH_SPEC_BLOCK)
        body = EH_SPEC_STMTS (body);
      if (TREE_CODE (body) == MUST_NOT_THROW_EXPR)
	body = TREE_OPERAND (body, 0);
      body = constexpr_fn_retval (body);
    }
  return body;
}

/* FUN is a constexpr constructor with massaged body BODY.  Return true
   if some bases/fields are uninitialized, and complain if COMPLAIN.  */

static bool
cx_check_missing_mem_inits (tree fun, tree body, bool complain)
{
  bool bad;
  tree field;
  unsigned i, nelts;
  tree ctype;

  if (TREE_CODE (body) != CONSTRUCTOR)
    return false;

  nelts = CONSTRUCTOR_NELTS (body);
  ctype = DECL_CONTEXT (fun);
  field = TYPE_FIELDS (ctype);

  if (TREE_CODE (ctype) == UNION_TYPE)
    {
      if (nelts == 0 && next_initializable_field (field))
	{
	  if (complain)
	    error ("%<constexpr%> constructor for union %qT must "
		   "initialize exactly one non-static data member", ctype);
	  return true;
	}
      return false;
    }

  bad = false;
  for (i = 0; i <= nelts; ++i)
    {
      tree index;
      if (i == nelts)
	index = NULL_TREE;
      else
	{
	  index = CONSTRUCTOR_ELT (body, i)->index;
	  /* Skip base and vtable inits.  */
	  if (TREE_CODE (index) != FIELD_DECL
	      || DECL_ARTIFICIAL (index))
	    continue;
	}
      for (; field != index; field = DECL_CHAIN (field))
	{
	  tree ftype;
	  if (TREE_CODE (field) != FIELD_DECL
	      || (DECL_C_BIT_FIELD (field) && !DECL_NAME (field))
	      || DECL_ARTIFICIAL (field))
	    continue;
	  ftype = strip_array_types (TREE_TYPE (field));
	  if (type_has_constexpr_default_constructor (ftype))
	    {
	      /* It's OK to skip a member with a trivial constexpr ctor.
	         A constexpr ctor that isn't trivial should have been
	         added in by now.  */
	      gcc_checking_assert (!TYPE_HAS_COMPLEX_DFLT (ftype)
				   || errorcount != 0);
	      continue;
	    }
	  if (!complain)
	    return true;
	  error ("uninitialized member %qD in %<constexpr%> constructor",
		 field);
	  bad = true;
	}
      if (field == NULL_TREE)
	break;
      field = DECL_CHAIN (field);
    }

  return bad;
}

/* We are processing the definition of the constexpr function FUN.
   Check that its BODY fulfills the propriate requirements and
   enter it in the constexpr function definition table.
   For constructor BODY is actually the TREE_LIST of the
   member-initializer list.  */

tree
register_constexpr_fundef (tree fun, tree body)
{
  constexpr_fundef entry;
  constexpr_fundef **slot;

  if (!is_valid_constexpr_fn (fun, !DECL_GENERATED_P (fun)))
    return NULL;

  body = massage_constexpr_body (fun, body);
  if (body == NULL_TREE || body == error_mark_node)
    {
      if (!DECL_CONSTRUCTOR_P (fun))
	error ("body of constexpr function %qD not a return-statement", fun);
      return NULL;
    }

  if (!potential_rvalue_constant_expression (body))
    {
      if (!DECL_GENERATED_P (fun))
	require_potential_rvalue_constant_expression (body);
      return NULL;
    }

  if (DECL_CONSTRUCTOR_P (fun)
      && cx_check_missing_mem_inits (fun, body, !DECL_GENERATED_P (fun)))
    return NULL;

  /* Create the constexpr function table if necessary.  */
  if (constexpr_fundef_table == NULL)
    constexpr_fundef_table
      = hash_table<constexpr_fundef_hasher>::create_ggc (101);

  entry.decl = fun;
  entry.body = body;
  slot = constexpr_fundef_table->find_slot (&entry, INSERT);

  gcc_assert (*slot == NULL);
  *slot = ggc_alloc<constexpr_fundef> ();
  **slot = entry;

  return fun;
}

/* FUN is a non-constexpr function called in a context that requires a
   constant expression.  If it comes from a constexpr template, explain why
   the instantiation isn't constexpr.  */

void
explain_invalid_constexpr_fn (tree fun)
{
  static hash_set<tree> *diagnosed;
  tree body;
  location_t save_loc;
  /* Only diagnose defaulted functions or instantiations.  */
  if (!DECL_DEFAULTED_FN (fun)
      && !is_instantiation_of_constexpr (fun))
    return;
  if (diagnosed == NULL)
    diagnosed = new hash_set<tree>;
  if (diagnosed->add (fun))
    /* Already explained.  */
    return;

  save_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fun);
  inform (0, "%q+D is not usable as a constexpr function because:", fun);
  /* First check the declaration.  */
  if (is_valid_constexpr_fn (fun, true))
    {
      /* Then if it's OK, the body.  */
      if (!DECL_DECLARED_CONSTEXPR_P (fun))
	explain_implicit_non_constexpr (fun);
      else
	{
	  body = massage_constexpr_body (fun, DECL_SAVED_TREE (fun));
	  require_potential_rvalue_constant_expression (body);
	  if (DECL_CONSTRUCTOR_P (fun))
	    cx_check_missing_mem_inits (fun, body, true);
	}
    }
  input_location = save_loc;
}

/* Objects of this type represent calls to constexpr functions
   along with the bindings of parameters to their arguments, for
   the purpose of compile time evaluation.  */

struct GTY((for_user)) constexpr_call {
  /* Description of the constexpr function definition.  */
  constexpr_fundef *fundef;
  /* Parameter bindings environment.  A TREE_LIST where each TREE_PURPOSE
     is a parameter _DECL and the TREE_VALUE is the value of the parameter.
     Note: This arrangement is made to accommodate the use of
     iterative_hash_template_arg (see pt.c).  If you change this
     representation, also change the hash calculation in
     cxx_eval_call_expression.  */
  tree bindings;
  /* Result of the call.
       NULL means the call is being evaluated.
       error_mark_node means that the evaluation was erroneous;
       otherwise, the actuall value of the call.  */
  tree result;
  /* The hash of this call; we remember it here to avoid having to
     recalculate it when expanding the hash table.  */
  hashval_t hash;
};

struct constexpr_call_hasher : ggc_hasher<constexpr_call *>
{
  static hashval_t hash (constexpr_call *);
  static bool equal (constexpr_call *, constexpr_call *);
		     };

/* A table of all constexpr calls that have been evaluated by the
   compiler in this translation unit.  */

static GTY (()) hash_table<constexpr_call_hasher> *constexpr_call_table;

static tree cxx_eval_constant_expression (const constexpr_call *, tree,
					  bool, bool, bool *, bool *);

/* Compute a hash value for a constexpr call representation.  */

inline hashval_t
constexpr_call_hasher::hash (constexpr_call *info)
{
  return info->hash;
}

/* Return true if the objects pointed to by P and Q represent calls
   to the same constexpr function with the same arguments.
   Otherwise, return false.  */

bool
constexpr_call_hasher::equal (constexpr_call *lhs, constexpr_call *rhs)
{
  tree lhs_bindings;
  tree rhs_bindings;
  if (lhs == rhs)
    return 1;
  if (!constexpr_fundef_hasher::equal (lhs->fundef, rhs->fundef))
    return 0;
  lhs_bindings = lhs->bindings;
  rhs_bindings = rhs->bindings;
  while (lhs_bindings != NULL && rhs_bindings != NULL)
    {
      tree lhs_arg = TREE_VALUE (lhs_bindings);
      tree rhs_arg = TREE_VALUE (rhs_bindings);
      gcc_assert (TREE_TYPE (lhs_arg) == TREE_TYPE (rhs_arg));
      if (!cp_tree_equal (lhs_arg, rhs_arg))
        return 0;
      lhs_bindings = TREE_CHAIN (lhs_bindings);
      rhs_bindings = TREE_CHAIN (rhs_bindings);
    }
  return lhs_bindings == rhs_bindings;
}

/* Initialize the constexpr call table, if needed.  */

static void
maybe_initialize_constexpr_call_table (void)
{
  if (constexpr_call_table == NULL)
    constexpr_call_table = hash_table<constexpr_call_hasher>::create_ggc (101);
}

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  If the call is lexically to a named function,
   retrun the _DECL for that function.  */

static tree
get_function_named_in_call (tree t)
{
  tree fun = NULL;
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      fun = CALL_EXPR_FN (t);
      break;

    case AGGR_INIT_EXPR:
      fun = AGGR_INIT_EXPR_FN (t);
      break;

    default:
      gcc_unreachable();
      break;
    }
  if (TREE_CODE (fun) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (fun, 0)) == FUNCTION_DECL)
    fun = TREE_OPERAND (fun, 0);
  return fun;
}

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  Return the Nth argument.  */

static inline tree
get_nth_callarg (tree t, int n)
{
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      return CALL_EXPR_ARG (t, n);

    case AGGR_INIT_EXPR:
      return AGGR_INIT_EXPR_ARG (t, n);

    default:
      gcc_unreachable ();
      return NULL;
    }
}

/* Look up the binding of the function parameter T in a constexpr
   function call context CALL.  */

static tree
lookup_parameter_binding (const constexpr_call *call, tree t)
{
  tree b = purpose_member (t, call->bindings);
  return TREE_VALUE (b);
}

/* Attempt to evaluate T which represents a call to a builtin function.
   We assume here that all builtin functions evaluate to scalar types
   represented by _CST nodes.  */

static tree
cxx_eval_builtin_function_call (const constexpr_call *call, tree t,
				bool allow_non_constant, bool addr,
				bool *non_constant_p, bool *overflow_p)
{
  const int nargs = call_expr_nargs (t);
  tree *args = (tree *) alloca (nargs * sizeof (tree));
  tree new_call;
  int i;
  for (i = 0; i < nargs; ++i)
    {
      args[i] = cxx_eval_constant_expression (call, CALL_EXPR_ARG (t, i),
					      allow_non_constant, addr,
					      non_constant_p, overflow_p);
      if (allow_non_constant && *non_constant_p)
	return t;
    }
  if (*non_constant_p)
    return t;
  new_call = build_call_array_loc (EXPR_LOCATION (t), TREE_TYPE (t),
                                   CALL_EXPR_FN (t), nargs, args);
  new_call = fold (new_call);
  VERIFY_CONSTANT (new_call);
  return new_call;
}

/* TEMP is the constant value of a temporary object of type TYPE.  Adjust
   the type of the value to match.  */

static tree
adjust_temp_type (tree type, tree temp)
{
  if (TREE_TYPE (temp) == type)
    return temp;
  /* Avoid wrapping an aggregate value in a NOP_EXPR.  */
  if (TREE_CODE (temp) == CONSTRUCTOR)
    return build_constructor (type, CONSTRUCTOR_ELTS (temp));
  gcc_assert (scalarish_type_p (type));
  return cp_fold_convert (type, temp);
}

/* Subroutine of cxx_eval_call_expression.
   We are processing a call expression (either CALL_EXPR or
   AGGR_INIT_EXPR) in the call context of OLD_CALL.  Evaluate
   all arguments and bind their values to correspondings
   parameters, making up the NEW_CALL context.  */

static void
cxx_bind_parameters_in_call (const constexpr_call *old_call, tree t,
                             constexpr_call *new_call,
			     bool allow_non_constant,
			     bool *non_constant_p, bool *overflow_p)
{
  const int nargs = call_expr_nargs (t);
  tree fun = new_call->fundef->decl;
  tree parms = DECL_ARGUMENTS (fun);
  int i;
  for (i = 0; i < nargs; ++i)
    {
      tree x, arg;
      tree type = parms ? TREE_TYPE (parms) : void_type_node;
      /* For member function, the first argument is a pointer to the implied
         object.  And for an object construction, don't bind `this' before
         it is fully constructed.  */
      if (i == 0 && DECL_CONSTRUCTOR_P (fun))
        goto next;
      x = get_nth_callarg (t, i);
      if (parms && DECL_BY_REFERENCE (parms))
	{
	  /* cp_genericize made this a reference for argument passing, but
	     we don't want to treat it like one for constexpr evaluation.  */
	  gcc_assert (TREE_CODE (type) == REFERENCE_TYPE);
	  gcc_assert (TREE_CODE (TREE_TYPE (x)) == REFERENCE_TYPE);
	  type = TREE_TYPE (type);
	  x = convert_from_reference (x);
	}
      arg = cxx_eval_constant_expression (old_call, x, allow_non_constant,
					  TREE_CODE (type) == REFERENCE_TYPE,
					  non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p && allow_non_constant)
	return;
      /* Just discard ellipsis args after checking their constantitude.  */
      if (!parms)
	continue;
      if (*non_constant_p)
	/* Don't try to adjust the type of non-constant args.  */
	goto next;

      /* Make sure the binding has the same type as the parm.  */
      if (TREE_CODE (type) != REFERENCE_TYPE)
	arg = adjust_temp_type (type, arg);
      new_call->bindings = tree_cons (parms, arg, new_call->bindings);
    next:
      parms = TREE_CHAIN (parms);
    }
}

/* Variables and functions to manage constexpr call expansion context.
   These do not need to be marked for PCH or GC.  */

/* FIXME remember and print actual constant arguments.  */
static vec<tree> call_stack = vNULL;
static int call_stack_tick;
static int last_cx_error_tick;

static bool
push_cx_call_context (tree call)
{
  ++call_stack_tick;
  if (!EXPR_HAS_LOCATION (call))
    SET_EXPR_LOCATION (call, input_location);
  call_stack.safe_push (call);
  if (call_stack.length () > (unsigned) max_constexpr_depth)
    return false;
  return true;
}

static void
pop_cx_call_context (void)
{
  ++call_stack_tick;
  call_stack.pop ();
}

vec<tree> 
cx_error_context (void)
{
  vec<tree> r = vNULL;
  if (call_stack_tick != last_cx_error_tick
      && !call_stack.is_empty ())
    r = call_stack;
  last_cx_error_tick = call_stack_tick;
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Evaluate the call expression tree T in the context of OLD_CALL expression
   evaluation.  */

static tree
cxx_eval_call_expression (const constexpr_call *old_call, tree t,
			  bool allow_non_constant, bool addr,
			  bool *non_constant_p, bool *overflow_p)
{
  location_t loc = EXPR_LOC_OR_LOC (t, input_location);
  tree fun = get_function_named_in_call (t);
  tree result;
  constexpr_call new_call = { NULL, NULL, NULL, 0 };
  constexpr_call **slot;
  constexpr_call *entry;
  bool depth_ok;

  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      /* Might be a constexpr function pointer.  */
      fun = cxx_eval_constant_expression (old_call, fun, allow_non_constant,
					  /*addr*/false, non_constant_p,
					  overflow_p);
      STRIP_NOPS (fun);
      if (TREE_CODE (fun) == ADDR_EXPR)
	fun = TREE_OPERAND (fun, 0);
    }
  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      if (!allow_non_constant && !*non_constant_p)
	error_at (loc, "expression %qE does not designate a constexpr "
		  "function", fun);
      *non_constant_p = true;
      return t;
    }
  if (DECL_CLONED_FUNCTION_P (fun))
    fun = DECL_CLONED_FUNCTION (fun);
  if (is_builtin_fn (fun))
    return cxx_eval_builtin_function_call (old_call, t, allow_non_constant,
					   addr, non_constant_p, overflow_p);
  if (!DECL_DECLARED_CONSTEXPR_P (fun))
    {
      if (!allow_non_constant)
	{
	  error_at (loc, "call to non-constexpr function %qD", fun);
	  explain_invalid_constexpr_fn (fun);
	}
      *non_constant_p = true;
      return t;
    }

  /* Shortcut trivial constructor/op=.  */
  if (trivial_fn_p (fun))
    {
      if (call_expr_nargs (t) == 2)
	{
	  tree arg = convert_from_reference (get_nth_callarg (t, 1));
	  return cxx_eval_constant_expression (old_call, arg, allow_non_constant,
					       addr, non_constant_p, overflow_p);
	}
      else if (TREE_CODE (t) == AGGR_INIT_EXPR
	       && AGGR_INIT_ZERO_FIRST (t))
	return build_zero_init (DECL_CONTEXT (fun), NULL_TREE, false);
    }

  /* If in direct recursive call, optimize definition search.  */
  if (old_call != NULL && old_call->fundef->decl == fun)
    new_call.fundef = old_call->fundef;
  else
    {
      new_call.fundef = retrieve_constexpr_fundef (fun);
      if (new_call.fundef == NULL || new_call.fundef->body == NULL)
        {
	  if (!allow_non_constant)
	    {
	      if (DECL_INITIAL (fun))
		{
		  /* The definition of fun was somehow unsuitable.  */
		  error_at (loc, "%qD called in a constant expression", fun);
		  explain_invalid_constexpr_fn (fun);
		}
	      else
		error_at (loc, "%qD used before its definition", fun);
	    }
	  *non_constant_p = true;
          return t;
        }
    }
  cxx_bind_parameters_in_call (old_call, t, &new_call,
			       allow_non_constant, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  depth_ok = push_cx_call_context (t);

  new_call.hash
    = iterative_hash_template_arg (new_call.bindings,
				   constexpr_fundef_hasher::hash (new_call.fundef));

  /* If we have seen this call before, we are done.  */
  maybe_initialize_constexpr_call_table ();
  slot = constexpr_call_table->find_slot (&new_call, INSERT);
  entry = *slot;
  if (entry == NULL)
    {
      /* We need to keep a pointer to the entry, not just the slot, as the
	 slot can move in the call to cxx_eval_builtin_function_call.  */
      *slot = entry = ggc_alloc<constexpr_call> ();
      *entry = new_call;
    }
  /* Calls which are in progress have their result set to NULL
     so that we can detect circular dependencies.  */
  else if (entry->result == NULL)
    {
      if (!allow_non_constant)
	error ("call has circular dependency");
      *non_constant_p = true;
      entry->result = result = error_mark_node;
    }

  if (!depth_ok)
    {
      if (!allow_non_constant)
	error ("constexpr evaluation depth exceeds maximum of %d (use "
	       "-fconstexpr-depth= to increase the maximum)",
	       max_constexpr_depth);
      *non_constant_p = true;
      entry->result = result = error_mark_node;
    }
  else
    {
      result = entry->result;
      if (!result || result == error_mark_node)
	result = (cxx_eval_constant_expression
		  (&new_call, new_call.fundef->body,
		   allow_non_constant, addr,
		   non_constant_p, overflow_p));
      if (result == error_mark_node)
	*non_constant_p = true;
      if (*non_constant_p)
	entry->result = result = error_mark_node;
      else
	{
	  /* If this was a call to initialize an object, set the type of
	     the CONSTRUCTOR to the type of that object.  */
	  if (DECL_CONSTRUCTOR_P (fun))
	    {
	      tree ob_arg = get_nth_callarg (t, 0);
	      STRIP_NOPS (ob_arg);
	      gcc_assert (TYPE_PTR_P (TREE_TYPE (ob_arg))
			  && CLASS_TYPE_P (TREE_TYPE (TREE_TYPE (ob_arg))));
	      result = adjust_temp_type (TREE_TYPE (TREE_TYPE (ob_arg)),
					 result);
	    }
	  entry->result = result;
	}
    }

  pop_cx_call_context ();
  return unshare_expr (result);
}

/* FIXME speed this up, it's taking 16% of compile time on sieve testcase.  */

bool
reduced_constant_expression_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case PTRMEM_CST:
      /* Even if we can't lower this yet, it's constant.  */
      return true;

    case CONSTRUCTOR:
      /* And we need to handle PTRMEM_CST wrapped in a CONSTRUCTOR.  */
      tree elt; unsigned HOST_WIDE_INT idx;
      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (t), idx, elt)
	if (!reduced_constant_expression_p (elt))
	  return false;
      return true;

    default:
      /* FIXME are we calling this too much?  */
      return initializer_constant_valid_p (t, TREE_TYPE (t)) != NULL_TREE;
    }
}

/* Some expressions may have constant operands but are not constant
   themselves, such as 1/0.  Call this function (or rather, the macro
   following it) to check for that condition.

   We only call this in places that require an arithmetic constant, not in
   places where we might have a non-constant expression that can be a
   component of a constant expression, such as the address of a constexpr
   variable that might be dereferenced later.  */

static bool
verify_constant (tree t, bool allow_non_constant, bool *non_constant_p,
		 bool *overflow_p)
{
  if (!*non_constant_p && !reduced_constant_expression_p (t))
    {
      if (!allow_non_constant)
	error ("%q+E is not a constant expression", t);
      *non_constant_p = true;
    }
  if (TREE_OVERFLOW_P (t))
    {
      if (!allow_non_constant)
	{
	  permerror (input_location, "overflow in constant expression");
	  /* If we're being permissive (and are in an enforcing
	     context), ignore the overflow.  */
	  if (flag_permissive)
	    return *non_constant_p;
	}
      *overflow_p = true;
    }
  return *non_constant_p;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce the unary expression tree T to a compile time value.
   If successful, return the value.  Otherwise issue a diagnostic
   and return error_mark_node.  */

static tree
cxx_eval_unary_expression (const constexpr_call *call, tree t,
			   bool allow_non_constant, bool addr,
			   bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree orig_arg = TREE_OPERAND (t, 0);
  tree arg = cxx_eval_constant_expression (call, orig_arg, allow_non_constant,
					   addr, non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg);
  if (arg == orig_arg)
    return t;
  r = fold_build1 (TREE_CODE (t), TREE_TYPE (t), arg);
  VERIFY_CONSTANT (r);
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for binary expressions.  */

static tree
cxx_eval_binary_expression (const constexpr_call *call, tree t,
			    bool allow_non_constant, bool addr,
			    bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree orig_lhs = TREE_OPERAND (t, 0);
  tree orig_rhs = TREE_OPERAND (t, 1);
  tree lhs, rhs;
  lhs = cxx_eval_constant_expression (call, orig_lhs,
				      allow_non_constant, addr,
				      non_constant_p, overflow_p);
  VERIFY_CONSTANT (lhs);
  rhs = cxx_eval_constant_expression (call, orig_rhs,
				      allow_non_constant, addr,
				      non_constant_p, overflow_p);
  VERIFY_CONSTANT (rhs);
  if (lhs == orig_lhs && rhs == orig_rhs)
    return t;
  r = fold_build2 (TREE_CODE (t), TREE_TYPE (t), lhs, rhs);
  VERIFY_CONSTANT (r);
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate condition expressions.  Dead branches are not
   looked into.  */

static tree
cxx_eval_conditional_expression (const constexpr_call *call, tree t,
				 bool allow_non_constant, bool addr,
				 bool *non_constant_p, bool *overflow_p)
{
  tree val = cxx_eval_constant_expression (call, TREE_OPERAND (t, 0),
					   allow_non_constant, addr,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (val);
  /* Don't VERIFY_CONSTANT the other operands.  */
  if (integer_zerop (val))
    return cxx_eval_constant_expression (call, TREE_OPERAND (t, 2),
					 allow_non_constant, addr,
					 non_constant_p, overflow_p);
  return cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a reference to an array slot.  */

static tree
cxx_eval_array_reference (const constexpr_call *call, tree t,
			  bool allow_non_constant, bool addr,
			  bool *non_constant_p, bool *overflow_p)
{
  tree oldary = TREE_OPERAND (t, 0);
  tree ary = cxx_eval_constant_expression (call, oldary,
					   allow_non_constant, addr,
					   non_constant_p, overflow_p);
  tree index, oldidx;
  HOST_WIDE_INT i;
  tree elem_type;
  unsigned len, elem_nchars = 1;
  if (*non_constant_p)
    return t;
  oldidx = TREE_OPERAND (t, 1);
  index = cxx_eval_constant_expression (call, oldidx,
					allow_non_constant, false,
					non_constant_p, overflow_p);
  VERIFY_CONSTANT (index);
  if (addr && ary == oldary && index == oldidx)
    return t;
  else if (addr)
    return build4 (ARRAY_REF, TREE_TYPE (t), ary, index, NULL, NULL);
  elem_type = TREE_TYPE (TREE_TYPE (ary));
  if (TREE_CODE (ary) == CONSTRUCTOR)
    len = CONSTRUCTOR_NELTS (ary);
  else if (TREE_CODE (ary) == STRING_CST)
    {
      elem_nchars = (TYPE_PRECISION (elem_type)
		     / TYPE_PRECISION (char_type_node));
      len = (unsigned) TREE_STRING_LENGTH (ary) / elem_nchars;
    }
  else
    {
      /* We can't do anything with other tree codes, so use
	 VERIFY_CONSTANT to complain and fail.  */
      VERIFY_CONSTANT (ary);
      gcc_unreachable ();
    }
  if (compare_tree_int (index, len) >= 0)
    {
      if (tree_int_cst_lt (index, array_type_nelts_top (TREE_TYPE (ary))))
	{
	  /* If it's within the array bounds but doesn't have an explicit
	     initializer, it's value-initialized.  */
	  tree val = build_value_init (elem_type, tf_warning_or_error);
	  return cxx_eval_constant_expression (call, val,
					       allow_non_constant, addr,
					       non_constant_p, overflow_p);
	}

      if (!allow_non_constant)
	error ("array subscript out of bound");
      *non_constant_p = true;
      return t;
    }
  else if (tree_int_cst_lt (index, integer_zero_node))
    {
      if (!allow_non_constant)
	error ("negative array subscript");
      *non_constant_p = true;
      return t;
    }
  i = tree_to_shwi (index);
  if (TREE_CODE (ary) == CONSTRUCTOR)
    return (*CONSTRUCTOR_ELTS (ary))[i].value;
  else if (elem_nchars == 1)
    return build_int_cst (cv_unqualified (TREE_TYPE (TREE_TYPE (ary))),
			  TREE_STRING_POINTER (ary)[i]);
  else
    {
      tree type = cv_unqualified (TREE_TYPE (TREE_TYPE (ary)));
      return native_interpret_expr (type, (const unsigned char *)
					  TREE_STRING_POINTER (ary)
					  + i * elem_nchars, elem_nchars);
    }
  /* Don't VERIFY_CONSTANT here.  */
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type.  */

static tree
cxx_eval_component_reference (const constexpr_call *call, tree t,
			      bool allow_non_constant, bool addr,
			      bool *non_constant_p, bool *overflow_p)
{
  unsigned HOST_WIDE_INT i;
  tree field;
  tree value;
  tree part = TREE_OPERAND (t, 1);
  tree orig_whole = TREE_OPERAND (t, 0);
  tree whole = cxx_eval_constant_expression (call, orig_whole,
					     allow_non_constant, addr,
					     non_constant_p, overflow_p);
  if (whole == orig_whole)
    return t;
  if (addr)
    return fold_build3 (COMPONENT_REF, TREE_TYPE (t),
			whole, part, NULL_TREE);
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (DECL_MUTABLE_P (part))
    {
      if (!allow_non_constant)
	error ("mutable %qD is not usable in a constant expression", part);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      if (field == part)
        return value;
    }
  if (TREE_CODE (TREE_TYPE (whole)) == UNION_TYPE
      && CONSTRUCTOR_NELTS (whole) > 0)
    {
      /* DR 1188 says we don't have to deal with this.  */
      if (!allow_non_constant)
	error ("accessing %qD member instead of initialized %qD member in "
	       "constant expression", part, CONSTRUCTOR_ELT (whole, 0)->index);
      *non_constant_p = true;
      return t;
    }

  /* If there's no explicit init for this field, it's value-initialized.  */
  value = build_value_init (TREE_TYPE (t), tf_warning_or_error);
  return cxx_eval_constant_expression (call, value,
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type that is
   expressed as a BIT_FIELD_REF.  */

static tree
cxx_eval_bit_field_ref (const constexpr_call *call, tree t,
			bool allow_non_constant, bool addr,
			bool *non_constant_p, bool *overflow_p)
{
  tree orig_whole = TREE_OPERAND (t, 0);
  tree retval, fldval, utype, mask;
  bool fld_seen = false;
  HOST_WIDE_INT istart, isize;
  tree whole = cxx_eval_constant_expression (call, orig_whole,
					     allow_non_constant, addr,
					     non_constant_p, overflow_p);
  tree start, field, value;
  unsigned HOST_WIDE_INT i;

  if (whole == orig_whole)
    return t;
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p
      && TREE_CODE (whole) != VECTOR_CST
      && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;

  if (TREE_CODE (whole) == VECTOR_CST)
    return fold_ternary (BIT_FIELD_REF, TREE_TYPE (t), whole,
			 TREE_OPERAND (t, 1), TREE_OPERAND (t, 2));

  start = TREE_OPERAND (t, 2);
  istart = tree_to_shwi (start);
  isize = tree_to_shwi (TREE_OPERAND (t, 1));
  utype = TREE_TYPE (t);
  if (!TYPE_UNSIGNED (utype))
    utype = build_nonstandard_integer_type (TYPE_PRECISION (utype), 1);
  retval = build_int_cst (utype, 0);
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      tree bitpos = bit_position (field);
      if (bitpos == start && DECL_SIZE (field) == TREE_OPERAND (t, 1))
	return value;
      if (TREE_CODE (TREE_TYPE (field)) == INTEGER_TYPE
	  && TREE_CODE (value) == INTEGER_CST
	  && tree_fits_shwi_p (bitpos)
	  && tree_fits_shwi_p (DECL_SIZE (field)))
	{
	  HOST_WIDE_INT bit = tree_to_shwi (bitpos);
	  HOST_WIDE_INT sz = tree_to_shwi (DECL_SIZE (field));
	  HOST_WIDE_INT shift;
	  if (bit >= istart && bit + sz <= istart + isize)
	    {
	      fldval = fold_convert (utype, value);
	      mask = build_int_cst_type (utype, -1);
	      mask = fold_build2 (LSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      mask = fold_build2 (RSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      fldval = fold_build2 (BIT_AND_EXPR, utype, fldval, mask);
	      shift = bit - istart;
	      if (BYTES_BIG_ENDIAN)
		shift = TYPE_PRECISION (utype) - shift - sz;
	      fldval = fold_build2 (LSHIFT_EXPR, utype, fldval,
				    size_int (shift));
	      retval = fold_build2 (BIT_IOR_EXPR, utype, retval, fldval);
	      fld_seen = true;
	    }
	}
    }
  if (fld_seen)
    return fold_convert (TREE_TYPE (t), retval);
  gcc_unreachable ();
  return error_mark_node;
}

/* Subroutine of cxx_eval_constant_expression.
   Evaluate a short-circuited logical expression T in the context
   of a given constexpr CALL.  BAILOUT_VALUE is the value for
   early return.  CONTINUE_VALUE is used here purely for
   sanity check purposes.  */

static tree
cxx_eval_logical_expression (const constexpr_call *call, tree t,
                             tree bailout_value, tree continue_value,
			     bool allow_non_constant, bool addr,
			     bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree lhs = cxx_eval_constant_expression (call, TREE_OPERAND (t, 0),
					   allow_non_constant, addr,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (lhs);
  if (tree_int_cst_equal (lhs, bailout_value))
    return lhs;
  gcc_assert (tree_int_cst_equal (lhs, continue_value));
  r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
				    allow_non_constant, addr, non_constant_p, overflow_p);
  VERIFY_CONSTANT (r);
  return r;
}

/* REF is a COMPONENT_REF designating a particular field.  V is a vector of
   CONSTRUCTOR elements to initialize (part of) an object containing that
   field.  Return a pointer to the constructor_elt corresponding to the
   initialization of the field.  */

static constructor_elt *
base_field_constructor_elt (vec<constructor_elt, va_gc> *v, tree ref)
{
  tree aggr = TREE_OPERAND (ref, 0);
  tree field = TREE_OPERAND (ref, 1);
  HOST_WIDE_INT i;
  constructor_elt *ce;

  gcc_assert (TREE_CODE (ref) == COMPONENT_REF);

  if (TREE_CODE (aggr) == COMPONENT_REF)
    {
      constructor_elt *base_ce
	= base_field_constructor_elt (v, aggr);
      v = CONSTRUCTOR_ELTS (base_ce->value);
    }

  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    if (ce->index == field)
      return ce;

  gcc_unreachable ();
  return NULL;
}

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T denotes a C-style array or a C-style
   aggregate.  Reduce it to a constant expression.  */

static tree
cxx_eval_bare_aggregate (const constexpr_call *call, tree t,
			 bool allow_non_constant, bool addr,
			 bool *non_constant_p, bool *overflow_p)
{
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
  vec<constructor_elt, va_gc> *n;
  vec_alloc (n, vec_safe_length (v));
  constructor_elt *ce;
  HOST_WIDE_INT i;
  bool changed = false;
  gcc_assert (!BRACE_ENCLOSED_INITIALIZER_P (t));
  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    {
      tree elt = cxx_eval_constant_expression (call, ce->value,
					       allow_non_constant, addr,
					       non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (allow_non_constant && *non_constant_p)
	goto fail;
      if (elt != ce->value)
	changed = true;
      if (ce->index && TREE_CODE (ce->index) == COMPONENT_REF)
	{
	  /* This is an initialization of a vfield inside a base
	     subaggregate that we already initialized; push this
	     initialization into the previous initialization.  */
	  constructor_elt *inner = base_field_constructor_elt (n, ce->index);
	  inner->value = elt;
	}
      else if (ce->index
	       && (TREE_CODE (ce->index) == NOP_EXPR
		   || TREE_CODE (ce->index) == POINTER_PLUS_EXPR))
	{
	  /* This is an initializer for an empty base; now that we've
	     checked that it's constant, we can ignore it.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (ce->index))));
	}
      else
	CONSTRUCTOR_APPEND_ELT (n, ce->index, elt);
    }
  if (*non_constant_p || !changed)
    {
    fail:
      vec_free (n);
      return t;
    }
  t = build_constructor (TREE_TYPE (t), n);
  TREE_CONSTANT (t) = true;
  if (TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
    t = fold (t);
  return t;
}

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T is a VEC_INIT_EXPR which denotes the desired
   initialization of a non-static data member of array type.  Reduce it to a
   CONSTRUCTOR.

   Note that apart from value-initialization (when VALUE_INIT is true),
   this is only intended to support value-initialization and the
   initializations done by defaulted constructors for classes with
   non-static data members of array type.  In this case, VEC_INIT_EXPR_INIT
   will either be NULL_TREE for the default constructor, or a COMPONENT_REF
   for the copy/move constructor.  */

static tree
cxx_eval_vec_init_1 (const constexpr_call *call, tree atype, tree init,
		     bool value_init, bool allow_non_constant, bool addr,
		     bool *non_constant_p, bool *overflow_p)
{
  tree elttype = TREE_TYPE (atype);
  int max = tree_to_shwi (array_type_nelts (atype));
  vec<constructor_elt, va_gc> *n;
  vec_alloc (n, max + 1);
  bool pre_init = false;
  int i;

  /* For the default constructor, build up a call to the default
     constructor of the element type.  We only need to handle class types
     here, as for a constructor to be constexpr, all members must be
     initialized, which for a defaulted default constructor means they must
     be of a class type with a constexpr default constructor.  */
  if (TREE_CODE (elttype) == ARRAY_TYPE)
    /* We only do this at the lowest level.  */;
  else if (value_init)
    {
      init = build_value_init (elttype, tf_warning_or_error);
      init = cxx_eval_constant_expression
	    (call, init, allow_non_constant, addr, non_constant_p, overflow_p);
      pre_init = true;
    }
  else if (!init)
    {
      vec<tree, va_gc> *argvec = make_tree_vector ();
      init = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&argvec, elttype, LOOKUP_NORMAL,
					tf_warning_or_error);
      release_tree_vector (argvec);
      init = cxx_eval_constant_expression (call, init, allow_non_constant,
					   addr, non_constant_p, overflow_p);
      pre_init = true;
    }

  if (*non_constant_p && !allow_non_constant)
    goto fail;

  for (i = 0; i <= max; ++i)
    {
      tree idx = build_int_cst (size_type_node, i);
      tree eltinit;
      if (TREE_CODE (elttype) == ARRAY_TYPE)
	{
	  /* A multidimensional array; recurse.  */
	  if (value_init || init == NULL_TREE)
	    eltinit = NULL_TREE;
	  else
	    eltinit = cp_build_array_ref (input_location, init, idx,
					  tf_warning_or_error);
	  eltinit = cxx_eval_vec_init_1 (call, elttype, eltinit, value_init,
					 allow_non_constant, addr,
					 non_constant_p, overflow_p);
	}
      else if (pre_init)
	{
	  /* Initializing an element using value or default initialization
	     we just pre-built above.  */
	  if (i == 0)
	    eltinit = init;
	  else
	    eltinit = unshare_expr (init);
	}
      else
	{
	  /* Copying an element.  */
	  gcc_assert (same_type_ignoring_top_level_qualifiers_p
		      (atype, TREE_TYPE (init)));
	  eltinit = cp_build_array_ref (input_location, init, idx,
					tf_warning_or_error);
	  if (!real_lvalue_p (init))
	    eltinit = move (eltinit);
	  eltinit = force_rvalue (eltinit, tf_warning_or_error);
	  eltinit = cxx_eval_constant_expression
	    (call, eltinit, allow_non_constant, addr, non_constant_p, overflow_p);
	}
      if (*non_constant_p && !allow_non_constant)
	goto fail;
      CONSTRUCTOR_APPEND_ELT (n, idx, eltinit);
    }

  if (!*non_constant_p)
    {
      init = build_constructor (atype, n);
      TREE_CONSTANT (init) = true;
      return init;
    }

 fail:
  vec_free (n);
  return init;
}

static tree
cxx_eval_vec_init (const constexpr_call *call, tree t,
		   bool allow_non_constant, bool addr,
		   bool *non_constant_p, bool *overflow_p)
{
  tree atype = TREE_TYPE (t);
  tree init = VEC_INIT_EXPR_INIT (t);
  tree r = cxx_eval_vec_init_1 (call, atype, init,
				VEC_INIT_EXPR_VALUE_INIT (t),
				allow_non_constant, addr, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;
  else
    return r;
}

/* A less strict version of fold_indirect_ref_1, which requires cv-quals to
   match.  We want to be less strict for simple *& folding; if we have a
   non-const temporary that we access through a const pointer, that should
   work.  We handle this here rather than change fold_indirect_ref_1
   because we're dealing with things like ADDR_EXPR of INTEGER_CST which
   don't really make sense outside of constant expression evaluation.  Also
   we want to allow folding to COMPONENT_REF, which could cause trouble
   with TBAA in fold_indirect_ref_1.

   Try to keep this function synced with fold_indirect_ref_1.  */

static tree
cxx_fold_indirect_ref (location_t loc, tree type, tree op0, bool *empty_base)
{
  tree sub, subtype;

  sub = op0;
  STRIP_NOPS (sub);
  subtype = TREE_TYPE (sub);
  if (!POINTER_TYPE_P (subtype))
    return NULL_TREE;

  if (TREE_CODE (sub) == ADDR_EXPR)
    {
      tree op = TREE_OPERAND (sub, 0);
      tree optype = TREE_TYPE (op);

      /* *&CONST_DECL -> to the value of the const decl.  */
      if (TREE_CODE (op) == CONST_DECL)
	return DECL_INITIAL (op);
      /* *&p => p;  make sure to handle *&"str"[cst] here.  */
      if (same_type_ignoring_top_level_qualifiers_p (optype, type))
	{
	  tree fop = fold_read_from_constant_string (op);
	  if (fop)
	    return fop;
	  else
	    return op;
	}
      /* *(foo *)&fooarray => fooarray[0] */
      else if (TREE_CODE (optype) == ARRAY_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	{
	  tree type_domain = TYPE_DOMAIN (optype);
	  tree min_val = size_zero_node;
	  if (type_domain && TYPE_MIN_VALUE (type_domain))
	    min_val = TYPE_MIN_VALUE (type_domain);
	  return build4_loc (loc, ARRAY_REF, type, op, min_val,
			     NULL_TREE, NULL_TREE);
	}
      /* *(foo *)&complexfoo => __real__ complexfoo */
      else if (TREE_CODE (optype) == COMPLEX_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	return fold_build1_loc (loc, REALPART_EXPR, type, op);
      /* *(foo *)&vectorfoo => BIT_FIELD_REF<vectorfoo,...> */
      else if (TREE_CODE (optype) == VECTOR_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	{
	  tree part_width = TYPE_SIZE (type);
	  tree index = bitsize_int (0);
	  return fold_build3_loc (loc, BIT_FIELD_REF, type, op, part_width, index);
	}
      /* Also handle conversion to an empty base class, which
	 is represented with a NOP_EXPR.  */
      else if (is_empty_class (type)
	       && CLASS_TYPE_P (optype)
	       && DERIVED_FROM_P (type, optype))
	{
	  *empty_base = true;
	  return op;
	}
      /* *(foo *)&struct_with_foo_field => COMPONENT_REF */
      else if (RECORD_OR_UNION_TYPE_P (optype))
	{
	  tree field = TYPE_FIELDS (optype);
	  for (; field; field = DECL_CHAIN (field))
	    if (TREE_CODE (field) == FIELD_DECL
		&& integer_zerop (byte_position (field))
		&& (same_type_ignoring_top_level_qualifiers_p
		    (TREE_TYPE (field), type)))
	      {
		return fold_build3 (COMPONENT_REF, type, op, field, NULL_TREE);
		break;
	      }
	}
    }
  else if (TREE_CODE (sub) == POINTER_PLUS_EXPR
	   && TREE_CODE (TREE_OPERAND (sub, 1)) == INTEGER_CST)
    {
      tree op00 = TREE_OPERAND (sub, 0);
      tree op01 = TREE_OPERAND (sub, 1);

      STRIP_NOPS (op00);
      if (TREE_CODE (op00) == ADDR_EXPR)
	{
	  tree op00type;
	  op00 = TREE_OPERAND (op00, 0);
	  op00type = TREE_TYPE (op00);

	  /* ((foo*)&vectorfoo)[1] => BIT_FIELD_REF<vectorfoo,...> */
	  if (TREE_CODE (op00type) == VECTOR_TYPE
	      && (same_type_ignoring_top_level_qualifiers_p
		  (type, TREE_TYPE (op00type))))
	    {
	      HOST_WIDE_INT offset = tree_to_shwi (op01);
	      tree part_width = TYPE_SIZE (type);
	      unsigned HOST_WIDE_INT part_widthi = tree_to_shwi (part_width)/BITS_PER_UNIT;
	      unsigned HOST_WIDE_INT indexi = offset * BITS_PER_UNIT;
	      tree index = bitsize_int (indexi);

	      if (offset / part_widthi < TYPE_VECTOR_SUBPARTS (op00type))
		return fold_build3_loc (loc,
					BIT_FIELD_REF, type, op00,
					part_width, index);

	    }
	  /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
	  else if (TREE_CODE (op00type) == COMPLEX_TYPE
		   && (same_type_ignoring_top_level_qualifiers_p
		       (type, TREE_TYPE (op00type))))
	    {
	      tree size = TYPE_SIZE_UNIT (type);
	      if (tree_int_cst_equal (size, op01))
		return fold_build1_loc (loc, IMAGPART_EXPR, type, op00);
	    }
	  /* ((foo *)&fooarray)[1] => fooarray[1] */
	  else if (TREE_CODE (op00type) == ARRAY_TYPE
		   && (same_type_ignoring_top_level_qualifiers_p
		       (type, TREE_TYPE (op00type))))
	    {
	      tree type_domain = TYPE_DOMAIN (op00type);
	      tree min_val = size_zero_node;
	      if (type_domain && TYPE_MIN_VALUE (type_domain))
		min_val = TYPE_MIN_VALUE (type_domain);
	      op01 = size_binop_loc (loc, EXACT_DIV_EXPR, op01,
				     TYPE_SIZE_UNIT (type));
	      op01 = size_binop_loc (loc, PLUS_EXPR, op01, min_val);
	      return build4_loc (loc, ARRAY_REF, type, op00, op01,
				 NULL_TREE, NULL_TREE);
	    }
	  /* Also handle conversion to an empty base class, which
	     is represented with a NOP_EXPR.  */
	  else if (is_empty_class (type)
		   && CLASS_TYPE_P (op00type)
		   && DERIVED_FROM_P (type, op00type))
	    {
	      *empty_base = true;
	      return op00;
	    }
	  /* ((foo *)&struct_with_foo_field)[1] => COMPONENT_REF */
	  else if (RECORD_OR_UNION_TYPE_P (op00type))
	    {
	      tree field = TYPE_FIELDS (op00type);
	      for (; field; field = DECL_CHAIN (field))
		if (TREE_CODE (field) == FIELD_DECL
		    && tree_int_cst_equal (byte_position (field), op01)
		    && (same_type_ignoring_top_level_qualifiers_p
			(TREE_TYPE (field), type)))
		  {
		    return fold_build3 (COMPONENT_REF, type, op00,
				     field, NULL_TREE);
		    break;
		  }
	    }
	}
    }
  /* *(foo *)fooarrptr => (*fooarrptr)[0] */
  else if (TREE_CODE (TREE_TYPE (subtype)) == ARRAY_TYPE
	   && (same_type_ignoring_top_level_qualifiers_p
	       (type, TREE_TYPE (TREE_TYPE (subtype)))))
    {
      tree type_domain;
      tree min_val = size_zero_node;
      tree newsub = cxx_fold_indirect_ref (loc, TREE_TYPE (subtype), sub, NULL);
      if (newsub)
	sub = newsub;
      else
	sub = build1_loc (loc, INDIRECT_REF, TREE_TYPE (subtype), sub);
      type_domain = TYPE_DOMAIN (TREE_TYPE (sub));
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      return build4_loc (loc, ARRAY_REF, type, sub, min_val, NULL_TREE,
			 NULL_TREE);
    }

  return NULL_TREE;
}

static tree
cxx_eval_indirect_ref (const constexpr_call *call, tree t,
		       bool allow_non_constant, bool addr,
		       bool *non_constant_p, bool *overflow_p)
{
  tree orig_op0 = TREE_OPERAND (t, 0);
  tree op0 = cxx_eval_constant_expression (call, orig_op0, allow_non_constant,
					   /*addr*/false, non_constant_p, overflow_p);
  bool empty_base = false;
  tree r;

  /* Don't VERIFY_CONSTANT here.  */
  if (*non_constant_p)
    return t;

  r = cxx_fold_indirect_ref (EXPR_LOCATION (t), TREE_TYPE (t), op0,
			     &empty_base);

  if (r)
    r = cxx_eval_constant_expression (call, r, allow_non_constant,
				      addr, non_constant_p, overflow_p);
  else
    {
      tree sub = op0;
      STRIP_NOPS (sub);
      if (TREE_CODE (sub) == ADDR_EXPR)
	{
	  /* We couldn't fold to a constant value.  Make sure it's not
	     something we should have been able to fold.  */
	  gcc_assert (!same_type_ignoring_top_level_qualifiers_p
		      (TREE_TYPE (TREE_TYPE (sub)), TREE_TYPE (t)));
	  /* DR 1188 says we don't have to deal with this.  */
	  if (!allow_non_constant)
	    error ("accessing value of %qE through a %qT glvalue in a "
		   "constant expression", build_fold_indirect_ref (sub),
		   TREE_TYPE (t));
	  *non_constant_p = true;
	  return t;
	}
    }

  /* If we're pulling out the value of an empty base, make sure
     that the whole object is constant and then return an empty
     CONSTRUCTOR.  */
  if (empty_base)
    {
      VERIFY_CONSTANT (r);
      r = build_constructor (TREE_TYPE (t), NULL);
      TREE_CONSTANT (r) = true;
    }

  if (r == NULL_TREE)
    {
      if (addr && op0 != orig_op0)
	return build1 (INDIRECT_REF, TREE_TYPE (t), op0);
      if (!addr)
	VERIFY_CONSTANT (t);
      return t;
    }
  return r;
}

/* Complain about R, a VAR_DECL, not being usable in a constant expression.
   Shared between potential_constant_expression and
   cxx_eval_constant_expression.  */

static void
non_const_var_error (tree r)
{
  tree type = TREE_TYPE (r);
  error ("the value of %qD is not usable in a constant "
	 "expression", r);
  /* Avoid error cascade.  */
  if (DECL_INITIAL (r) == error_mark_node)
    return;
  if (DECL_DECLARED_CONSTEXPR_P (r))
    inform (DECL_SOURCE_LOCATION (r),
	    "%qD used in its own initializer", r);
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    {
      if (!CP_TYPE_CONST_P (type))
	inform (DECL_SOURCE_LOCATION (r),
		"%q#D is not const", r);
      else if (CP_TYPE_VOLATILE_P (type))
	inform (DECL_SOURCE_LOCATION (r),
		"%q#D is volatile", r);
      else if (!DECL_INITIAL (r)
	       || !TREE_CONSTANT (DECL_INITIAL (r)))
	inform (DECL_SOURCE_LOCATION (r),
		"%qD was not initialized with a constant "
		"expression", r);
      else
	gcc_unreachable ();
    }
  else
    {
      if (cxx_dialect >= cxx11 && !DECL_DECLARED_CONSTEXPR_P (r))
	inform (DECL_SOURCE_LOCATION (r),
		"%qD was not declared %<constexpr%>", r);
      else
	inform (DECL_SOURCE_LOCATION (r),
		"%qD does not have integral or enumeration type",
		r);
    }
}

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for trinary expressions.  */

static tree
cxx_eval_trinary_expression (const constexpr_call *call, tree t,
			     bool allow_non_constant, bool addr,
			     bool *non_constant_p, bool *overflow_p)
{
  int i;
  tree args[3];
  tree val;

  for (i = 0; i < 3; i++)
    {
      args[i] = cxx_eval_constant_expression (call, TREE_OPERAND (t, i),
					      allow_non_constant, addr,
					      non_constant_p, overflow_p);
      VERIFY_CONSTANT (args[i]);
    }

  val = fold_ternary_loc (EXPR_LOCATION (t), TREE_CODE (t), TREE_TYPE (t),
			  args[0], args[1], args[2]);
  if (val == NULL_TREE)
    return t;
  VERIFY_CONSTANT (val);
  return val;
}

bool
var_in_constexpr_fn (tree t)
{
  tree ctx = DECL_CONTEXT (t);
  return (cxx_dialect >= cxx14 && ctx && TREE_CODE (ctx) == FUNCTION_DECL
	  && DECL_DECLARED_CONSTEXPR_P (ctx));
}

/* Attempt to reduce the expression T to a constant value.
   On failure, issue diagnostic and return error_mark_node.  */
/* FIXME unify with c_fully_fold */

static tree
cxx_eval_constant_expression (const constexpr_call *call, tree t,
			      bool allow_non_constant, bool addr,
			      bool *non_constant_p, bool *overflow_p)
{
  tree r = t;

  if (t == error_mark_node)
    {
      *non_constant_p = true;
      return t;
    }
  if (CONSTANT_CLASS_P (t))
    {
      if (TREE_CODE (t) == PTRMEM_CST)
	t = cplus_expand_constant (t);
      else if (TREE_OVERFLOW (t) && (!flag_permissive || allow_non_constant))
	*overflow_p = true;
      return t;
    }
  if (TREE_CODE (t) != NOP_EXPR
      && reduced_constant_expression_p (t))
    return fold (t);

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      if (addr)
	return t;
      /* else fall through. */
    case CONST_DECL:
      r = integral_constant_value (t);
      if (TREE_CODE (r) == TARGET_EXPR
	  && TREE_CODE (TARGET_EXPR_INITIAL (r)) == CONSTRUCTOR)
	r = TARGET_EXPR_INITIAL (r);
      if (DECL_P (r) && var_in_constexpr_fn (r)
	  && DECL_INITIAL (r))
	r = cxx_eval_constant_expression (call, DECL_INITIAL (r),
					  allow_non_constant, false,
					  non_constant_p, overflow_p);
      if (DECL_P (r))
	{
	  if (!allow_non_constant)
	    non_const_var_error (r);
	  *non_constant_p = true;
	}
      break;

    case FUNCTION_DECL:
    case TEMPLATE_DECL:
    case LABEL_DECL:
      return t;

    case PARM_DECL:
      if (call && DECL_CONTEXT (t) == call->fundef->decl)
	{
	  if (DECL_ARTIFICIAL (t) && DECL_CONSTRUCTOR_P (DECL_CONTEXT (t)))
	    {
	      if (!allow_non_constant)
		sorry ("use of the value of the object being constructed "
		       "in a constant expression");
	      *non_constant_p = true;
	    }
	  else
	    r = lookup_parameter_binding (call, t);
	}
      else if (addr)
	/* Defer in case this is only used for its type.  */;
      else
	{
	  if (!allow_non_constant)
	    error ("%qE is not a constant expression", t);
	  *non_constant_p = true;
	}
      break;

    case CALL_EXPR:
    case AGGR_INIT_EXPR:
      r = cxx_eval_call_expression (call, t, allow_non_constant, addr,
				    non_constant_p, overflow_p);
      break;

    case TARGET_EXPR:
      if (!literal_type_p (TREE_TYPE (t)))
	{
	  if (!allow_non_constant)
	    {
	      error ("temporary of non-literal type %qT in a "
		     "constant expression", TREE_TYPE (t));
	      explain_non_literal_class (TREE_TYPE (t));
	    }
	  *non_constant_p = true;
	  break;
	}
      /* else fall through.  */
    case INIT_EXPR:
      /* Pass false for 'addr' because these codes indicate
	 initialization of a temporary.  */
      r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
					allow_non_constant, false,
					non_constant_p, overflow_p);
      if (!*non_constant_p)
	/* Adjust the type of the result to the type of the temporary.  */
	r = adjust_temp_type (TREE_TYPE (t), r);
      break;

    case SCOPE_REF:
      r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
					allow_non_constant, addr,
					non_constant_p, overflow_p);
      break;

    case RETURN_EXPR:
    case NON_LVALUE_EXPR:
    case TRY_CATCH_EXPR:
    case CLEANUP_POINT_EXPR:
    case MUST_NOT_THROW_EXPR:
    case SAVE_EXPR:
      r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 0),
					allow_non_constant, addr,
					non_constant_p, overflow_p);
      break;

      /* These differ from cxx_eval_unary_expression in that this doesn't
	 check for a constant operand or result; an address can be
	 constant without its operand being, and vice versa.  */
    case INDIRECT_REF:
      r = cxx_eval_indirect_ref (call, t, allow_non_constant, addr,
				 non_constant_p, overflow_p);
      break;

    case ADDR_EXPR:
      {
	tree oldop = TREE_OPERAND (t, 0);
	tree op = cxx_eval_constant_expression (call, oldop,
						allow_non_constant,
						/*addr*/true,
						non_constant_p, overflow_p);
	/* Don't VERIFY_CONSTANT here.  */
	if (*non_constant_p)
	  return t;
	/* This function does more aggressive folding than fold itself.  */
	r = build_fold_addr_expr_with_type (op, TREE_TYPE (t));
	if (TREE_CODE (r) == ADDR_EXPR && TREE_OPERAND (r, 0) == oldop)
	  return t;
	break;
      }

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case CONJ_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
      r = cxx_eval_unary_expression (call, t, allow_non_constant, addr,
				     non_constant_p, overflow_p);
      break;

    case SIZEOF_EXPR:
      if (SIZEOF_EXPR_TYPE_P (t))
	r = cxx_sizeof_or_alignof_type (TREE_TYPE (TREE_OPERAND (t, 0)),
					SIZEOF_EXPR, false);
      else if (TYPE_P (TREE_OPERAND (t, 0)))
	r = cxx_sizeof_or_alignof_type (TREE_OPERAND (t, 0), SIZEOF_EXPR,
					false);
      else
	r = cxx_sizeof_or_alignof_expr (TREE_OPERAND (t, 0), SIZEOF_EXPR,
					false);
      if (r == error_mark_node)
	r = size_one_node;
      VERIFY_CONSTANT (r);
      break;

    case COMPOUND_EXPR:
      {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  Also handle EMPTY_CLASS_EXPR
	   introduced by build_call_a.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if ((TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	    || TREE_CODE (op1) == EMPTY_CLASS_EXPR)
	  r = cxx_eval_constant_expression (call, op0, allow_non_constant,
					    addr, non_constant_p, overflow_p);
	else
	  {
	    /* Check that the LHS is constant and then discard it.  */
	    cxx_eval_constant_expression (call, op0, allow_non_constant,
					  false, non_constant_p, overflow_p);
	    op1 = TREE_OPERAND (t, 1);
	    r = cxx_eval_constant_expression (call, op1, allow_non_constant,
					      addr, non_constant_p, overflow_p);
	  }
      }
      break;

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      r = cxx_eval_binary_expression (call, t, allow_non_constant, addr,
				      non_constant_p, overflow_p);
      break;

      /* fold can introduce non-IF versions of these; still treat them as
	 short-circuiting.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      r = cxx_eval_logical_expression (call, t, boolean_false_node,
				       boolean_true_node,
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
      break;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      r = cxx_eval_logical_expression (call, t, boolean_true_node,
				       boolean_false_node,
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
      break;

    case ARRAY_REF:
      r = cxx_eval_array_reference (call, t, allow_non_constant, addr,
				    non_constant_p, overflow_p);
      break;

    case COMPONENT_REF:
      if (is_overloaded_fn (t))
	{
	  /* We can only get here in checking mode via 
	     build_non_dependent_expr,  because any expression that
	     calls or takes the address of the function will have
	     pulled a FUNCTION_DECL out of the COMPONENT_REF.  */
	  gcc_checking_assert (allow_non_constant || errorcount);
	  *non_constant_p = true;
	  return t;
	}
      r = cxx_eval_component_reference (call, t, allow_non_constant, addr,
					non_constant_p, overflow_p);
      break;

    case BIT_FIELD_REF:
      r = cxx_eval_bit_field_ref (call, t, allow_non_constant, addr,
				  non_constant_p, overflow_p);
      break;

    case COND_EXPR:
    case VEC_COND_EXPR:
      r = cxx_eval_conditional_expression (call, t, allow_non_constant, addr,
					   non_constant_p, overflow_p);
      break;

    case CONSTRUCTOR:
      r = cxx_eval_bare_aggregate (call, t, allow_non_constant, addr,
				   non_constant_p, overflow_p);
      break;

    case VEC_INIT_EXPR:
      /* We can get this in a defaulted constructor for a class with a
	 non-static data member of array type.  Either the initializer will
	 be NULL, meaning default-initialization, or it will be an lvalue
	 or xvalue of the same type, meaning direct-initialization from the
	 corresponding member.  */
      r = cxx_eval_vec_init (call, t, allow_non_constant, addr,
			     non_constant_p, overflow_p);
      break;

    case FMA_EXPR:
    case VEC_PERM_EXPR:
      r = cxx_eval_trinary_expression (call, t, allow_non_constant, addr,
				       non_constant_p, overflow_p);
      break;

    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
    case NOP_EXPR:
      {
	tree oldop = TREE_OPERAND (t, 0);
	tree op = cxx_eval_constant_expression (call, oldop,
						allow_non_constant, addr,
						non_constant_p, overflow_p);
	if (*non_constant_p)
	  return t;
	if (POINTER_TYPE_P (TREE_TYPE (t))
	    && TREE_CODE (op) == INTEGER_CST
	    && !integer_zerop (op))
	  {
	    if (!allow_non_constant)
	      error_at (EXPR_LOC_OR_LOC (t, input_location),
			"reinterpret_cast from integer to pointer");
	    *non_constant_p = true;
	    return t;
	  }
	if (op == oldop)
	  /* We didn't fold at the top so we could check for ptr-int
	     conversion.  */
	  return fold (t);
	r = fold_build1 (TREE_CODE (t), TREE_TYPE (t), op);
	/* Conversion of an out-of-range value has implementation-defined
	   behavior; the language considers it different from arithmetic
	   overflow, which is undefined.  */
	if (TREE_OVERFLOW_P (r) && !TREE_OVERFLOW_P (op))
	  TREE_OVERFLOW (r) = false;
      }
      break;

    case EMPTY_CLASS_EXPR:
      /* This is good enough for a function argument that might not get
	 used, and they can't do anything with it, so just return it.  */
      return t;

    case LAMBDA_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case THROW_EXPR:
    case MODIFY_EXPR:
    case MODOP_EXPR:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case OBJ_TYPE_REF:
    case WITH_CLEANUP_EXPR:
    case STATEMENT_LIST:
    case BIND_EXPR:
    case NON_DEPENDENT_EXPR:
    case BASELINK:
    case EXPR_STMT:
    case OFFSET_REF:
      if (!allow_non_constant)
        error_at (EXPR_LOC_OR_LOC (t, input_location),
		  "expression %qE is not a constant-expression", t);
      *non_constant_p = true;
      break;

    default:
      internal_error ("unexpected expression %qE of kind %s", t,
		      get_tree_code_name (TREE_CODE (t)));
      *non_constant_p = true;
      break;
    }

  if (r == error_mark_node)
    *non_constant_p = true;

  if (*non_constant_p)
    return t;
  else
    return r;
}

static tree
cxx_eval_outermost_constant_expr (tree t, bool allow_non_constant)
{
  bool non_constant_p = false;
  bool overflow_p = false;
  tree r = cxx_eval_constant_expression (NULL, t, allow_non_constant,
					 false, &non_constant_p, &overflow_p);

  verify_constant (r, allow_non_constant, &non_constant_p, &overflow_p);

  if (TREE_CODE (t) != CONSTRUCTOR
      && cp_has_mutable_p (TREE_TYPE (t)))
    {
      /* We allow a mutable type if the original expression was a
	 CONSTRUCTOR so that we can do aggregate initialization of
	 constexpr variables.  */
      if (!allow_non_constant)
	error ("%qT cannot be the type of a complete constant expression "
	       "because it has mutable sub-objects", TREE_TYPE (t));
      non_constant_p = true;
    }

  /* Technically we should check this for all subexpressions, but that
     runs into problems with our internal representation of pointer
     subtraction and the 5.19 rules are still in flux.  */
  if (CONVERT_EXPR_CODE_P (TREE_CODE (r))
      && ARITHMETIC_TYPE_P (TREE_TYPE (r))
      && TREE_CODE (TREE_OPERAND (r, 0)) == ADDR_EXPR)
    {
      if (!allow_non_constant)
	error ("conversion from pointer type %qT "
	       "to arithmetic type %qT in a constant-expression",
	       TREE_TYPE (TREE_OPERAND (r, 0)), TREE_TYPE (r));
      non_constant_p = true;
    }

  if (!non_constant_p && overflow_p)
    non_constant_p = true;

  if (non_constant_p && !allow_non_constant)
    return error_mark_node;
  else if (non_constant_p && TREE_CONSTANT (r))
    {
      /* This isn't actually constant, so unset TREE_CONSTANT.  */
      if (EXPR_P (r))
	r = copy_node (r);
      else if (TREE_CODE (r) == CONSTRUCTOR)
	r = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (r), r);
      else
	r = build_nop (TREE_TYPE (r), r);
      TREE_CONSTANT (r) = false;
    }
  else if (non_constant_p || r == t)
    return t;

  if (TREE_CODE (r) == CONSTRUCTOR && CLASS_TYPE_P (TREE_TYPE (r)))
    {
      if (TREE_CODE (t) == TARGET_EXPR
	  && TARGET_EXPR_INITIAL (t) == r)
	return t;
      else
	{
	  r = get_target_expr (r);
	  TREE_CONSTANT (r) = true;
	  return r;
	}
    }
  else
    return r;
}

/* Returns true if T is a valid subexpression of a constant expression,
   even if it isn't itself a constant expression.  */

bool
is_sub_constant_expr (tree t)
{
  bool non_constant_p = false;
  bool overflow_p = false;
  cxx_eval_constant_expression (NULL, t, true, false, &non_constant_p,
				&overflow_p);
  return !non_constant_p && !overflow_p;
}

/* If T represents a constant expression returns its reduced value.
   Otherwise return error_mark_node.  If T is dependent, then
   return NULL.  */

tree
cxx_constant_value (tree t)
{
  return cxx_eval_outermost_constant_expr (t, false);
}

/* If T is a constant expression, returns its reduced value.
   Otherwise, if T does not have TREE_CONSTANT set, returns T.
   Otherwise, returns a version of T without TREE_CONSTANT.  */

tree
maybe_constant_value (tree t)
{
  tree r;

  if (instantiation_dependent_expression_p (t)
      || type_unknown_p (t)
      || BRACE_ENCLOSED_INITIALIZER_P (t)
      || !potential_constant_expression (t))
    {
      if (TREE_OVERFLOW_P (t))
	{
	  t = build_nop (TREE_TYPE (t), t);
	  TREE_CONSTANT (t) = false;
	}
      return t;
    }

  r = cxx_eval_outermost_constant_expr (t, true);
#ifdef ENABLE_CHECKING
  /* cp_tree_equal looks through NOPs, so allow them.  */
  gcc_assert (r == t
	      || CONVERT_EXPR_P (t)
	      || (TREE_CONSTANT (t) && !TREE_CONSTANT (r))
	      || !cp_tree_equal (r, t));
#endif
  return r;
}

/* Like maybe_constant_value, but returns a CONSTRUCTOR directly, rather
   than wrapped in a TARGET_EXPR.  */

tree
maybe_constant_init (tree t)
{
  if (TREE_CODE (t) == EXPR_STMT)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == CONVERT_EXPR
      && VOID_TYPE_P (TREE_TYPE (t)))
    t = TREE_OPERAND (t, 0);
  t = maybe_constant_value (t);
  if (TREE_CODE (t) == TARGET_EXPR)
    {
      tree init = TARGET_EXPR_INITIAL (t);
      if (TREE_CODE (init) == CONSTRUCTOR)
	t = init;
    }
  return t;
}

#if 0
/* FIXME see ADDR_EXPR section in potential_constant_expression_1.  */
/* Return true if the object referred to by REF has automatic or thread
   local storage.  */

enum { ck_ok, ck_bad, ck_unknown };
static int
check_automatic_or_tls (tree ref)
{
  enum machine_mode mode;
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  int volatilep = 0, unsignedp = 0;
  tree decl = get_inner_reference (ref, &bitsize, &bitpos, &offset,
				   &mode, &unsignedp, &volatilep, false);
  duration_kind dk;

  /* If there isn't a decl in the middle, we don't know the linkage here,
     and this isn't a constant expression anyway.  */
  if (!DECL_P (decl))
    return ck_unknown;
  dk = decl_storage_duration (decl);
  return (dk == dk_auto || dk == dk_thread) ? ck_bad : ck_ok;
}
#endif

/* Return true if T denotes a potentially constant expression.  Issue
   diagnostic as appropriate under control of FLAGS.  If WANT_RVAL is true,
   an lvalue-rvalue conversion is implied.

   C++0x [expr.const] used to say

   6 An expression is a potential constant expression if it is
     a constant expression where all occurrences of function
     parameters are replaced by arbitrary constant expressions
     of the appropriate type.

   2  A conditional expression is a constant expression unless it
      involves one of the following as a potentially evaluated
      subexpression (3.2), but subexpressions of logical AND (5.14),
      logical OR (5.15), and conditional (5.16) operations that are
      not evaluated are not considered.   */

static bool
potential_constant_expression_1 (tree t, bool want_rval, tsubst_flags_t flags)
{
  enum { any = false, rval = true };
  int i;
  tree tmp;

  if (t == error_mark_node)
    return false;
  if (t == NULL_TREE)
    return true;
  if (TREE_THIS_VOLATILE (t))
    {
      if (flags & tf_error)
        error ("expression %qE has side-effects", t);
      return false;
    }
  if (CONSTANT_CLASS_P (t))
    return true;

  switch (TREE_CODE (t))
    {
    case FUNCTION_DECL:
    case BASELINK:
    case TEMPLATE_DECL:
    case OVERLOAD:
    case TEMPLATE_ID_EXPR:
    case LABEL_DECL:
    case LABEL_EXPR:
    case CONST_DECL:
    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
    case OFFSETOF_EXPR:
    case NOEXCEPT_EXPR:
    case TEMPLATE_PARM_INDEX:
    case TRAIT_EXPR:
    case IDENTIFIER_NODE:
    case USERDEF_LITERAL:
      /* We can see a FIELD_DECL in a pointer-to-member expression.  */
    case FIELD_DECL:
    case PARM_DECL:
    case USING_DECL:
      return true;

    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      /* -- an invocation of a function other than a constexpr function
            or a constexpr constructor.  */
      {
        tree fun = get_function_named_in_call (t);
        const int nargs = call_expr_nargs (t);
	i = 0;

	if (is_overloaded_fn (fun))
	  {
	    if (TREE_CODE (fun) == FUNCTION_DECL)
	      {
		if (builtin_valid_in_constant_expr_p (fun))
		  return true;
		if (!DECL_DECLARED_CONSTEXPR_P (fun)
		    /* Allow any built-in function; if the expansion
		       isn't constant, we'll deal with that then.  */
		    && !is_builtin_fn (fun))
		  {
		    if (flags & tf_error)
		      {
			error_at (EXPR_LOC_OR_LOC (t, input_location),
				  "call to non-constexpr function %qD", fun);
			explain_invalid_constexpr_fn (fun);
		      }
		    return false;
		  }
		/* A call to a non-static member function takes the address
		   of the object as the first argument.  But in a constant
		   expression the address will be folded away, so look
		   through it now.  */
		if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fun)
		    && !DECL_CONSTRUCTOR_P (fun))
		  {
		    tree x = get_nth_callarg (t, 0);
		    if (is_this_parameter (x))
		      {
			if (DECL_CONTEXT (x) == NULL_TREE
			    || DECL_CONSTRUCTOR_P (DECL_CONTEXT (x)))
			  {
			    if (flags & tf_error)
			      sorry ("calling a member function of the "
				     "object being constructed in a constant "
				     "expression");
			    return false;
			  }
			/* Otherwise OK.  */;
		      }
		    else if (!potential_constant_expression_1 (x, rval, flags))
		      return false;
		    i = 1;
		  }
	      }
	    else
	      {
		if (!potential_constant_expression_1 (fun, true, flags))
		  return false;
		fun = get_first_fn (fun);
	      }
	    /* Skip initial arguments to base constructors.  */
	    if (DECL_BASE_CONSTRUCTOR_P (fun))
	      i = num_artificial_parms_for (fun);
	    fun = DECL_ORIGIN (fun);
	  }
	else
          {
	    if (potential_constant_expression_1 (fun, rval, flags))
	      /* Might end up being a constant function pointer.  */;
	    else
	      return false;
          }
        for (; i < nargs; ++i)
          {
            tree x = get_nth_callarg (t, i);
	    if (!potential_constant_expression_1 (x, rval, flags))
	      return false;
          }
        return true;
      }

    case NON_LVALUE_EXPR:
      /* -- an lvalue-to-rvalue conversion (4.1) unless it is applied to
            -- an lvalue of integral type that refers to a non-volatile
               const variable or static data member initialized with
               constant expressions, or

            -- an lvalue of literal type that refers to non-volatile
               object defined with constexpr, or that refers to a
               sub-object of such an object;  */
      return potential_constant_expression_1 (TREE_OPERAND (t, 0), rval, flags);

    case VAR_DECL:
      if (want_rval && !decl_constant_var_p (t)
	  && !var_in_constexpr_fn (t)
	  && !dependent_type_p (TREE_TYPE (t)))
        {
          if (flags & tf_error)
            non_const_var_error (t);
          return false;
        }
      return true;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
      /* -- a reinterpret_cast.  FIXME not implemented, and this rule
	 may change to something more specific to type-punning (DR 1312).  */
      {
        tree from = TREE_OPERAND (t, 0);
	if (POINTER_TYPE_P (TREE_TYPE (t))
	    && TREE_CODE (from) == INTEGER_CST
	    && !integer_zerop (from))
	  {
	    if (flags & tf_error)
	      error_at (EXPR_LOC_OR_LOC (t, input_location),
			"reinterpret_cast from integer to pointer");
	    return false;
	  }
        return (potential_constant_expression_1
		(from, TREE_CODE (t) != VIEW_CONVERT_EXPR, flags));
      }

    case ADDR_EXPR:
      /* -- a unary operator & that is applied to an lvalue that
            designates an object with thread or automatic storage
            duration;  */
      t = TREE_OPERAND (t, 0);

      if (TREE_CODE (t) == OFFSET_REF && PTRMEM_OK_P (t))
	/* A pointer-to-member constant.  */
	return true;

#if 0
      /* FIXME adjust when issue 1197 is fully resolved.  For now don't do
         any checking here, as we might dereference the pointer later.  If
         we remove this code, also remove check_automatic_or_tls.  */
      i = check_automatic_or_tls (t);
      if (i == ck_ok)
	return true;
      if (i == ck_bad)
        {
          if (flags & tf_error)
            error ("address-of an object %qE with thread local or "
                   "automatic storage is not a constant expression", t);
          return false;
        }
#endif
      return potential_constant_expression_1 (t, any, flags);

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARROW_EXPR:
    case OFFSET_REF:
      /* -- a class member access unless its postfix-expression is
            of literal type or of pointer to literal type.  */
      /* This test would be redundant, as it follows from the
	 postfix-expression being a potential constant expression.  */
      return potential_constant_expression_1 (TREE_OPERAND (t, 0),
					      want_rval, flags);

    case EXPR_PACK_EXPANSION:
      return potential_constant_expression_1 (PACK_EXPANSION_PATTERN (t),
					      want_rval, flags);

    case INDIRECT_REF:
      {
        tree x = TREE_OPERAND (t, 0);
        STRIP_NOPS (x);
        if (is_this_parameter (x))
	  {
	    if (DECL_CONTEXT (x)
		&& !DECL_DECLARED_CONSTEXPR_P (DECL_CONTEXT (x)))
	      {
		if (flags & tf_error)
		  error ("use of %<this%> in a constant expression");
		return false;
	      }
	    if (want_rval && DECL_CONTEXT (x)
		&& DECL_CONSTRUCTOR_P (DECL_CONTEXT (x)))
	      {
		if (flags & tf_error)
		  sorry ("use of the value of the object being constructed "
			 "in a constant expression");
		return false;
	      }
	    return true;
	  }
	return potential_constant_expression_1 (x, rval, flags);
      }

    case LAMBDA_EXPR:
    case DYNAMIC_CAST_EXPR:
    case PSEUDO_DTOR_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case THROW_EXPR:
    case MODIFY_EXPR:
    case MODOP_EXPR:
    case OMP_ATOMIC:
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case OBJ_TYPE_REF:
    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
    case MUST_NOT_THROW_EXPR:
    case TRY_CATCH_EXPR:
    case STATEMENT_LIST:
      /* Don't bother trying to define a subset of statement-expressions to
	 be constant-expressions, at least for now.  */
    case STMT_EXPR:
    case EXPR_STMT:
    case BIND_EXPR:
    case TRANSACTION_EXPR:
    case IF_STMT:
    case DO_STMT:
    case FOR_STMT:
    case WHILE_STMT:
    case DECL_EXPR:
      if (flags & tf_error)
        error ("expression %qE is not a constant-expression", t);
      return false;

    case TYPEID_EXPR:
      /* -- a typeid expression whose operand is of polymorphic
            class type;  */
      {
        tree e = TREE_OPERAND (t, 0);
        if (!TYPE_P (e) && !type_dependent_expression_p (e)
	    && TYPE_POLYMORPHIC_P (TREE_TYPE (e)))
          {
            if (flags & tf_error)
              error ("typeid-expression is not a constant expression "
                     "because %qE is of polymorphic type", e);
            return false;
          }
        return true;
      }

    case MINUS_EXPR:
      /* -- a subtraction where both operands are pointers.   */
      if (TYPE_PTR_P (TREE_OPERAND (t, 0))
          && TYPE_PTR_P (TREE_OPERAND (t, 1)))
        {
          if (flags & tf_error)
            error ("difference of two pointer expressions is not "
                   "a constant expression");
          return false;
        }
      want_rval = true;
      goto binary;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      /* -- a relational or equality operator where at least
            one of the operands is a pointer.  */
      if (TYPE_PTR_P (TREE_OPERAND (t, 0))
          || TYPE_PTR_P (TREE_OPERAND (t, 1)))
        {
          if (flags & tf_error)
            error ("pointer comparison expression is not a "
                   "constant expression");
          return false;
        }
      want_rval = true;
      goto binary;

    case BIT_NOT_EXPR:
      /* A destructor.  */
      if (TYPE_P (TREE_OPERAND (t, 0)))
	return true;
      /* else fall through.  */

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case CONJ_EXPR:
    case SAVE_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    case UNARY_PLUS_EXPR:
      return potential_constant_expression_1 (TREE_OPERAND (t, 0), rval,
					      flags);

    case CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      if (cxx_dialect < cxx11
	  && !dependent_type_p (TREE_TYPE (t))
	  && !INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (t)))
	/* In C++98, a conversion to non-integral type can't be part of a
	   constant expression.  */
	{
	  if (flags & tf_error)
	    error ("cast to non-integral type %qT in a constant expression",
		   TREE_TYPE (t));
	  return false;
	}

      return (potential_constant_expression_1
	      (TREE_OPERAND (t, 0),
	       TREE_CODE (TREE_TYPE (t)) != REFERENCE_TYPE, flags));

    case PAREN_EXPR:
    case NON_DEPENDENT_EXPR:
      /* For convenience.  */
    case RETURN_EXPR:
      return potential_constant_expression_1 (TREE_OPERAND (t, 0),
					      want_rval, flags);

    case SCOPE_REF:
      return potential_constant_expression_1 (TREE_OPERAND (t, 1),
					      want_rval, flags);

    case TARGET_EXPR:
      if (!literal_type_p (TREE_TYPE (t)))
	{
	  if (flags & tf_error)
	    {
	      error ("temporary of non-literal type %qT in a "
		     "constant expression", TREE_TYPE (t));
	      explain_non_literal_class (TREE_TYPE (t));
	    }
	  return false;
	}
    case INIT_EXPR:
      return potential_constant_expression_1 (TREE_OPERAND (t, 1),
					      rval, flags);

    case CONSTRUCTOR:
      {
        vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
        constructor_elt *ce;
        for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
	  if (!potential_constant_expression_1 (ce->value, want_rval, flags))
	    return false;
	return true;
      }

    case TREE_LIST:
      {
	gcc_assert (TREE_PURPOSE (t) == NULL_TREE
		    || DECL_P (TREE_PURPOSE (t)));
	if (!potential_constant_expression_1 (TREE_VALUE (t), want_rval,
					      flags))
	  return false;
	if (TREE_CHAIN (t) == NULL_TREE)
	  return true;
	return potential_constant_expression_1 (TREE_CHAIN (t), want_rval,
						flags);
      }

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      {
	tree denom = TREE_OPERAND (t, 1);
	if (!potential_constant_expression_1 (denom, rval, flags))
	  return false;
	/* We can't call cxx_eval_outermost_constant_expr on an expression
	   that hasn't been through fold_non_dependent_expr yet.  */
	if (!processing_template_decl)
	  denom = cxx_eval_outermost_constant_expr (denom, true);
	if (integer_zerop (denom))
	  {
	    if (flags & tf_error)
	      error ("division by zero is not a constant-expression");
	    return false;
	  }
	else
	  {
	    want_rval = true;
	    return potential_constant_expression_1 (TREE_OPERAND (t, 0),
						    want_rval, flags);
	  }
      }

    case COMPOUND_EXPR:
      {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  Also handle EMPTY_CLASS_EXPR
	   introduced by build_call_a.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if ((TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	    || TREE_CODE (op1) == EMPTY_CLASS_EXPR)
	  return potential_constant_expression_1 (op0, want_rval, flags);
	else
	  goto binary;
      }

      /* If the first operand is the non-short-circuit constant, look at
	 the second operand; otherwise we only care about the first one for
	 potentiality.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      tmp = boolean_true_node;
      goto truth;
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      tmp = boolean_false_node;
    truth:
      {
	tree op = TREE_OPERAND (t, 0);
	if (!potential_constant_expression_1 (op, rval, flags))
	  return false;
	if (!processing_template_decl)
	  op = cxx_eval_outermost_constant_expr (op, true);
	if (tree_int_cst_equal (op, tmp))
	  return potential_constant_expression_1 (TREE_OPERAND (t, 1), rval, flags);
	else
	  return true;
      }

    case PLUS_EXPR:
    case MULT_EXPR:
    case POINTER_PLUS_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      want_rval = true;
      /* Fall through.  */
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
    binary:
      for (i = 0; i < 2; ++i)
	if (!potential_constant_expression_1 (TREE_OPERAND (t, i),
					      want_rval, flags))
	  return false;
      return true;

    case CILK_SYNC_STMT:
    case CILK_SPAWN_STMT:
    case ARRAY_NOTATION_REF:
      return false;

    case FMA_EXPR:
    case VEC_PERM_EXPR:
     for (i = 0; i < 3; ++i)
      if (!potential_constant_expression_1 (TREE_OPERAND (t, i),
					    true, flags))
	return false;
     return true;

    case COND_EXPR:
    case VEC_COND_EXPR:
      /* If the condition is a known constant, we know which of the legs we
	 care about; otherwise we only require that the condition and
	 either of the legs be potentially constant.  */
      tmp = TREE_OPERAND (t, 0);
      if (!potential_constant_expression_1 (tmp, rval, flags))
	return false;
      if (!processing_template_decl)
	tmp = cxx_eval_outermost_constant_expr (tmp, true);
      if (integer_zerop (tmp))
	return potential_constant_expression_1 (TREE_OPERAND (t, 2),
						want_rval, flags);
      else if (TREE_CODE (tmp) == INTEGER_CST)
	return potential_constant_expression_1 (TREE_OPERAND (t, 1),
						want_rval, flags);
      for (i = 1; i < 3; ++i)
	if (potential_constant_expression_1 (TREE_OPERAND (t, i),
					     want_rval, tf_none))
	  return true;
      if (flags & tf_error)
        error ("expression %qE is not a constant-expression", t);
      return false;

    case VEC_INIT_EXPR:
      if (VEC_INIT_EXPR_IS_CONSTEXPR (t))
	return true;
      if (flags & tf_error)
	{
	  error ("non-constant array initialization");
	  diagnose_non_constexpr_vec_init (t);
	}
      return false;

    default:
      if (objc_is_property_ref (t))
	return false;

      sorry ("unexpected AST of kind %s", get_tree_code_name (TREE_CODE (t)));
      gcc_unreachable();
      return false;
    }
}

/* The main entry point to the above.  */

bool
potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, tf_none);
}

/* As above, but require a constant rvalue.  */

bool
potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, true, tf_none);
}

/* Like above, but complain about non-constant expressions.  */

bool
require_potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, tf_warning_or_error);
}

/* Cross product of the above.  */

bool
require_potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, true, tf_warning_or_error);
}

#include "gt-cp-constexpr.h"
