/* Conversion of SESE regions to Polyhedra.
   Copyright (C) 2009-2015 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define USES_ISL

#include "config.h"

#ifdef HAVE_isl

#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "cfghooks.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "params.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-pass.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "domwalk.h"
#include "tree-ssa-propagate.h"

#include <isl/constraint.h>
#include <isl/set.h>
#include <isl/map.h>
#include <isl/union_map.h>
#include <isl/constraint.h>
#include <isl/aff.h>
#include <isl/val.h>

/* Since ISL-0.13, the extern is in val_gmp.h.  */
#if !defined(HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE) && defined(__cplusplus)
extern "C" {
#endif
#include <isl/val_gmp.h>
#if !defined(HAVE_ISL_SCHED_CONSTRAINTS_COMPUTE_SCHEDULE) && defined(__cplusplus)
}
#endif

#include "graphite.h"

/* Assigns to RES the value of the INTEGER_CST T.  */

static inline void
tree_int_to_gmp (tree t, mpz_t res)
{
  wi::to_mpz (t, res, TYPE_SIGN (TREE_TYPE (t)));
}

/* Return an ISL identifier for the polyhedral basic block PBB.  */

static isl_id *
isl_id_for_pbb (scop_p s, poly_bb_p pbb)
{
  char name[10];
  snprintf (name, sizeof (name), "S_%d", pbb_index (pbb));
  return isl_id_alloc (s->isl_context, name, pbb);
}

/* Converts the STATIC_SCHEDULE of PBB into a scattering polyhedron.
   We generate SCATTERING_DIMENSIONS scattering dimensions.

   The scattering polyhedron consists of these dimensions: scattering,
   loop_iterators, parameters.

   Example:

   | scattering_dimensions = 5
   | nb_iterators = 1
   | scop_nb_params = 2
   |
   | Schedule:
   |   i
   | 4 5
   |
   | Scattering polyhedron:
   |
   | scattering: {s1, s2, s3, s4, s5}
   | loop_iterators: {i}
   | parameters: {p1, p2}
   |
   | s1  s2  s3  s4  s5  i   p1  p2  1
   | 1   0   0   0   0   0   0   0  -4  = 0
   | 0   1   0   0   0  -1   0   0   0  = 0
   | 0   0   1   0   0   0   0   0  -5  = 0  */

static void
build_pbb_scattering_polyhedrons (isl_aff *static_sched,
				  poly_bb_p pbb)
{
  isl_val *val;

  int scattering_dimensions = isl_set_dim (pbb->domain, isl_dim_set) * 2 + 1;

  isl_space *dc = isl_set_get_space (pbb->domain);
  isl_space *dm = isl_space_add_dims (isl_space_from_domain (dc),
				      isl_dim_out, scattering_dimensions);
  pbb->schedule = isl_map_universe (dm);

  for (int i = 0; i < scattering_dimensions; i++)
    {
      /* Textual order inside this loop.  */
      if ((i % 2) == 0)
	{
	  isl_constraint *c = isl_equality_alloc
	      (isl_local_space_from_space (isl_map_get_space (pbb->schedule)));

	  val = isl_aff_get_coefficient_val (static_sched, isl_dim_in, i / 2);
	  gcc_assert (val && isl_val_is_int (val));

	  val = isl_val_neg (val);
	  c = isl_constraint_set_constant_val (c, val);
	  c = isl_constraint_set_coefficient_si (c, isl_dim_out, i, 1);
	  pbb->schedule = isl_map_add_constraint (pbb->schedule, c);
	}

      /* Iterations of this loop.  */
      else /* if ((i % 2) == 1) */
	{
	  int loop = (i - 1) / 2;
	  pbb->schedule = isl_map_equate (pbb->schedule, isl_dim_in, loop,
					  isl_dim_out, i);
	}
    }

  pbb->transformed = isl_map_copy (pbb->schedule);
}

/* Build for BB the static schedule.

   The static schedule is a Dewey numbering of the abstract syntax
   tree: http://en.wikipedia.org/wiki/Dewey_Decimal_Classification

   The following example informally defines the static schedule:

   A
   for (i: ...)
     {
       for (j: ...)
         {
           B
           C
         }

       for (k: ...)
         {
           D
           E
         }
     }
   F

   Static schedules for A to F:

     DEPTH
     0 1 2
   A 0
   B 1 0 0
   C 1 0 1
   D 1 1 0
   E 1 1 1
   F 2
*/

static void
build_scop_scattering (scop_p scop)
{
  gimple_poly_bb_p previous_gbb = NULL;
  isl_space *dc = isl_set_get_space (scop->param_context);
  isl_aff *static_sched;

  dc = isl_space_add_dims (dc, isl_dim_set, number_of_loops (cfun));
  static_sched = isl_aff_zero_on_domain (isl_local_space_from_space (dc));

  /* We have to start schedules at 0 on the first component and
     because we cannot compare_prefix_loops against a previous loop,
     prefix will be equal to zero, and that index will be
     incremented before copying.  */
  static_sched = isl_aff_add_coefficient_si (static_sched, isl_dim_in, 0, -1);

  int i;
  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    {
      gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);
      int prefix = 0;

      if (previous_gbb)
	prefix = nb_common_loops (scop->scop_info->region, previous_gbb, gbb);

      previous_gbb = gbb;

      static_sched = isl_aff_add_coefficient_si (static_sched, isl_dim_in,
						 prefix, 1);
      build_pbb_scattering_polyhedrons (static_sched, pbb);
    }

  isl_aff_free (static_sched);
}

static isl_pw_aff *extract_affine (scop_p, tree, __isl_take isl_space *space);

/* Extract an affine expression from the chain of recurrence E.  */

static isl_pw_aff *
extract_affine_chrec (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_pw_aff *lhs = extract_affine (s, CHREC_LEFT (e), isl_space_copy (space));
  isl_pw_aff *rhs = extract_affine (s, CHREC_RIGHT (e), isl_space_copy (space));
  isl_local_space *ls = isl_local_space_from_space (space);
  unsigned pos = sese_loop_depth (s->scop_info->region, get_chrec_loop (e)) - 1;
  isl_aff *loop = isl_aff_set_coefficient_si
    (isl_aff_zero_on_domain (ls), isl_dim_in, pos, 1);
  isl_pw_aff *l = isl_pw_aff_from_aff (loop);

  /* Before multiplying, make sure that the result is affine.  */
  gcc_assert (isl_pw_aff_is_cst (rhs)
	      || isl_pw_aff_is_cst (l));

  return isl_pw_aff_add (lhs, isl_pw_aff_mul (rhs, l));
}

/* Extract an affine expression from the mult_expr E.  */

static isl_pw_aff *
extract_affine_mul (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_pw_aff *lhs = extract_affine (s, TREE_OPERAND (e, 0),
				    isl_space_copy (space));
  isl_pw_aff *rhs = extract_affine (s, TREE_OPERAND (e, 1), space);

  if (!isl_pw_aff_is_cst (lhs)
      && !isl_pw_aff_is_cst (rhs))
    {
      isl_pw_aff_free (lhs);
      isl_pw_aff_free (rhs);
      return NULL;
    }

  return isl_pw_aff_mul (lhs, rhs);
}

/* Return an ISL identifier from the name of the ssa_name E.  */

static isl_id *
isl_id_for_ssa_name (scop_p s, tree e)
{
  char name1[10];
  snprintf (name1, sizeof (name1), "P_%d", SSA_NAME_VERSION (e));
  return isl_id_alloc (s->isl_context, name1, e);
}

/* Return an ISL identifier for the data reference DR.  Data references and
   scalar references get the same isl_id.  They need to be comparable and are
   distinguished through the first dimension, which contains the alias set or
   SSA_NAME_VERSION number.  */

static isl_id *
isl_id_for_dr (scop_p s)
{
  return isl_id_alloc (s->isl_context, "", 0);
}

/* Extract an affine expression from the ssa_name E.  */

static isl_pw_aff *
extract_affine_name (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_id *id = isl_id_for_ssa_name (s, e);
  int dimension = isl_space_find_dim_by_id (space, isl_dim_param, id);
  isl_id_free (id);
  isl_set *dom = isl_set_universe (isl_space_copy (space));
  isl_aff *aff = isl_aff_zero_on_domain (isl_local_space_from_space (space));
  aff = isl_aff_add_coefficient_si (aff, isl_dim_param, dimension, 1);
  return isl_pw_aff_alloc (dom, aff);
}

/* Extract an affine expression from the gmp constant G.  */

static isl_pw_aff *
extract_affine_gmp (mpz_t g, __isl_take isl_space *space)
{
  isl_local_space *ls = isl_local_space_from_space (isl_space_copy (space));
  isl_aff *aff = isl_aff_zero_on_domain (ls);
  isl_set *dom = isl_set_universe (space);
  isl_ctx *ct = isl_aff_get_ctx (aff);
  isl_val *v = isl_val_int_from_gmp (ct, g);
  aff = isl_aff_add_constant_val (aff, v);

  return isl_pw_aff_alloc (dom, aff);
}

/* Extract an affine expression from the integer_cst E.  */

static isl_pw_aff *
extract_affine_int (tree e, __isl_take isl_space *space)
{
  mpz_t g;

  mpz_init (g);
  tree_int_to_gmp (e, g);
  isl_pw_aff *res = extract_affine_gmp (g, space);
  mpz_clear (g);

  return res;
}

/* Compute pwaff mod 2^width.  */

static isl_pw_aff *
wrap (isl_pw_aff *pwaff, unsigned width)
{
  isl_val *mod;

  mod = isl_val_int_from_ui (isl_pw_aff_get_ctx (pwaff), width);
  mod = isl_val_2exp (mod);
  pwaff = isl_pw_aff_mod_val (pwaff, mod);

  return pwaff;
}

/* When parameter NAME is in REGION, returns its index in SESE_PARAMS.
   Otherwise returns -1.  */

static inline int
parameter_index_in_region_1 (tree name, sese_info_p region)
{
  int i;
  tree p;

  gcc_assert (TREE_CODE (name) == SSA_NAME);

  FOR_EACH_VEC_ELT (region->params, i, p)
    if (p == name)
      return i;

  return -1;
}

/* Extract an affine expression from the tree E in the scop S.  */

static isl_pw_aff *
extract_affine (scop_p s, tree e, __isl_take isl_space *space)
{
  isl_pw_aff *lhs, *rhs, *res;

  if (e == chrec_dont_know) {
    isl_space_free (space);
    return NULL;
  }

  switch (TREE_CODE (e))
    {
    case POLYNOMIAL_CHREC:
      res = extract_affine_chrec (s, e, space);
      break;

    case MULT_EXPR:
      res = extract_affine_mul (s, e, space);
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, TREE_OPERAND (e, 1), space);
      res = isl_pw_aff_add (lhs, rhs);
      break;

    case MINUS_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, TREE_OPERAND (e, 1), space);
      res = isl_pw_aff_sub (lhs, rhs);
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      lhs = extract_affine (s, TREE_OPERAND (e, 0), isl_space_copy (space));
      rhs = extract_affine (s, integer_minus_one_node, space);
      res = isl_pw_aff_mul (lhs, rhs);
      break;

    case SSA_NAME:
      gcc_assert (-1 != parameter_index_in_region_1 (e, s->scop_info)
		  || !invariant_in_sese_p_rec (e, s->scop_info->region, NULL));
      res = extract_affine_name (s, e, space);
      break;

    case INTEGER_CST:
      res = extract_affine_int (e, space);
      /* No need to wrap a single integer.  */
      return res;

    CASE_CONVERT:
    case NON_LVALUE_EXPR:
      res = extract_affine (s, TREE_OPERAND (e, 0), space);
      break;

    default:
      gcc_unreachable ();
      break;
    }

  tree type = TREE_TYPE (e);
  if (TYPE_UNSIGNED (type))
    res = wrap (res, TYPE_PRECISION (type));

  return res;
}

/* Assign dimension for each parameter in SCOP.  */

static void
set_scop_parameter_dim (scop_p scop)
{
  sese_info_p region = scop->scop_info;
  unsigned nbp = sese_nb_params (region);
  isl_space *space = isl_space_set_alloc (scop->isl_context, nbp, 0);

  unsigned i;
  tree e;
  FOR_EACH_VEC_ELT (region->params, i, e)
    space = isl_space_set_dim_id (space, isl_dim_param, i,
                                  isl_id_for_ssa_name (scop, e));

  scop->param_context = isl_set_universe (space);
}

static inline bool
cleanup_loop_iter_dom (isl_set *inner, isl_set *outer, isl_space *space, mpz_t g)
{
  isl_set_free (inner);
  isl_set_free (outer);
  isl_space_free (space);
  mpz_clear (g);
  return false;
}

/* Builds the constraint polyhedra for LOOP in SCOP.  OUTER_PH gives
   the constraints for the surrounding loops.  */

static bool
build_loop_iteration_domains (scop_p scop, struct loop *loop,
                              int nb,
			      isl_set *outer, isl_set **doms)
{

  tree nb_iters = number_of_latch_executions (loop);
  sese_l region = scop->scop_info->region;
  gcc_assert (loop_in_sese_p (loop, region));

  isl_set *inner = isl_set_copy (outer);
  int pos = isl_set_dim (outer, isl_dim_set);
  isl_val *v;
  mpz_t g;

  mpz_init (g);

  inner = isl_set_add_dims (inner, isl_dim_set, 1);
  isl_space *space = isl_set_get_space (inner);

  /* 0 <= loop_i */
  isl_constraint *c = isl_inequality_alloc
      (isl_local_space_from_space (isl_space_copy (space)));
  c = isl_constraint_set_coefficient_si (c, isl_dim_set, pos, 1);
  inner = isl_set_add_constraint (inner, c);

  /* loop_i <= cst_nb_iters */
  if (TREE_CODE (nb_iters) == INTEGER_CST)
    {
      c = isl_inequality_alloc
	  (isl_local_space_from_space (isl_space_copy (space)));
      c = isl_constraint_set_coefficient_si (c, isl_dim_set, pos, -1);
      tree_int_to_gmp (nb_iters, g);
      v = isl_val_int_from_gmp (scop->isl_context, g);
      c = isl_constraint_set_constant_val (c, v);
      inner = isl_set_add_constraint (inner, c);
    }

  /* loop_i <= expr_nb_iters */
  else if (!chrec_contains_undetermined (nb_iters))
    {
      isl_pw_aff *aff;

      nb_iters = scalar_evolution_in_region (region, loop, nb_iters);

      /* Bail out as we do not know the scev.  */
      if (chrec_contains_undetermined (nb_iters))
	return cleanup_loop_iter_dom (inner, outer, space, g);

      aff = extract_affine (scop, nb_iters, isl_set_get_space (inner));
      isl_set *valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (aff));
      valid = isl_set_project_out (valid, isl_dim_set, 0,
				   isl_set_dim (valid, isl_dim_set));

      if (valid)
	scop->param_context = isl_set_intersect (scop->param_context, valid);

      isl_local_space *ls = isl_local_space_from_space (isl_space_copy (space));
      isl_aff *al = isl_aff_set_coefficient_si (isl_aff_zero_on_domain (ls),
						isl_dim_in, pos, 1);
      isl_set *le = isl_pw_aff_le_set (isl_pw_aff_from_aff (al),
				       isl_pw_aff_copy (aff));
      inner = isl_set_intersect (inner, le);

      widest_int nit;
      if (max_stmt_executions (loop, &nit))
	{
	  /* Insert in the context the constraints from the
	     estimation of the number of iterations NIT and the
	     symbolic number of iterations (involving parameter
	     names) NB_ITERS.  First, build the affine expression
	     "NIT - NB_ITERS" and then say that it is positive,
	     i.e., NIT approximates NB_ITERS: "NIT >= NB_ITERS".  */
	  mpz_t g;
	  mpz_init (g);
	  wi::to_mpz (nit, g, SIGNED);
	  mpz_sub_ui (g, g, 1);

	  isl_pw_aff *approx
	    = extract_affine_gmp (g, isl_set_get_space (inner));
	  isl_set *x = isl_pw_aff_ge_set (approx, aff);
	  x = isl_set_project_out (x, isl_dim_set, 0,
				   isl_set_dim (x, isl_dim_set));
	  scop->param_context = isl_set_intersect (scop->param_context, x);

	  isl_constraint *c = isl_inequality_alloc
	      (isl_local_space_from_space (isl_space_copy (space)));
	  c = isl_constraint_set_coefficient_si (c, isl_dim_set, pos, -1);
	  v = isl_val_int_from_gmp (scop->isl_context, g);
	  mpz_clear (g);
	  c = isl_constraint_set_constant_val (c, v);
	  inner = isl_set_add_constraint (inner, c);
	}
      else
	isl_pw_aff_free (aff);
    }
  else
    gcc_unreachable ();

  if (loop->inner
      && !build_loop_iteration_domains (scop, loop->inner, nb + 1,
					isl_set_copy (inner), doms))
    return cleanup_loop_iter_dom (inner, outer, space, g);

  if (nb != 0
      && loop->next
      && loop_in_sese_p (loop->next, region)
      && !build_loop_iteration_domains (scop, loop->next, nb,
					isl_set_copy (outer), doms))
    return cleanup_loop_iter_dom (inner, outer, space, g);

  doms[loop->num] = inner;

  isl_set_free (outer);
  isl_space_free (space);
  mpz_clear (g);
  return true;
}

/* Returns a linear expression for tree T evaluated in PBB.  */

static isl_pw_aff *
create_pw_aff_from_tree (poly_bb_p pbb, tree t)
{
  scop_p scop = PBB_SCOP (pbb);

  t = scalar_evolution_in_region (scop->scop_info->region, pbb_loop (pbb), t);

  /* Bail out as we do not know the scev.  */
  if (chrec_contains_undetermined (t))
    return NULL;

  gcc_assert (!automatically_generated_chrec_p (t));

  return extract_affine (scop, t, isl_set_get_space (pbb->domain));
}

/* Add conditional statement STMT to pbb.  CODE is used as the comparison
   operator.  This allows us to invert the condition or to handle
   inequalities.  */

static bool
add_condition_to_pbb (poly_bb_p pbb, gcond *stmt, enum tree_code code)
{
  isl_pw_aff *lhs = create_pw_aff_from_tree (pbb, gimple_cond_lhs (stmt));
  if (!lhs)
    return false;

  isl_pw_aff *rhs = create_pw_aff_from_tree (pbb, gimple_cond_rhs (stmt));
  if (!rhs)
    {
      isl_pw_aff_free (lhs);
      return false;
    }

  isl_set *cond;
  switch (code)
    {
      case LT_EXPR:
	cond = isl_pw_aff_lt_set (lhs, rhs);
	break;

      case GT_EXPR:
	cond = isl_pw_aff_gt_set (lhs, rhs);
	break;

      case LE_EXPR:
	cond = isl_pw_aff_le_set (lhs, rhs);
	break;

      case GE_EXPR:
	cond = isl_pw_aff_ge_set (lhs, rhs);
	break;

      case EQ_EXPR:
	cond = isl_pw_aff_eq_set (lhs, rhs);
	break;

      case NE_EXPR:
	cond = isl_pw_aff_ne_set (lhs, rhs);
	break;

      default:
	isl_pw_aff_free (lhs);
	isl_pw_aff_free (rhs);
	return true;
    }

  cond = isl_set_coalesce (cond);
  cond = isl_set_set_tuple_id (cond, isl_set_get_tuple_id (pbb->domain));
  pbb->domain = isl_set_intersect (pbb->domain, cond);
  return true;
}

/* Add conditions to the domain of PBB.  */

static bool
add_conditions_to_domain (poly_bb_p pbb)
{
  unsigned int i;
  gimple *stmt;
  gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);

  if (GBB_CONDITIONS (gbb).is_empty ())
    return true;

  FOR_EACH_VEC_ELT (GBB_CONDITIONS (gbb), i, stmt)
    switch (gimple_code (stmt))
      {
      case GIMPLE_COND:
	  {
            /* Don't constrain on anything else than INTEGER_TYPE.  */
	    if (TREE_CODE (TREE_TYPE (gimple_cond_lhs (stmt))) != INTEGER_TYPE)
              break;

	    gcond *cond_stmt = as_a <gcond *> (stmt);
	    enum tree_code code = gimple_cond_code (cond_stmt);

	    /* The conditions for ELSE-branches are inverted.  */
	    if (!GBB_CONDITION_CASES (gbb)[i])
	      code = invert_tree_comparison (code, false);

	    if (!add_condition_to_pbb (pbb, cond_stmt, code))
	      return false;
	    break;
	  }

      case GIMPLE_SWITCH:
	/* Switch statements are not supported right now - fall through.  */

      default:
	gcc_unreachable ();
	break;
      }

  return true;
}

/* Traverses all the GBBs of the SCOP and add their constraints to the
   iteration domains.  */

static bool
add_conditions_to_constraints (scop_p scop)
{
  int i;
  poly_bb_p pbb;

  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    if (!add_conditions_to_domain (pbb))
      return false;

  return true;
}

/* Add constraints on the possible values of parameter P from the type
   of P.  */

static void
add_param_constraints (scop_p scop, graphite_dim_t p)
{
  tree parameter = scop->scop_info->params[p];
  tree type = TREE_TYPE (parameter);
  tree lb = NULL_TREE;
  tree ub = NULL_TREE;

  if (POINTER_TYPE_P (type) || !TYPE_MIN_VALUE (type))
    lb = lower_bound_in_type (type, type);
  else
    lb = TYPE_MIN_VALUE (type);

  if (POINTER_TYPE_P (type) || !TYPE_MAX_VALUE (type))
    ub = upper_bound_in_type (type, type);
  else
    ub = TYPE_MAX_VALUE (type);

  if (lb)
    {
      isl_space *space = isl_set_get_space (scop->param_context);
      isl_constraint *c;
      mpz_t g;
      isl_val *v;

      c = isl_inequality_alloc (isl_local_space_from_space (space));
      mpz_init (g);
      tree_int_to_gmp (lb, g);
      v = isl_val_int_from_gmp (scop->isl_context, g);
      v = isl_val_neg (v);
      mpz_clear (g);
      c = isl_constraint_set_constant_val (c, v);
      c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, 1);

      scop->param_context = isl_set_add_constraint (scop->param_context, c);
    }

  if (ub)
    {
      isl_space *space = isl_set_get_space (scop->param_context);
      isl_constraint *c;
      mpz_t g;
      isl_val *v;

      c = isl_inequality_alloc (isl_local_space_from_space (space));

      mpz_init (g);
      tree_int_to_gmp (ub, g);
      v = isl_val_int_from_gmp (scop->isl_context, g);
      mpz_clear (g);
      c = isl_constraint_set_constant_val (c, v);
      c = isl_constraint_set_coefficient_si (c, isl_dim_param, p, -1);

      scop->param_context = isl_set_add_constraint (scop->param_context, c);
    }
}

/* Build the context of the SCOP.  The context usually contains extra
   constraints that are added to the iteration domains that constrain
   some parameters.  */

static void
build_scop_context (scop_p scop)
{
  graphite_dim_t p, n = scop_nb_params (scop);

  for (p = 0; p < n; p++)
    add_param_constraints (scop, p);
}

/* Build the iteration domains: the loops belonging to the current
   SCOP, and that vary for the execution of the current basic block.
   Returns false if there is no loop in SCOP.  */

static bool
build_scop_iteration_domain (scop_p scop)
{
  sese_info_p region = scop->scop_info;
  int nb_loops = number_of_loops (cfun);
  isl_set **doms = XCNEWVEC (isl_set *, nb_loops);
  bool res = true;
  int i;
  struct loop *loop;
  FOR_EACH_VEC_ELT (region->loop_nest, i, loop)
    if (!loop_in_sese_p (loop_outer (loop), region->region)
	&& !build_loop_iteration_domains (scop, loop, 0,
					  isl_set_copy (scop->param_context), doms))
      {
	res = false;
	goto cleanup;
      }

  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    {
      loop = pbb_loop (pbb);

      if (doms[loop->num])
	pbb->domain = isl_set_copy (doms[loop->num]);
      else
	pbb->domain = isl_set_copy (scop->param_context);

      pbb->domain = isl_set_set_tuple_id (pbb->domain,
					  isl_id_for_pbb (scop, pbb));
    }

 cleanup:
  for (int i = 0; i < nb_loops; i++)
    if (doms[i])
      isl_set_free (doms[i]);

  free (doms);
  return res;
}

/* Add a constrain to the ACCESSES polyhedron for the alias set of
   data reference DR.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static isl_map *
pdr_add_alias_set (isl_map *acc, dr_info &dri)
{
  isl_constraint *c = isl_equality_alloc
      (isl_local_space_from_space (isl_map_get_space (acc)));
  /* Positive numbers for all alias sets.  */
  c = isl_constraint_set_constant_si (c, -dri.alias_set);
  c = isl_constraint_set_coefficient_si (c, isl_dim_out, 0, 1);

  return isl_map_add_constraint (acc, c);
}

/* Add a constrain to the ACCESSES polyhedron for the alias set of
   data reference DR.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static isl_map *
add_scalar_version_numbers (isl_map *acc, tree var)
{
  isl_constraint *c = isl_equality_alloc
      (isl_local_space_from_space (isl_map_get_space (acc)));
  int max_arrays = PARAM_VALUE (PARAM_GRAPHITE_MAX_ARRAYS_PER_SCOP);
  /* Each scalar variables has a unique alias set number starting from
     max_arrays.  */
  c = isl_constraint_set_constant_si (c, -max_arrays - SSA_NAME_VERSION (var));
  c = isl_constraint_set_coefficient_si (c, isl_dim_out, 0, 1);

  return isl_map_add_constraint (acc, c);
}

/* Assign the affine expression INDEX to the output dimension POS of
   MAP and return the result.  */

static isl_map *
set_index (isl_map *map, int pos, isl_pw_aff *index)
{
  isl_map *index_map;
  int len = isl_map_dim (map, isl_dim_out);
  isl_id *id;

  index_map = isl_map_from_pw_aff (index);
  index_map = isl_map_insert_dims (index_map, isl_dim_out, 0, pos);
  index_map = isl_map_add_dims (index_map, isl_dim_out, len - pos - 1);

  id = isl_map_get_tuple_id (map, isl_dim_out);
  index_map = isl_map_set_tuple_id (index_map, isl_dim_out, id);
  id = isl_map_get_tuple_id (map, isl_dim_in);
  index_map = isl_map_set_tuple_id (index_map, isl_dim_in, id);

  return isl_map_intersect (map, index_map);
}

/* Add to ACCESSES polyhedron equalities defining the access functions
   to the memory.  ACCESSP_NB_DIMS is the dimension of the ACCESSES
   polyhedron, DOM_NB_DIMS is the dimension of the iteration domain.
   PBB is the poly_bb_p that contains the data reference DR.  */

static isl_map *
pdr_add_memory_accesses (isl_map *acc, dr_info &dri)
{
  data_reference_p dr = dri.dr;
  poly_bb_p pbb = dri.pbb;
  int i, nb_subscripts = DR_NUM_DIMENSIONS (dr);
  scop_p scop = PBB_SCOP (pbb);

  for (i = 0; i < nb_subscripts; i++)
    {
      isl_pw_aff *aff;
      tree afn = DR_ACCESS_FN (dr, nb_subscripts - 1 - i);

      aff = extract_affine (scop, afn,
			    isl_space_domain (isl_map_get_space (acc)));
      acc = set_index (acc, i + 1, aff);
    }

  return acc;
}

/* Return true when the LOW and HIGH bounds of an array reference REF are valid
   to extract constraints on accessed elements of the array.  Returning false is
   the conservative answer.  */

static bool
bounds_are_valid (tree ref, tree low, tree high)
{
  if (!high)
    return false;

  if (!tree_fits_shwi_p (low)
      || !tree_fits_shwi_p (high))
    return false;

  /* 1-element arrays at end of structures may extend over
     their declared size.  */
  if (array_at_struct_end_p (ref)
      && operand_equal_p (low, high, 0))
    return false;

  /* Fortran has some arrays where high bound is -1 and low is 0.  */
  if (integer_onep (fold_build2 (LT_EXPR, boolean_type_node, high, low)))
    return false;

  return true;
}

/* Add constrains representing the size of the accessed data to the
   ACCESSES polyhedron.  ACCESSP_NB_DIMS is the dimension of the
   ACCESSES polyhedron, DOM_NB_DIMS is the dimension of the iteration
   domain.  */

static isl_set *
pdr_add_data_dimensions (isl_set *subscript_sizes, scop_p scop,
			 data_reference_p dr)
{
  tree ref = DR_REF (dr);

  int nb_subscripts = DR_NUM_DIMENSIONS (dr);
  for (int i = nb_subscripts - 1; i >= 0; i--, ref = TREE_OPERAND (ref, 0))
    {
      if (TREE_CODE (ref) != ARRAY_REF)
	return subscript_sizes;

      tree low = array_ref_low_bound (ref);
      tree high = array_ref_up_bound (ref);

      if (!bounds_are_valid (ref, low, high))
	continue;

      isl_space *space = isl_set_get_space (subscript_sizes);
      isl_pw_aff *lb = extract_affine_int (low, isl_space_copy (space));
      isl_pw_aff *ub = extract_affine_int (high, isl_space_copy (space));

      /* high >= 0 */
      isl_set *valid = isl_pw_aff_nonneg_set (isl_pw_aff_copy (ub));
      valid = isl_set_project_out (valid, isl_dim_set, 0,
				   isl_set_dim (valid, isl_dim_set));
      scop->param_context = isl_set_intersect (scop->param_context, valid);

      isl_aff *aff
	= isl_aff_zero_on_domain (isl_local_space_from_space (space));
      aff = isl_aff_add_coefficient_si (aff, isl_dim_in, i + 1, 1);
      isl_set *univ
	= isl_set_universe (isl_space_domain (isl_aff_get_space (aff)));
      isl_pw_aff *index = isl_pw_aff_alloc (univ, aff);

      isl_id *id = isl_set_get_tuple_id (subscript_sizes);
      lb = isl_pw_aff_set_tuple_id (lb, isl_dim_in, isl_id_copy (id));
      ub = isl_pw_aff_set_tuple_id (ub, isl_dim_in, id);

      /* low <= sub_i <= high */
      isl_set *lbs = isl_pw_aff_ge_set (isl_pw_aff_copy (index), lb);
      isl_set *ubs = isl_pw_aff_le_set (index, ub);
      subscript_sizes = isl_set_intersect (subscript_sizes, lbs);
      subscript_sizes = isl_set_intersect (subscript_sizes, ubs);
    }

  return subscript_sizes;
}

/* Build data accesses for DRI.  */

static void
build_poly_dr (dr_info &dri)
{
  isl_map *acc;
  isl_set *subscript_sizes;
  poly_bb_p pbb = dri.pbb;
  data_reference_p dr = dri.dr;
  scop_p scop = PBB_SCOP (pbb);
  isl_id *id = isl_id_for_dr (scop);

  {
    isl_space *dc = isl_set_get_space (pbb->domain);
    int nb_out = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_add_dims (isl_space_from_domain (dc),
					   isl_dim_out, nb_out);

    acc = isl_map_universe (space);
    acc = isl_map_set_tuple_id (acc, isl_dim_out, isl_id_copy (id));
  }

  acc = pdr_add_alias_set (acc, dri);
  acc = pdr_add_memory_accesses (acc, dri);

  {
    int nb = 1 + DR_NUM_DIMENSIONS (dr);
    isl_space *space = isl_space_set_alloc (scop->isl_context, 0, nb);

    space = isl_space_set_tuple_id (space, isl_dim_set, id);
    subscript_sizes = isl_set_nat_universe (space);
    subscript_sizes = isl_set_fix_si (subscript_sizes, isl_dim_set, 0,
				      dri.alias_set);
    subscript_sizes = pdr_add_data_dimensions (subscript_sizes, scop, dr);
  }

  new_poly_dr (pbb, DR_STMT (dr), DR_IS_READ (dr) ? PDR_READ : PDR_WRITE,
	       acc, subscript_sizes);
}

static void
build_poly_sr_1 (poly_bb_p pbb, gimple *stmt, tree var, enum poly_dr_type kind,
		 isl_map *acc, isl_set *subscript_sizes)
{
  int max_arrays = PARAM_VALUE (PARAM_GRAPHITE_MAX_ARRAYS_PER_SCOP);
  /* Each scalar variables has a unique alias set number starting from
     max_arrays.  */
  subscript_sizes = isl_set_fix_si (subscript_sizes, isl_dim_set, 0,
				    max_arrays + SSA_NAME_VERSION (var));

  new_poly_dr (pbb, stmt, kind, add_scalar_version_numbers (acc, var),
	       subscript_sizes);
}

/* Record all cross basic block scalar variables in PBB.  */

static void
build_poly_sr (poly_bb_p pbb)
{
  scop_p scop = PBB_SCOP (pbb);
  gimple_poly_bb_p gbb = PBB_BLACK_BOX (pbb);
  vec<scalar_use> reads = gbb->read_scalar_refs;
  vec<tree> writes = gbb->write_scalar_refs;

  isl_space *dc = isl_set_get_space (pbb->domain);
  int nb_out = 1;
  isl_space *space = isl_space_add_dims (isl_space_from_domain (dc),
					 isl_dim_out, nb_out);
  isl_id *id = isl_id_for_dr (scop);
  space = isl_space_set_tuple_id (space, isl_dim_set, isl_id_copy (id));
  isl_map *acc = isl_map_universe (isl_space_copy (space));
  acc = isl_map_set_tuple_id (acc, isl_dim_out, id);
  isl_set *subscript_sizes = isl_set_nat_universe (space);

  int i;
  tree var;
  FOR_EACH_VEC_ELT (writes, i, var)
    build_poly_sr_1 (pbb, SSA_NAME_DEF_STMT (var), var, PDR_WRITE,
		     isl_map_copy (acc), isl_set_copy (subscript_sizes));

  scalar_use *use;
  FOR_EACH_VEC_ELT (reads, i, use)
    build_poly_sr_1 (pbb, use->first, use->second, PDR_READ, isl_map_copy (acc),
		     isl_set_copy (subscript_sizes));

  isl_map_free (acc);
  isl_set_free (subscript_sizes);
}

/* Build data references in SCOP.  */

static void
build_scop_drs (scop_p scop)
{
  int i;
  dr_info *dri;
  FOR_EACH_VEC_ELT (scop->drs, i, dri)
    build_poly_dr (*dri);

  poly_bb_p pbb;
  FOR_EACH_VEC_ELT (scop->pbbs, i, pbb)
    build_poly_sr (pbb);
}

/* Builds the polyhedral representation for a SESE region.  */

bool
build_poly_scop (scop_p scop)
{
  set_scop_parameter_dim (scop);
  if (!build_scop_iteration_domain (scop))
    return false;

  build_scop_context (scop);

  if (!add_conditions_to_constraints (scop))
    return false;

  build_scop_drs (scop);
  build_scop_scattering (scop);
  return true;
}
#endif  /* HAVE_isl */
