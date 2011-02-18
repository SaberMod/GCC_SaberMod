/* Language-dependent hooks for Objective-C.
   Copyright 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Ziemowit Laski  <zlaski@apple.com>

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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "c-tree.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "ggc.h"
#include "objc-act.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "c-objc-common.h"
#include "c-lang.h"

enum c_language_kind c_language = clk_objc;
static void objc_init_ts (void);

/* Lang hooks common to C and ObjC are declared in c-objc-common.h;
   consequently, there should be very few hooks below.  */

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU Objective-C"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT objc_init
#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME objc_printable_name
#undef LANG_HOOKS_GIMPLIFY_EXPR 
#define LANG_HOOKS_GIMPLIFY_EXPR objc_gimplify_expr
#undef LANG_HOOKS_INIT_TS
#define LANG_HOOKS_INIT_TS objc_init_ts

/* Each front end provides its own lang hook initializer.  */
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Lang hook routines common to C and ObjC appear in c-objc-common.c;
   there should be very few (if any) routines below.  */

static void
objc_init_ts (void)
{
  tree_contains_struct[CLASS_METHOD_DECL][TS_DECL_NON_COMMON] = 1;
  tree_contains_struct[INSTANCE_METHOD_DECL][TS_DECL_NON_COMMON] = 1;
  tree_contains_struct[KEYWORD_DECL][TS_DECL_NON_COMMON] = 1;
  tree_contains_struct[PROPERTY_DECL][TS_DECL_NON_COMMON] = 1;
  
  tree_contains_struct[CLASS_METHOD_DECL][TS_DECL_WITH_VIS] = 1;
  tree_contains_struct[INSTANCE_METHOD_DECL][TS_DECL_WITH_VIS] = 1;
  tree_contains_struct[KEYWORD_DECL][TS_DECL_WITH_VIS] = 1;
  tree_contains_struct[PROPERTY_DECL][TS_DECL_WITH_VIS] = 1;

  tree_contains_struct[CLASS_METHOD_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[INSTANCE_METHOD_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[KEYWORD_DECL][TS_DECL_WRTL] = 1;
  tree_contains_struct[PROPERTY_DECL][TS_DECL_WRTL] = 1;
  
  tree_contains_struct[CLASS_METHOD_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[INSTANCE_METHOD_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[KEYWORD_DECL][TS_DECL_MINIMAL] = 1;
  tree_contains_struct[PROPERTY_DECL][TS_DECL_MINIMAL] = 1;
  
  tree_contains_struct[CLASS_METHOD_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[INSTANCE_METHOD_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[KEYWORD_DECL][TS_DECL_COMMON] = 1;
  tree_contains_struct[PROPERTY_DECL][TS_DECL_COMMON] = 1;
}

#include "gtype-objc.h"
