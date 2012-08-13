// Copyright (C) 2012
// Free Software Foundation
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#ifndef _VTV_UTILS_H
#define _VTV_UTILS_H 1

#include <stdlib.h>

/* TODO: we need to clean up this. Maybe use __glibcxx_assert */

/* TODO: Define what is the expected behavior of assert and error */
/* Handling of runtime error */
#define VTV_error abort

#define VTV_ASSERT(EXPR) ((void)(!(EXPR) ? VTV_error() : (void) 0))
#define VTV_DEBUG_ASSERT(EXPR) VTV_ASSERT(EXPR)
//#define VTV_DEBUG_ASSERT(EXPR) (void)0

/* Name of the section where we put general VTV variables for protection */
#define VTV_PROTECTED_VARS_SECTION ".data.rel.ro.vtable_vars"
#define VTV_PROTECTED_VAR __attribute__ ((section (VTV_PROTECTED_VARS_SECTION)))

#endif /* VTV_UTILS_H */
