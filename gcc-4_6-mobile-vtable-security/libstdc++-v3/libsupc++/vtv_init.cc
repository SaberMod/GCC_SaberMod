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


/* Needs to build with C++ because the definition of
 * __VLTChangePermission is in C++ */
#ifndef __cplusplus
#error "This file must be compiled with a C++ compiler"
#endif

#include "vtv_rts.h"

/* Set the following macro to 1 to get internal debugging messages */
#define VTV_DEBUG 0

#if (VTV_DEBUG == 1)
#include <stdio.h>
#endif

/* Define this dummy symbol to detect at link time the cases where
   user is compiling with -fvtable-verify=std and not linking with the
   vtv_init library. Note that the visibility needs to be hidden. Each
   object module is supposed to link with the vtv_init library and we
   don't want this definition to come from a different library */
unsigned int
__vtv_defined_in_vtv_init_lib __attribute__ ((visibility ("hidden"))) = 0;

void __VLTunprotect() __attribute__((constructor(98)));
void __VLTunprotect()
{
#if (VTV_DEBUG == 1)
  fprintf(stderr, "in __VLTunprotect\n");
#endif

  __VLTChangePermission(__VLTP_READ_WRITE);
}

void __VLTprotect() __attribute__((constructor(100)));
void __VLTprotect()
{
#if (VTV_DEBUG == 1)
  fprintf(stderr, "in __VLTprotect\n");
#endif

  __VLTChangePermission(__VLTP_READ_ONLY);
}
