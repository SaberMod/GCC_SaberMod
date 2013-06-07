/* Copyright (C) 2012
   Free Software Foundation

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */


/* This file contains all the definitions that go into the libvtv_init
   library, which is part of the vtable verification feature.  This
   library should contain exactly two functions (__vtv_unprotect and
   __vtv_protect) and one global variable definition
   (__vtv_defined_in_vtv_init_lib).  Any program that was compiled
   with the option "-fvtable-verify=std" MUST also be linked with
   libvtv_init, because the two functions defined here are used by the
   vtable verification code.  The reason they are in a separate
   library rather than in libstdc++ with all the rest of the vtable
   verification runtime code is as follows.  Each .o file that was
   compiled with vtable verification will contain calls into the
   runtime (made from constructor initialization functions) to build
   the data structures needed for verification.  At all times except
   when they are being constructed, these data structures need to be
   in protected memory, so that attackers cannot corrupt them.
   __vtv_unprotect sets the memory containing these data structures to
   be writable, for updates.  __vtv_protect makes the memory read-only,
   for all other times.  This memory protection and unprotection is
   done via calls to mprotect, which are costly.  So instead of
   calling __vtv_unprotect and __vtv_protect once per object file we
   want to call them once per executable.  Therefore instead of
   putting calls to them directly into each object file, we put the
   calls to them only in __vtv_register_pair, in the libstdc++ library.
   We give __vtv_unprotect an initialization priority to make it run
   before all of our data structure construction functions, and we
   give __vtv_protect an initialization priority to make it run after
   all of our data structure constructiion functions.  We put them
   into a separate library and link that library with the
   "--whole-archive" linker option, to make sure that both functions get
   linked in (since the actual calls to them are in the libstdc++
   runtime).  We can't put them into libstdc++ because linking
   libstdc++ with "--whole-archive" is probably not a good idea.

   The __vtv_defined_in_vtv_lib variable is referenced, but not
   defined, in the constructor initialization functions where we have
   the calls to build our data structures.  The purpose of this
   variable is to cause a linker error to occur if the programmer
   compiled with -fvtable-verify=std and did not link with the vtv_int
   library (better a link-time error than a run-time error).  */


/* Needs to build with C++ because the definition of
   __vtv_change_permission is in C++.  */
#ifndef __cplusplus
#error "This file must be compiled with a C++ compiler"
#endif

#include "../../include/vtv-change-permission.h"
#include "vtv_rts.h"

/* Define this dummy symbol to detect at link time the cases where
   user is compiling with -fvtable-verify=std and not linking with the
   vtv_init library. Note that the visibility needs to be hidden. Each
   object module is supposed to link with the vtv_init library and we
   don't want this definition to come from a different library */
unsigned int
__vtv_defined_in_vtv_init_lib __attribute__ ((visibility ("hidden"))) = 0;

void __vtv_unprotect (void) __attribute__ ((constructor(98)));
void __vtv_protect (void) __attribute__ ((constructor(100)));

void
__vtv_unprotect (void)
{
  __vtv_change_permission (__VLTP_READ_WRITE);
}

void
__vtv_protect (void)
{
  __vtv_change_permission (__VLTP_READ_ONLY);
}


#ifdef VTV_STATIC_VERIFY

#include "vtv_rts_core.h"

extern s2s * __vtv_symbol_unification_map;

#include "vtv_rts_core.cc"

#endif
