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

#include "../../include/vtv-change-permission.h"
#include "vtv_rts.h"

/* The is part of the vtable verification runtime library.  For more
   information about this feature, see the comments in vtv_rts.cc.  */

/* The functions in this file are used to create the libvtv_stubs
   library, as part of the vtable verification feature.  When building
   a binary without vtable verification, and linking it with a
   (possibly pre-built third-party) library that was built with
   verification, it is possible that vtable verification will fail due
   to incomplete data (rather than due to corrupt vtable pointers).  In
   this case we need to give programmers a way of turning off the
   vtable verification in their libraries.  They can do so by linking
   with the libvtv_stubs library, which (as you can see) will replace
   the real verification functions with a set of functions that do
   nothing (so no more verification failures/aborts).  */

extern "C"
void
__VLTChangePermission (int perm __attribute__((__unused__)))
{
}


#ifdef VTV_DEBUG

void
__VLTRegisterSetDebug (void ** __attribute__((__unused__)), 
                       const void * __attribute__((__unused__)),
                       std::size_t __attribute__((__unused__)),
                       std::size_t __attribute__((__unused__)),
                       void ** __attribute__((__unused__)))
{
}

void
__VLTRegisterPairDebug (void ** __attribute__((__unused__)),
                        const  void * __attribute__((__unused__)),
                        std::size_t __attribute__((__unused__)),
                        const  void * __attribute__((__unused__)),
                        const  char * __attribute__((__unused__)),
                        const  char * __attribute__((__unused__)))
{
}

const void *
__VLTVerifyVtablePointerDebug
                     (void **set_handle_ptr  __attribute__((__unused__)),
                      const void *vtable_ptr,
                      const char *set_symbol_name __attribute__((__unused__)),
                      const char *vtable_name __attribute__((__unused__)))

{
  return vtable_ptr;
}

#else

void
__VLTRegisterSet (void ** __attribute__((__unused__)),
                  const void * __attribute__((__unused__)),
                  std::size_t __attribute__((__unused__)),
                  std::size_t __attribute__((__unused__)),
                  void ** __attribute__((__unused__)))
{
}

void
__VLTRegisterPair (void ** __attribute__((__unused__)),
                   const  void * __attribute__((__unused__)),
                   std::size_t __attribute__((__unused__)),
                   const  void * __attribute__((__unused__)))
{
}

const void *
__VLTVerifyVtablePointer (void **set_handle_ptr __attribute__((__unused__)),
                          const void *vtable_ptr)
{
  return vtable_ptr;
}

#endif 
