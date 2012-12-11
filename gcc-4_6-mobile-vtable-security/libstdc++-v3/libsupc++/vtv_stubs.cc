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

#include "vtv_rts.h"

void
__VLTChangePermission (int perm __attribute__((__unused__)))
{
}


#ifdef VTV_DEBUG

void __VLTInitSetSymbolDebug(void ** set_handle_ptr __attribute__((__unused__)),
                             const void * set_symbol_key
                                 __attribute__((__unused__)),
                             std::size_t size_hint __attribute__((__unused__)))
{
}

void
__VLTRegisterPairDebug (void ** set_handle_ptr __attribute__((__unused__)),
                        const void * vtable_ptr __attribute__((__unused__)),
                        const char * set_symbol_name
                            __attribute__((__unused__)),
                        const char * vtable_name __attribute__((__unused__)))
{
}

const void *
__VLTVerifyVtablePointerDebug (void ** set_handle_ptr
                                   __attribute__((__unused__)),
                               const void * vtable_ptr,
                               const char * set_symbol_name
                                   __attribute__((__unused__)),
                               const char * vtable_name
                                   __attribute__((__unused__)))
{
  return vtable_ptr;
}

#else

void __VLTInitSetSymbol(void ** set_handle_ptr __attribute__((__unused__)),
                        const void * set_symbol_key __attribute__((__unused__)),
                        std::size_t size_hint __attribute__((__unused__)))
{
}

void
__VLTRegisterPair (void ** set_handle_ptr __attribute__((__unused__)),
                   const void * vtable_ptr __attribute__((__unused__)))
{
}

const void *
__VLTVerifyVtablePointer (void ** set_handle_ptr __attribute__((__unused__)),
                          const void * vtable_ptr)
{
  return vtable_ptr;
}

#endif
