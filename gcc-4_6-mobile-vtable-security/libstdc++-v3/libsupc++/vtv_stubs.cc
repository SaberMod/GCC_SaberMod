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

void
__VLTRegisterPairDebug (void ** data_pointer __attribute__((__unused__)),
                        void * test_value __attribute__((__unused__)),
                        int size_hint __attribute__((__unused__)),
                        char * base_ptr_var_name __attribute__((__unused__)),
                        int len1 __attribute__((__unused__)),
                        char * vtable_name __attribute__((__unused__)),
                        int len2 __attribute__((__unused__)))
{
}

void *
__VLTVerifyVtablePointerDebug (void ** data_pointer __attribute__((__unused__)),
                               void * test_value,
                               char * base_vtbl_var_name __attribute__((__unused__)),
                               int len1 __attribute__((__unused__)),
                               char * vtable_name __attribute__((__unused__)),
                               int len2 __attribute__((__unused__)))

{
  return test_value;
}

#else

void
__VLTRegisterPair (void ** data_pointer __attribute__((__unused__)),
                   void * test_value __attribute__((__unused__)),
                   int size_hint __attribute__((__unused__)))

{
}

void *
__VLTVerifyVtablePointer (void ** data_pointer __attribute__((__unused__)),
                          void * test_value)
{
  return test_value;
}

#endif
