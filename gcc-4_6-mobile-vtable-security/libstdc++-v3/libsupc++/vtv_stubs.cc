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

void *
__VLTVerifyVtablePointerDebug (void **data_pointer, void *test_value,
                               char *base_vtbl_var_name, int len1, char *vtable_name,
                               int len2)
{
  return test_value;
}

void *
__VLTVerifyVtablePointer (void **data_pointer, void *test_value)
{
  return test_value;
}

void *
__VLTRegisterPair (void **data_pointer, void *test_value, int size_hint,
                   char *base_ptr_var_name, int len1, char *vtable_name,
                   int len2)
{
  return test_value;
}

void
__VLTChangePermission (int perm)
{
}
