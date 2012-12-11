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
  __VLTChangePermission(__VLTP_READ_WRITE);
}

void __VLTprotect() __attribute__((constructor(100)));
void __VLTprotect()
{
  __VLTChangePermission(__VLTP_READ_ONLY);
}

/* This VTV_STATIC_VERIFY macro is experimental for now. If we are
   going to use it we need to put the code below in a common
   place. Right now it is a copy of the code in vtv_rts.cc */
#ifdef VTV_STATIC_VERIFY

#ifdef VTV_DEBUG

const void *
__VLTVerifyVtablePointerDebug (void ** set_handle_ptr,
                               const void * vtable_ptr,
                               const char * set_symbol_name,
                               const char * vtable_name)
{
#ifndef VTV_EMPTY_VERIFY
  VTV_DEBUG_ASSERT(set_handle_ptr != NULL && *set_handle_ptr != NULL);
  int_vptr vtbl_ptr = (int_vptr) vtable_ptr;

  vtv_set_handle * handle_ptr;
  if (!is_set_handle_handle(*set_handle_ptr))
    handle_ptr = (vtv_set_handle *)set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle(*set_handle_ptr);

  if (vtv_sets::contains(vtbl_ptr, handle_ptr))
    {
      if (debug_verify_vtable)
        {
          if (verify_vtable_log_fd == -1)
            vtv_open_log("vtv_verify_vtable.log");
          vtv_add_to_log(verify_vtable_log_fd,
                         "Verified %s %s value = %p\n",
                         set_symbol_name, vtable_name, vtable_ptr);
        }
    }
  else
    {
      snprintf(debug_log_message, sizeof(debug_log_message),
               "Looking for %s in %s\n", vtable_name, set_symbol_name);
      __vtv_verify_fail_debug (set_handle_ptr, vtable_ptr, debug_log_message);

      /* Normally __vtv_verify_fail will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replace __vtv_verify_fail with
         some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
    }
#endif

  return vtable_ptr;
}

#else

const void *
__VLTVerifyVtablePointer (void ** set_handle_ptr, const void * vtable_ptr)
{
#ifndef VTV_EMPTY_VERIFY
  int_vptr vtbl_ptr = (int_vptr) vtable_ptr;

  vtv_set_handle * handle_ptr;
  if (!is_set_handle_handle(*set_handle_ptr))
    handle_ptr = (vtv_set_handle *)set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle(*set_handle_ptr);

  if (!vtv_sets::contains(vtbl_ptr, handle_ptr))
    {
      __vtv_verify_fail ((void **)handle_ptr, vtable_ptr);
      /* Normally __vtv_verify_fail will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replaced __vtv_verify_fail with
         some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
    }
#endif

  return vtable_ptr;
}

#endif // VTV_DEBUG

#endif // VTV_STATIC_VERIFY
