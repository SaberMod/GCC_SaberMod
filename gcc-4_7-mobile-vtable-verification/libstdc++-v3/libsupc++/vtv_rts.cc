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

/* This file is part of the vtable security feature implementation.
   The vtable security feature is designed to detect when a virtual
   call is about to be made through an invalid vtable pointer
   (possibly due to data corruption or malicious attacks). The
   compiler finds every virtual call, and inserts a verification call
   before the virtual call.  The verification call takes the actual
   vtable pointer value in the object through which the virtual call
   is being made, and compares the vtable pointer against a set of all
   valid vtable pointers that the object could contain (this set is
   based on the declared type of the object).  If the pointer is in
   the valid set, execution is allowed to continue; otherwise the
   program is halted.

  There are several pieces needed in order to make this work: 1. For
  every virtual class in the program (i.e. a class that contains
  virtual methods), we need to build the set of all possible valid
  vtables that an object of that class could point to.  This includes
  vtables for any class(es) that inherit from the class under
  consideration.  2. For every such data set we build up, we need a
  way to find and reference the data set.  This is complicated by the
  fact that the real vtable addresses are not known until runtime,
  when the program is loaded into memory, but we need to reference the
  sets at compile time when we are inserting verification calls into
  the program.  3.  We need to find every virtual call in the program,
  and insert the verification call (with the appropriate arguments)
  before the virtual call.  4. We need some runtime library pieces:
  the code to build up the data sets at runtime; the code to actually
  perform the verification using the data sets; and some code to set
  protections on the data sets, so they themselves do not become
  hacker targets.

  To find and reference the set of valid vtable pointers for any given
  virtual class, we create a special global varible for each virtual
  class.  We refer to this as the "vtable map variable" for that
  class.  The vtable map variable has the type "void *", and is
  initialized by the compiler to NULL.  At runtime when the set of
  valid vtable pointers for a virtual class, e.g. class Foo, is built,
  the vtable map variable for class Foo is made to point to the set.
  During compile time, when the compiler is inserting verification
  calls into the program, it passes the vtable map variable for the
  appropriate class to the verification call, so that at runtime the
  verification call can find the appropriate data set.

  The actual set of valid vtable pointers for a virtual class,
  e.g. class Foo, cannot be built until runtime, when the vtables get
  loaded into memory and their addresses are known.  But the knowledge
  about which vtables belong in which class' hierarchy is only known
  at compile time.  Therefore at compile time we collect class
  hierarchy and vtable information about every virtual class, and we
  generate calls to build up the data sets at runtime.  To build the
  data sets, we call one of the functions we add to the runtime
  library, __VLTRegisterPair.  __VLTRegisterPair takes two arguments,
  a vtable map variable and the address of a vtable.  If the vtable
  map variable is currently NULL, it creates a new data set (hash
  table), makes the vtable map variable point to the new data set, and
  inserts the vtable address into the data set.  If the vtable map
  variable is not NULL, it just inserts the vtable address into the
  data set.  In order to make sure that our data sets are built before
  any verification calls happen, we create a special constructor
  initialization function for each compilation unit, give it a very
  high initialization priority, and insert all of our calls to
  __VLTRegisterPair into our special constructor initialization
  function.  */

/* This file contains the main externally visible runtime library
   functions for vtable verification: __VLTChangePermission,
   __VLTRegisterPair, and __VLTVerifyVtablePointer.  It also contains
   debug versions __VLTRegisterPairDebug and
   __VLTVerifyVtablePointerDebug, which have extra parameters in order
   to make it easier to debug verification failures.

   This file also contains the failure functions that get called when
   a vtable pointer is not found in the data set.  Two particularly
   important functions are __vtv_verify_fail and __vtv_really_fail.
   They are both externally visible.  __vtv_verify_fail is defined in
   such a way that it can be replaced by a programmer, if desired.  It
   is the function that __VLTVerifyVtablePointer calls if it can't
   find the pointer in the data set.  Allowing the programmer to
   overwrite this function means that he/she can do some alternate
   verification, including NOT failing in certain specific cases, if
   desired.  This may be the case if the programmer has to deal wtih
   unverified third party software, for example.  __vtv_really_fail is
   available for the programmer to call from his version of
   __vtv_verify_fail, if he decides the failure is real.

   The final piece of functionality implemented in this file is symbol
   resolution for multiple instances of the same vtable map variable.
   If the same virtual class is used in two different compilation
   units, then each compilation unit will create a vtable map variable
   for the class.  We need all instances of the same vtable map
   variable to point to the same (single) set of valid vtable
   pointters for the class, so we wrote our own hashtable-based symbol
   resolution for vtable map variables (with a tiny optimization in
   the case where there is only one instance of the variable).

   There are two other important pieces to the runtime for vtable
   verification besides the main pieces that go into libstdc++.so: two
   special tiny shared libraries, libvtv_init.so and libvtv_stubs.so.
   libvtv_init.so is built from vtv_init.cc.  It is designed to hel[p
   minimize the calls made to mprotect (see the comments in
   vtv_init.cc for more details).  Anything compiled with
   "-fvtable-verify=std" must be linked with libvtv_init.so (the gcc
   driver has been modified to do this).  vtv_stubs.so is built from
   vtv_stubs.cc.  It replaces the main runtime functions
   (__VLTChangePermissino, __VLTRegisterPair and
   __VLTVerifyVtablePoitner) with stub functions that do nothing.  If
   a programmer has a library that was built with verification, but
   wishes to not have verification turned on, the programmer can link
   in the vtv_stubs.so library.  */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <link.h>
#include <fcntl.h>
#include <limits.h>

/* For gthreads suppport */
#include <bits/c++config.h>
#include <ext/concurrence.h>

#include "vtv_utils.h"
#include "vtv_malloc.h"
#include "vtv_set.h"
#include "vtv_map.h"
#include "vtv_rts.h"
#include "vtv_fail.h"
#include "vtv_rts_core.h"

#include "../../include/vtv-change-permission.h"

#define WHITELIST_SIZE 2

int whitelist_fail_count VTV_PROTECTED_HIDDEN_VAR = 0;
int whitelist_phdr_callback_count VTV_PROTECTED_HIDDEN_VAR = 0;

char whitelist_entries [WHITELIST_SIZE][80] VTV_PROTECTED_HIDDEN_VAR =
                                                 { "libnetflixplugin2.so",
                                                   "libpepflashplayer.so" };

struct whitelist_data_struct
{
  ElfW (Addr) low_addr;
  ElfW (Addr) high_addr;
};

// TODO: Should this be a HIDDEN variable
whitelist_data_struct whitelist_data[WHITELIST_SIZE]
  VTV_PROTECTED_GLOBAL_VAR = {};

#ifdef __GTHREAD_MUTEX_INIT
__gthread_mutex_t update_whitelist_lock VTV_PROTECTED_HIDDEN_VAR
                                                         = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t update_whitelist_lock VTV_PROTECTED_HIDDEN_VAR;
#endif

#ifndef __GTHREAD_MUTEX_INIT
static void
initialize_whitelist_mutexes ()
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&update_whitelist_lock);
}
#endif

static int
dl_iterate_phdr_whitelist_callback (struct dl_phdr_info *info,
                                    size_t unused __attribute__((__unused__)),
                                    void *data __attribute__((__unused__)))
{
  if (strlen (info->dlpi_name) == 0)
    return 0;

  for (int i = 0; i < WHITELIST_SIZE; ++i)
    {
      if (whitelist_data[i].low_addr != (ElfW (Addr)) 0x0)
        continue;

      if (strstr (info->dlpi_name, whitelist_entries[i]) != NULL)
        for (int j = 0; j < info->dlpi_phnum; ++j)
          if (info->dlpi_phdr[j].p_type == PT_GNU_RELRO)
            {
              whitelist_data[i].low_addr = info->dlpi_addr
                                               + info->dlpi_phdr[j].p_vaddr;
              whitelist_data[i].high_addr = info->dlpi_addr
                                               + info->dlpi_phdr[j].p_vaddr
                                               + info->dlpi_phdr[j].p_memsz;
              break;
            }
    }

  return 0;
}

extern "C" {

  /* __fortify_fail is a function in glibc that calls __libc_message,
     causing it to print out a program termination error message
     (including the name of the binary being terminated), a stack
     trace where the error occurred, and a memory map dump.  Ideally
     we would have called __libc_message directly, but that function
     does not appear to be accessible to functions outside glibc,
     whereas __fortify_fail is.  We call __fortify_fail from
     __vtv_really_fail.  We looked at calling __libc_fatal, which is
     externally accessible, but it does not do the back trace and
     memory dump.  */

  extern void __fortify_fail (const char *) __attribute__((noreturn));

} /* extern "C" */


/* This function calls __fortify_fail with a FAILURE_MSG and then
   calls abort.  */

void
__vtv_really_fail (const char *failure_msg)
{
  __fortify_fail (failure_msg);

  /* We should never get this far; __fortify_fail calls __libc_message
     which prints out a back trace and a memory dump and then is
     supposed to call abort, but let's play it safe anyway and call abort
     ourselves.  */
  abort ();
}

/* This function takes an error MSG, a vtable map variable
   (DATA_SET_PTR) and a vtable pointer (VTBL_PTR).  It is called when
   an attempt to verify VTBL_PTR with the set pointed to by
   DATA_SET_PTR failed.  It outputs a failure message with the
   addresses involved, and calls __vtv_really_fail.  */

static void
vtv_fail (const char *msg, void **data_set_ptr, const void *vtbl_ptr)
{
  char buffer[128];
  int buf_len;
  const char *format_str =
                 "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";

  snprintf (buffer, sizeof (buffer), format_str, vtbl_ptr,
            is_set_handle_handle(*data_set_ptr) ?
              ptr_from_set_handle_handle (*data_set_ptr) :
              *data_set_ptr);
  buf_len = strlen (buffer);
  /*  Send this to to stderr.  */
  write (2, buffer, buf_len);

#ifndef VTV_NO_ABORT
    __vtv_really_fail (msg);
#endif
}


/* Send information about what we were trying to do when verification
   failed to the error log, then call vtv_fail.  This function can be
   overwritten/replaced by the user, to implement a secondary
   verification function instead.  DATA_SET_PTR is the vtable map
   variable used for the failed verification, and VTBL_PTR is the
   vtable pointer that was not found in the set.  */

void
__vtv_verify_fail (void **data_set_ptr, const void *vtbl_ptr)
{
  char log_msg[256];
  bool need_to_update_whitelist = false;

#ifndef __GTHREAD_MUTEX_INIT
  static __gthread_once_t mutex_once VTV_PROTECTED_VAR = __GTHREAD_ONCE_INIT;

  __gthread_once (&mutex_once, initialize_whitelist_mutexes);
#endif

  for (int i = 0; i < WHITELIST_SIZE; ++i)
    if (whitelist_data[i].low_addr == (ElfW (Addr)) 0x0
        || whitelist_data[i].high_addr == (ElfW (Addr)) 0x0)
      need_to_update_whitelist = true;

  if (need_to_update_whitelist)
    {
      char * low_address;
      size_t list_size;

      __gthread_mutex_lock (&update_whitelist_lock);

      low_address = (char *)&whitelist_data;
      list_size = WHITELIST_SIZE * sizeof (struct whitelist_data_struct);

      low_address = (char *)((unsigned long)low_address & ~(VTV_PAGE_SIZE - 1));
      size_t protect_size = (char *)&whitelist_data - low_address + list_size;
      if (mprotect ((void *) low_address, protect_size,
                    PROT_READ | PROT_WRITE) == -1)
        VTV_error ();

      dl_iterate_phdr (dl_iterate_phdr_whitelist_callback, (void *) NULL);
      whitelist_phdr_callback_count++;

      if (mprotect ((void *) low_address, protect_size, PROT_READ) == -1)
        VTV_error ();

      __gthread_mutex_unlock (&update_whitelist_lock);
    }

  for (int i = 0; i < WHITELIST_SIZE; ++i)
    if (whitelist_data[i].low_addr <= (ElfW (Addr)) vtbl_ptr
        && (ElfW (Addr)) vtbl_ptr <= whitelist_data[i].high_addr)
      {
        whitelist_fail_count++;
        return;
      }

  snprintf (log_msg, sizeof (log_msg), "Looking for vtable %p in set %p.\n",
            vtbl_ptr,
            is_set_handle_handle (*data_set_ptr) ?
              ptr_from_set_handle_handle (*data_set_ptr) :
              *data_set_ptr);
  __vtv_log_verification_failure (log_msg, false);

  const char *format_str =
            "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";
  snprintf (log_msg, sizeof (log_msg), format_str, vtbl_ptr, *data_set_ptr);
  __vtv_log_verification_failure (log_msg, false);
  __vtv_log_verification_failure ("  Backtrace: \n", true);

  const char *fail_msg = "Potential vtable pointer corruption detected!!\n";
  vtv_fail (fail_msg, data_set_ptr, vtbl_ptr);
}


#ifndef VTV_STATIC_VERIFY

s2s * __vtv_symbol_unification_map VTV_PROTECTED_HIDDEN_VAR = NULL;

#include "vtv_rts_core.cc"

#else

s2s * __vtv_symbol_unification_map VTV_PROTECTED_GLOBAL_VAR = NULL;

#endif
