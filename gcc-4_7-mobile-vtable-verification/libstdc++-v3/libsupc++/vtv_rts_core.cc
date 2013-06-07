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
  library, __vtv_register_pair.  __vtv_register_pair takes two arguments,
  a vtable map variable and the address of a vtable.  If the vtable
  map variable is currently NULL, it creates a new data set (hash
  table), makes the vtable map variable point to the new data set, and
  inserts the vtable address into the data set.  If the vtable map
  variable is not NULL, it just inserts the vtable address into the
  data set.  In order to make sure that our data sets are built before
  any verification calls happen, we create a special constructor
  initialization function for each compilation unit, give it a very
  high initialization priority, and insert all of our calls to
  __vtv_register_pair into our special constructor initialization
  function.  */

/* This file contains the main externally visible runtime library
   functions for vtable verification: __vtv_change_permission,
   __vtv_register_pair, and __vtv_verify_vtable_pointer.  It also contains
   debug versions __vtv_register_pair_debug and
   __vtv_verify_vtable_pointer_debug, which have extra parameters in order
   to make it easier to debug verification failures.

   This file also contains the failure functions that get called when
   a vtable pointer is not found in the data set.  Two particularly
   important functions are __vtv_verify_fail and __vtv_really_fail.
   They are both externally visible.  __vtv_verify_fail is defined in
   such a way that it can be replaced by a programmer, if desired.  It
   is the function that __vtv_verify_vtable_pointer calls if it can't
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
   (__VLTChangePermissino, __vtv_register_pair and
   __vtv_verify_vtable_pointer) with stub functions that do nothing.  If
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

/* Be careful about initialization of statics in this file.  Some of
   the routines below are called before any runtime initialization for
   statics in this file will be done. For example, dont try to
   initialize any of these statics with a runtime call (for ex:
   sysconf. The initialization will happen after calls to the routines
   to protect/unprotec the vtabla_map variables */

/* The following variables are used only for debugging and performance
   tuning purposes. Therefore they do not need to be "protected".
   They cannot be used to attack the vtable verification system and if
   they become corrupted it will not affect the correctness or
   security of any of the rest of the vtable verification feature.  */

static const bool local_debug_hash = HASHTABLE_STATS;
static const int debug_init = 0;
static const int debug_verify_vtable = 0;

static int init_log_fd = -1;

/*  Variables needed for getting the statistics about the hashtable set.  */
#if HASHTABLE_STATS
_AtomicStatCounter stat_contains = 0;
_AtomicStatCounter stat_insert = 0;
_AtomicStatCounter stat_resize = 0;
_AtomicStatCounter stat_create = 0;
_AtomicStatCounter stat_probes_in_non_trivial_set = 0;
_AtomicStatCounter stat_contains_size0 = 0;
_AtomicStatCounter stat_contains_size1 = 0;
_AtomicStatCounter stat_contains_size2 = 0;
_AtomicStatCounter stat_contains_size3 = 0;
_AtomicStatCounter stat_contains_size4 = 0;
_AtomicStatCounter stat_contains_size5 = 0;
_AtomicStatCounter stat_contains_size6 = 0;
_AtomicStatCounter stat_contains_size7 = 0;
_AtomicStatCounter stat_contains_size8 = 0;
_AtomicStatCounter stat_contains_size9 = 0;
_AtomicStatCounter stat_contains_size10 = 0;
_AtomicStatCounter stat_contains_size11 = 0;
_AtomicStatCounter stat_contains_size12 = 0;
_AtomicStatCounter stat_contains_size13_or_more = 0;
_AtomicStatCounter stat_contains_sizes = 0;
_AtomicStatCounter stat_grow_from_size0_to_1 = 0;
_AtomicStatCounter stat_grow_from_size1_to_2 = 0;
_AtomicStatCounter stat_double_the_number_of_buckets = 0;
_AtomicStatCounter stat_insert_found_hash_collision = 0;
_AtomicStatCounter stat_contains_in_non_trivial_set = 0;
_AtomicStatCounter stat_insert_key_that_was_already_present = 0;
#endif

typedef const s2s::key_type  vtv_symbol_key;

static inline void
register_set_common (void **set_handle_ptr, size_t num_args,
                     void **vtable_ptr_array, bool debug)
{
  /* Now figure out what pointer to use for the set pointer, for the
     inserts.  */
  vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;

  if (debug)
    VTV_DEBUG_ASSERT (__vtv_symbol_unification_map != NULL);

  if (!is_set_handle_handle (*set_handle_ptr))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

  /* Now we've got the set and it's initialized, add the vtable
     pointers.  */
  for (size_t index = 0; index < num_args; ++index)
    {
      int_vptr vtbl_ptr = vptr_to_int_vptr (vtable_ptr_array[index]);
      vtv_sets::insert (vtbl_ptr, handle_ptr);
    }
}

static inline void
register_pair_common (void **set_handle_ptr, const void *vtable_ptr,
                      const char *set_symbol_name, const char *vtable_name,
                      bool debug)
{
  /* Now we've got the set and it's initialized, add the vtable
     pointer (assuming that it's not NULL...It may be NULL, as we may
     have called this function merely to initialize the set
     pointer).  */
  int_vptr vtbl_ptr = vptr_to_int_vptr (vtable_ptr);
  if (vtbl_ptr)
    {
      vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;
      if (debug)
        VTV_DEBUG_ASSERT (__vtv_symbol_unification_map != NULL);
      if (!is_set_handle_handle (*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

      vtv_sets::insert (vtbl_ptr, handle_ptr);
    }

  if (debug && debug_init)
    {
      if (init_log_fd == -1)
        init_log_fd = __vtv_open_log("vtv_init.log");

      __vtv_add_to_log(init_log_fd,
                       "Registered %s : %s (%p) 2 level deref = %s\n",
                       set_symbol_name, vtable_name, vtbl_ptr,
                       is_set_handle_handle(*set_handle_ptr) ? "yes" : "no" );
    }
}

/* Ideally it would be nice if the library always provided the 2
   versions of the runtime libraries. However, when we use VTV_DEBUG
   we want to make sure that only the debug versions are being
   used. We could change this once the code is more stable.  */

#ifdef VTV_DEBUG

/* Global file descriptor variables for logging, tracing and debugging.  */
static int verify_vtable_log_fd = -1;

/* This holds a formatted error logging message, to be written to the
   vtable verify failures log.  */
static char debug_log_message[1024];

static inline void
init_set_symbol_debug (void **set_handle_ptr, const void *set_symbol_key,
                       size_t size_hint)
{
  VTV_DEBUG_ASSERT (set_handle_ptr);

  if (__vtv_symbol_unification_map == NULL)
    {
      /* TODO:  For now we have chosen 1024, but we need to come up with a
         better initial size for this.  */
      __vtv_symbol_unification_map = s2s::create (1024);
      VTV_DEBUG_ASSERT (__vtv_symbol_unification_map);
    }

  vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;
  vtv_symbol_key *symbol_key_ptr = (vtv_symbol_key *) set_symbol_key;

  const s2s::value_type * map_value_ptr =
                              __vtv_symbol_unification_map->get (symbol_key_ptr);
  char buffer[200];
  if (map_value_ptr == NULL)
    {
      if (*handle_ptr != NULL)
        {
          snprintf (buffer, sizeof (buffer),
                    "*** Found non-NULL local set ptr %p missing for symbol"
                    " %.*s",
                    *handle_ptr, symbol_key_ptr->n, symbol_key_ptr->bytes);
          __vtv_log_verification_failure (buffer, true);
          VTV_DEBUG_ASSERT (0);
        }
    }
  else if (*handle_ptr != NULL &&
           (handle_ptr != *map_value_ptr &&
            ptr_from_set_handle_handle (*handle_ptr) != *map_value_ptr))
    {
      VTV_DEBUG_ASSERT (*map_value_ptr != NULL);
      snprintf (buffer, sizeof(buffer),
                "*** Found diffence between local set ptr %p and set ptr %p"
                "for symbol %.*s",
                *handle_ptr, *map_value_ptr,
                symbol_key_ptr->n, symbol_key_ptr->bytes);
      __vtv_log_verification_failure (buffer, true);
      VTV_DEBUG_ASSERT (0);
    }
  else if (*handle_ptr == NULL)
    {
      /* Execution should not reach this point.  */
    }

  if (*handle_ptr != NULL)
    {
      if (!is_set_handle_handle (*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);
      vtv_sets::resize (size_hint, handle_ptr);
      return;
    }

  VTV_DEBUG_ASSERT (*handle_ptr == NULL);
  if (map_value_ptr != NULL)
    {
      if (*map_value_ptr == handle_ptr)
        vtv_sets::resize (size_hint, *map_value_ptr);
      else
        {
          /* The one level handle to the set already exists. So, we
             are adding one level of indirection here and we will
             store a pointer to the one level handle here.  */

          vtv_set_handle_handle * handle_handle_ptr =
                                           (vtv_set_handle_handle *)handle_ptr;
          *handle_handle_ptr = set_handle_handle(*map_value_ptr);
          VTV_DEBUG_ASSERT (*handle_handle_ptr != NULL);

          /* The handle can itself be NULL if the set has only
             been initiazlied with size hint == 1. */
          vtv_sets::resize (size_hint, *map_value_ptr);
        }
    }
  else
    {
      /* We will create a new set. So, in this case handle_ptr is the
         one level pointer to the set handle.  Create copy of map name
         in case the memory where this comes from gets unmapped by
         dlclose.  */
      size_t map_key_len = symbol_key_ptr->n + sizeof (vtv_symbol_key);
      void *map_key = __vtv_malloc (map_key_len);

      memcpy (map_key, symbol_key_ptr, map_key_len);

      s2s::value_type *value_ptr;
      __vtv_symbol_unification_map =
        __vtv_symbol_unification_map->find_or_add_key ((vtv_symbol_key *)map_key,
                                                     &value_ptr);
      *value_ptr = handle_ptr;

      /*  TODO: We should verify the return value. */
      vtv_sets::create (size_hint, handle_ptr);
      VTV_DEBUG_ASSERT (size_hint <= 1 || *handle_ptr != NULL);
    }

  if (debug_init)
    {
      if (init_log_fd == -1)
        init_log_fd = __vtv_open_log ("vtv_init.log");

      __vtv_add_to_log (init_log_fd,
			"Init handle:%p for symbol:%.*s hash:%u size_hint:%lu"
			"number of symbols:%lu \n",
			set_handle_ptr, symbol_key_ptr->n,
			symbol_key_ptr->bytes, symbol_key_ptr->hash, size_hint,
			__vtv_symbol_unification_map->size ());
    }
}

/* This routine initializes a set handle to a vtable set. It makes
   sure that there is only one set handle for a particular set by
   using a map from set name to pointer to set handle. Since there
   will be multiple copies of the pointer to the set handle (one per
   compilation unit that uses it), it makes sure to initialize all the
   pointers to the set handle so that the set handle is unique. To
   make this a little more efficient and avoid a level of indirection
   in some cases, the first pointer to handle for a particular handle
   becomes the handle itself and the other pointers will point to the
   set handle.  This is the debug version of this function, so it
   outputs extra debugging messages and logging.  SET_HANDLE_PTR is
   the address of the vtable map variable, SET_SYMBOL_KEY is the hash
   table key (containing the name of the map variable and the hash
   value) and SIZE_HINT is a guess for the best initial size for the
   set of vtable pointers that SET_HANDLE_POINTER will point to.  */

void
__vtv_register_set_debug (void **set_handle_ptr, const void *set_symbol_key,
                          size_t size_hint, size_t num_args,
                          void **vtable_ptr_array)
{
#ifndef VTV_EMPTY_VERIFY
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&__vtv_stats.num_calls_to_regset);

  VTV_DEBUG_ASSERT (set_handle_ptr != NULL);
  init_set_symbol_debug (set_handle_ptr, set_symbol_key, size_hint);

  register_set_common (set_handle_ptr, num_args, vtable_ptr_array, true);

  accumulate_cycle_count (&__vtv_stats.regset_cycles, start);
#endif
}

void
__vtv_register_pair_debug (void **set_handle_ptr, const  void *set_symbol_key,
                           size_t size_hint, const void *vtable_ptr,
                           const char *set_symbol_name, const char *vtable_name)
{
#ifndef VTV_EMPTY_VERIFY
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&__vtv_stats.num_calls_to_regpair);

  VTV_DEBUG_ASSERT (set_handle_ptr != NULL);
  init_set_symbol_debug (set_handle_ptr, set_symbol_key, size_hint);

  register_pair_common (set_handle_ptr, vtable_ptr, set_symbol_name, vtable_name,
                        true);

  accumulate_cycle_count (&__vtv_stats.regpair_cycles, start);
#endif
}

/* This function is called from __vtv_verify_vtable_pointer_debug; it
   sends as much debugging information as it can to the error log
   file, then calls __vtv_verify_fail.  SET_HANDLE_PTR is the pointer
   to the set of valid vtable pointers, VTBL_PTR is the pointer that
   was not found in the set, and DEBUG_MSG is the message to be
   written to the log file before failing. n */

static void
__vtv_verify_fail_debug (void **set_handle_ptr, const void *vtbl_ptr,
                         const char *debug_msg)
{
  __vtv_log_verification_failure (debug_msg, false);

  /* Call the public interface in case it has been overwritten by
     user.  */
  __vtv_verify_fail (set_handle_ptr, vtbl_ptr);

  __vtv_log_verification_failure ("Returned from __vtv_verify_fail."
				  " Secondary verification succeeded.\n", false);
}

/* This is the debug version of the verification function.  It takes
   the address of a vtable map variable (SET_HANDLE_PTR) and a
   VTABLE_PTR to validate, as well as the name of the vtable map
   variable (SET_SYMBOL_NAME) and VTABLE_NAME, which are used for
   debugging messages.  It checks to see if VTABLE_PTR is in the set
   pointed to by SET_HANDLE_PTR.  If so, it returns VTABLE_PTR,
   otherwise it calls __vtv_verify_fail, which usually logs error
   messages and calls abort.  */

const void *
__vtv_verify_vtable_pointer_debug (void **set_handle_ptr,
                                   const void *vtable_ptr,
                                   const char *set_symbol_name,
                                   const char *vtable_name)
{
#ifndef VTV_EMPTY_VERIFY
  increment_num_calls (&__vtv_stats.num_calls_to_verify_vtable);
  unsigned long long start = get_cycle_count ();
  VTV_DEBUG_ASSERT (set_handle_ptr != NULL && *set_handle_ptr != NULL);
  int_vptr vtbl_ptr = vptr_to_int_vptr (vtable_ptr);

  vtv_set_handle *handle_ptr;
  if (!is_set_handle_handle (*set_handle_ptr))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

  if (vtv_sets::contains (vtbl_ptr, handle_ptr))
    {
      if (debug_verify_vtable)
        {
          if (verify_vtable_log_fd == -1)
            __vtv_open_log ("vtv_verify_vtable.log");
          __vtv_add_to_log (verify_vtable_log_fd,
			    "Verified %s %s value = %p\n",
			    set_symbol_name, vtable_name, vtable_ptr);
        }
    }
  else
    {
      /* We failed to find the vtable pointer in the set of valid
         pointers.  Log the error data and call the failure
         function.  */
      snprintf (debug_log_message, sizeof (debug_log_message),
                "Looking for %s in %s\n", vtable_name, set_symbol_name);
      __vtv_verify_fail_debug (set_handle_ptr, vtable_ptr, debug_log_message);

      /* Normally __vtv_verify_fail_debug will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replaced __vtv_verify_fail_debug
         with some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
    }
  accumulate_cycle_count (&__vtv_stats.verify_vtable_cycles, start);
#endif /* ifndef VTV_EMPTY_VERIFY*/

  return vtable_ptr;
}

#else /* ifdef VTV_DEBUG */

static inline void
init_set_symbol (void **set_handle_ptr, const void *set_symbol_key,
                 size_t size_hint)
{
  vtv_set_handle *handle_ptr = (vtv_set_handle *) set_handle_ptr;

  if (*handle_ptr != NULL)
    {
      if (!is_set_handle_handle (*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);
      vtv_sets::resize (size_hint, handle_ptr);
      return;
    }

  if (__vtv_symbol_unification_map == NULL)
    __vtv_symbol_unification_map = s2s::create (1024);

  vtv_symbol_key *symbol_key_ptr = (vtv_symbol_key *) set_symbol_key;
  const s2s::value_type *map_value_ptr =
                              __vtv_symbol_unification_map->get (symbol_key_ptr);

  if (map_value_ptr != NULL)
    {
      if (*map_value_ptr == handle_ptr)
        vtv_sets::resize (size_hint, *map_value_ptr);
      else
        {
          /* The one level handle to the set already exists. So, we
             are adding one level of indirection here and we will
             store a pointer to the one level pointer here.  */
          vtv_set_handle_handle *handle_handle_ptr =
                                          (vtv_set_handle_handle *) handle_ptr;
          *handle_handle_ptr = set_handle_handle (*map_value_ptr);
          vtv_sets::resize (size_hint, *map_value_ptr);
        }
    }
  else
    {
      /* We will create a new set. So, in this case handle_ptr is the
         one level pointer to the set handle.  Create copy of map name
         in case the memory where this comes from gets unmapped by
         dlclose.  */
      size_t map_key_len = symbol_key_ptr->n + sizeof (vtv_symbol_key);
      void * map_key = __vtv_malloc (map_key_len);
      memcpy (map_key, symbol_key_ptr, map_key_len);

      s2s::value_type * value_ptr;
      __vtv_symbol_unification_map =
        __vtv_symbol_unification_map->find_or_add_key ((vtv_symbol_key *)map_key,
                                                     &value_ptr);

      *value_ptr = handle_ptr;

      /* TODO: We should verify the return value.  */
      vtv_sets::create (size_hint, handle_ptr);
    }
}

/* This routine initializes a set handle to a vtable set. It makes
   sure that there is only one set handle for a particular set by
   using a map from set name to pointer to set handle. Since there
   will be multiple copies of the pointer to the set handle (one per
   compilation unit that uses it), it makes sure to initialize all the
   pointers to the set handle so that the set handle is unique. To
   make this a little more efficient and avoid a level of indirection
   in some cases, the first pointer to handle for a particular handle
   becomes the handle itself and the other pointers will point to the
   set handle.  SET_HANDLE_PTR is the address of the vtable map
   variable, SET_SYMBOL_KEY is the hash table key (containing the name
   of the map variable and the hash value) and SIZE_HINT is a guess
   for the best initial size for the set of vtable pointers that
   SET_HANDLE_POINTER will point to.*/


void
__vtv_register_set (void **set_handle_ptr, const void *set_symbol_key,
                    size_t size_hint, size_t num_args, void **vtable_ptr_array)
{
#ifndef VTV_EMPTY_VERIFY
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&__vtv_stats.num_calls_to_regset);

  init_set_symbol (set_handle_ptr, set_symbol_key, size_hint);
  register_set_common (set_handle_ptr, num_args, vtable_ptr_array, false);

  accumulate_cycle_count (&__vtv_stats.regset_cycles, start);
#endif /* ifndef VTV_EMPTY_VERIFY  */
}



void
__vtv_register_pair (void **set_handle_ptr, const  void *set_symbol_key,
                     size_t size_hint, const void *vtable_ptr)
{
#ifndef VTV_EMPTY_VERIFY
  unsigned long long start = get_cycle_count ();
  increment_num_calls (&__vtv_stats.num_calls_to_regpair);

  init_set_symbol (set_handle_ptr, set_symbol_key, size_hint);
  register_pair_common (set_handle_ptr, vtable_ptr, NULL, NULL,  false);

  accumulate_cycle_count (&__vtv_stats.regpair_cycles, start);
#endif /* ifndef VTV_EMPTY_VERIFY  */
}

/* This is the main verification function.  It takes the address of a
   vtable map variable (SET_HANDLE_PTR) and a VTABLE_PTR to validate.
   It checks to see if VTABLE_PTR is in the set pointed to by
   SET_HANDLE_PTR.  If so, it returns VTABLE_PTR, otherwise it calls
   __vtv_verify_fail, which usually logs error messages and calls
   abort.  Since this function gets called VERY frequently, it is
   important for it to be as efficient as possible.  */

const void *
__vtv_verify_vtable_pointer (void ** set_handle_ptr, const void * vtable_ptr)
{
#ifndef VTV_EMPTY_VERIFY
  increment_num_calls (&__vtv_stats.num_calls_to_verify_vtable);
  unsigned long long start = get_cycle_count ();
  int_vptr vtbl_ptr = vptr_to_int_vptr (vtable_ptr);

  vtv_set_handle *handle_ptr;
  if (VTV_LIKELY (!is_set_handle_handle (*set_handle_ptr)))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle (*set_handle_ptr);

  if (VTV_UNLIKELY (!vtv_sets::contains (vtbl_ptr, handle_ptr)))
    {
      __vtv_verify_fail ((void **) handle_ptr, vtable_ptr);
      /* Normally __vtv_verify_fail will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replaced __vtv_verify_fail with
         some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
    }
  accumulate_cycle_count (&__vtv_stats.verify_vtable_cycles, start);
#endif /* ifndef VTV_EMPTY_VERIFY  */

  return vtable_ptr;
}

#endif /* else-clause of ifdef VTV_DEBUG  */
