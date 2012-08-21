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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <execinfo.h>

#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <link.h>
#include <fcntl.h>

/* For gthreads suppport */
#include <bits/c++config.h>
#include <ext/concurrence.h>

#include "vtv_utils.h"
#include "vtv_malloc.h"
#include "vtv_set.h"
#include "vtv_rts.h"
#include "vtv_fail.h"

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


/* Be careful about initialization of statics in this file.  Some of
   the routines below are called before any runtime initialization for
   statics in this file will be done. For example, dont try to
   initialize any of these statics with a runtime call (for ex:
   sysconf. The initialization will happen after calls to the routines
   to protect/unprotec the vtabla_map variables */

static const int debug_hash = 0;
static const int debug_functions = 0;
static const int debug_register_pairs = 0;

/* Put the following variables in a rel.ro section so that the are protected.
   They are explicitly unprotected and protected again by calls to VTV_unprotect
   and VTV_protect */

static int log_file_fd VTV_PROTECTED_VAR = -1;

/* types needed by insert_only_hash_sets */
typedef uintptr_t int_vptr;

struct vptr_hash {
  size_t operator()(int_vptr v) const
  {
    const uint32_t x = 0x7a35e4d9;
    const int shift = (sizeof(v) == 8) ? 23 : 21;
    v = x * v;
    return v ^ (v >> shift);
  }
};

struct vptr_set_alloc {
  void* operator()(size_t n) const
  {
    return VTV_malloc(n);
  }
};

typedef insert_only_hash_sets<int_vptr, vptr_hash, vptr_set_alloc> vtv_sets;
typedef vtv_sets::insert_only_hash_set vtv_set;
typedef vtv_set * vtv_set_handle;

struct mprotect_data {
  int prot_mode;
  unsigned long page_size;
};

static int
dl_iterate_phdr_callback (struct dl_phdr_info *info, size_t,
                          void *data)
{
  mprotect_data * mdata = (mprotect_data *) data;
  int j;

  if (debug_functions)
    fprintf(stderr, "looking at load module %s to change permissions to %s\n", 
            info->dlpi_name,
            (mdata->prot_mode & PROT_WRITE) ? "READ/WRITE" : "READ-ONLY");
  for (j = 0; j < info->dlpi_phnum; j++)
    {
      ElfW(Addr) relocated_start_addr = info->dlpi_addr + info->dlpi_phdr[j].p_vaddr;
      ElfW(Addr) unrelocated_start_addr = info->dlpi_phdr[j].p_vaddr;
      ElfW(Word) size_in_memory = info->dlpi_phdr[j].p_memsz;

      if (debug_functions)
        fprintf(stderr, "Segment info relocated=%p unrelocated=%p size=%u\n", 
                (void *)relocated_start_addr, (void *)unrelocated_start_addr, size_in_memory);

      if (info->dlpi_phdr[j].p_type == PT_GNU_RELRO)
        {
          if (debug_functions)
            fprintf(stderr, "Found RELRO segment. relocated=%p unrelocated=%p size=%u\n", 
                    (void *)relocated_start_addr, (void *)unrelocated_start_addr, size_in_memory);

          ElfW(Addr) mp_low = relocated_start_addr & ~(mdata->page_size - 1);
          size_t mp_size = relocated_start_addr + size_in_memory - mp_low - 1;

          if (mprotect((void *)mp_low, mp_size, mdata->prot_mode) == -1)
            {
              if (debug_functions)
                {
                  fprintf(stderr, "Failed called to mprotect for %s error: ", 
			  (mdata->prot_mode & PROT_WRITE) ? 
                          "READ/WRITE" : "READ-ONLY");
                  perror(NULL);
                }
              VTV_error();
            }
          else if (debug_functions)
            fprintf(stderr, "mprotect'ed range [%p, %p]\n", 
		    (void *)mp_low, (char *)mp_low + mp_size);

          break;
        }
    }
  return 0;
}

/* Unprotect all the vtable map vars and other side data that is used
   to keep the core hash_map data. All of these data have been put
   into relro sections */
static void
VTV_unprotect_vtable_vars (void)
{
  mprotect_data mdata; 

  mdata.prot_mode = PROT_READ | PROT_WRITE;
  mdata.page_size = sysconf(_SC_PAGE_SIZE);
  dl_iterate_phdr (dl_iterate_phdr_callback, (void *) &mdata);
}

/* Protect all the vtable map vars and other side data that is used
   to keep the core hash_map data. All of these data have been put
   into relro sections */
static void
VTV_protect_vtable_vars (void)
{
  mprotect_data mdata; 

  mdata.prot_mode = PROT_READ;
  mdata.page_size = sysconf(_SC_PAGE_SIZE);
  dl_iterate_phdr (dl_iterate_phdr_callback, (void *) &mdata);
}

// TODO: NEED TO PROTECT THIS VAR
static int change_permissions_count = 0;

#if defined __GTHREAD_MUTEX_INIT
static __gthread_mutex_t register_pairs_lock VTV_PROTECTED_VAR = __GTHREAD_MUTEX_INIT;
// TODO: NEED TO PROTECT THIS VAR
static __gthread_mutex_t change_permissions_lock = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t register_pairs_lock VTV_PROTECTED_VAR;
// TODO: NEED TO PROTECT THIS VAR
static __gthread_mutex_t change_permissions_lock;

static void
initialize_change_permissions_mutexes ()
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&register_pair_lock);
  __GTHREAD_MUTEX_INIT_FUNCTION (&change_permissions_lock);
}
#endif

void 
__VLTChangePermission (int perm)
{
  if (debug_functions)
    {
      if (perm == __VLTP_READ_WRITE)
	fprintf (stdout, "Changing VLT permisisons to Read-Write.\n");
      else if (perm == __VLTP_READ_ONLY)
	fprintf (stdout, "Changing VLT permissions to Read-only.\n");
      else
	fprintf (stdout, "Unrecognized permissions value: %d\n", perm);
    }

#ifndef __GTHREAD_MUTEX_INIT
  static __gthread_once_t mutex_once VTV_PROTECTED_VAR = __GTHREAD_ONCE_INIT;

  __gthread_once (&mutex_once, initialize_change_permissions_mutexes);
#endif

  /* Ordering of these unprotect/protect calls is very important. 
     You first need to unprotect all the map vars and side
     structures before you do anything with the core data
     structures (hash_maps) */

  if (perm == __VLTP_READ_WRITE)
    {
      // TODO: need to revisit this code for dlopen. It most probably is
      // not unlocking the protected vtable vars after for a load module
      // that is not the first load module
      __gthread_mutex_lock(&change_permissions_lock);
      VTV_ASSERT(change_permissions_count >= 0);
      change_permissions_count++;
      if (change_permissions_count == 1)
        {
          VTV_unprotect_vtable_vars ();
          VTV_malloc_init ();
          VTV_malloc_unprotect ();
        }
      __gthread_mutex_unlock(&change_permissions_lock);

      /* Now grab the register pairs lock so that only one thread is
         modifying the register pair sets */
      __gthread_mutex_lock(&register_pairs_lock);
    }
  else if (perm == __VLTP_READ_ONLY)
    {
      /* At this point we must be done with all register pair calls
         for this load module. */
      __gthread_mutex_unlock(&register_pairs_lock);

      __gthread_mutex_lock(&change_permissions_lock);
      if (change_permissions_count == 1)
        {
          VTV_malloc_protect ();
          VTV_protect_vtable_vars ();
        }
      change_permissions_count--;
      VTV_ASSERT(change_permissions_count >= 0);
      __gthread_mutex_unlock(&change_permissions_lock);

      if (debug_hash)
        vtv_sets::dump_statistics();
    }
}

/* For some reason, when the char * names get passed into these
   functions, they are missing the '\0' at the end; therefore we
   also pass in the length of the string and make sure, when writing
   out the names, that we only write out the correct number of
   bytes. */
void
log_register_pairs (int fd, const char *format_string_dummy, int format_arg1,
		    int format_arg2, char *base_var_name, char *vtable_name,
		    int_vptr vtbl_ptr)
{
  if (fd == -1)
    return;

  char format_string[60];

  /* format_string needs to contain something like "%.10s" (for
     example) to write a vtable_name that is of length
     10. Unfortunately the length varies with every name, so we need to
     generate a new format string, with the correct length, EACH TIME.
     That is what the 'format_string_dummy' parameter is for.  It
     contains something like '%%.%ds', and we then use that plus the
     length argument to generate the correct format_string, to allow
     us to write out the string that is missing the '\0' at it's
     end. */

  snprintf (format_string, sizeof(format_string), format_string_dummy, 
            format_arg1, format_arg2);
  vtv_add_to_log(fd, format_string, base_var_name, vtable_name, vtbl_ptr);
}

/* For some reason, when the char * names get passed into these
   functions, they are missing the '\0' at the end; therefore we
   also pass in the length of the string and make sure, when writing
   out the names, that we only write out the correct number of
   bytes. */
void
print_debugging_message (const char *format_string_dummy, int format_arg1,
			 int format_arg2,
			 char *str_arg1, char *str_arg2)
{
  char format_string[50];

  /* format_string needs to contain something like "%.10s" (for
     example) to write a vtable_name that is of length
     10. Unfortunately the length varies with every name, so we need to
     generate a new format string, with the correct length, EACH TIME.
     That is what the 'format_string_dummy' parameter is for.  It
     contains something like '%%.%ds', and we then use that plus the
     length argument to generate the correct format_string, to allow
     us to write out the string that is missing the '\0' at it's
     end. */

  snprintf (format_string, sizeof(format_string), format_string_dummy, format_arg1, format_arg2);

  fprintf (stdout, format_string, str_arg1, str_arg2);
}

// TODO: move the calculation of power of two to the code that calculates the hint
static unsigned 
next_power_of_two(unsigned int num)
{
  unsigned result = 1;
  while (result < num)
    result = result << 1;
  return result;
}

/* TODO: Why is this returning anything
   remove unnecessary arguments */
void *
__VLTRegisterPair (void **data_pointer, void *test_value, int size_hint,
		   char *base_ptr_var_name, int len1, char *vtable_name,
		   int len2)
{
  int_vptr vtbl_ptr = (int_vptr) test_value;
  vtv_set_handle * handle_ptr = (vtv_set_handle *) data_pointer;

#ifndef __GTHREAD_MUTEX_INIT
  static __gthread_once_t mutex_once VTV_PROTECTED_VAR = __GTHREAD_ONCE_INIT;

  __gthread_once (&mutex_once, initialize_mutex_once);
#endif

  if (*handle_ptr == NULL)
    {
      // use a temporary handle to create the set and insert the vtbl ptr
      // before modifying the original handle to avoid possible race
      // condition with reader.
      vtv_set_handle tmp_handle;
      // TODO: verify return value
      // TODO: pass power of 2 hint.
      vtv_sets::create(next_power_of_two(size_hint), &tmp_handle);
      vtv_sets::insert(vtbl_ptr, &tmp_handle);
      *handle_ptr = tmp_handle;
    }
  else
    // TODO: handle size hint in this case
    // TODO: verify return value?
    vtv_sets::insert(vtbl_ptr, handle_ptr);


  if (debug_functions && base_ptr_var_name && vtable_name)
    print_debugging_message ("Registered %%.%ds : %%.%ds\n", len1, len2,
			     base_ptr_var_name, vtable_name);
  if (debug_register_pairs)
    {
      if (log_file_fd == -1)
	log_file_fd = vtv_open_log("/tmp/vtv_register_pairs.log");

      log_register_pairs (log_file_fd, "Registered %%.%ds : %%.%ds (%%p)\n",
			  len1, len2,
			  base_ptr_var_name, vtable_name, vtbl_ptr);
    }

  return NULL;
}

static void PrintStackTrace()
{
  #define STACK_DEPTH 20
  void * callers[STACK_DEPTH];
  int actual_depth = backtrace(callers, STACK_DEPTH);
  char ** symbols = backtrace_symbols(callers, actual_depth);
  if (symbols == NULL )
    {
      fprintf(stderr, "Could not get backtrace\n");
      return;
    }

  for (int i = 0; i < actual_depth; i++)
    fprintf(stderr, "%s\n", symbols[i]);

  free(symbols);
}

void *
__VLTVerifyVtablePointerDebug (void ** data_pointer, void * test_value,
                               char * base_vtbl_var_name, int len1, char * vtable_name,
                               int len2)
{
  vtv_set_handle * handle_ptr = (vtv_set_handle *)data_pointer;
  int_vptr vtbl_ptr = (int_vptr) test_value;

  if (vtv_sets::contains(vtbl_ptr, handle_ptr))
    {
      if (debug_functions)
	fprintf (stdout, "Verified object vtable pointer = %lx\n", 
                 (unsigned long)vtbl_ptr);
    }
  else
    {
      /* The data structure is not NULL, but we failed to find our
         object's vtpr in it.  Write out information and call abort.*/
      if (base_vtbl_var_name && vtable_name)
	print_debugging_message ("Looking for %%.%ds in %%.%ds \n", len2, len1,
				 vtable_name, base_vtbl_var_name);
      fprintf (stderr, "FAILED to verify object vtable pointer=%lx!!!\n",
               (unsigned long)vtbl_ptr);

      __vtv_verify_fail (data_pointer, test_value);
      /* Normally __vtv_verify_fail will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replace __vtv_verify_fail with
         some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
      fprintf (stderr, "Returned from __vtv_verify_fail."
               "  Secondary verification succeeded.\n");
    }

  return test_value;
}

void *
__VLTVerifyVtablePointer (void ** data_pointer, void * test_value)
{
  vtv_set_handle * handle_ptr = (vtv_set_handle *) data_pointer;
  int_vptr vtbl_ptr = (int_vptr) test_value;

  if (!vtv_sets::contains(vtbl_ptr, handle_ptr))
    {
      __vtv_verify_fail (data_pointer, test_value);
      /* Normally __vtv_verify_fail will call abort, so we won't
         execute the return below.  If we get this far, the assumption
         is that the programmer has replace __vtv_verify_fail with
         some kind of secondary verification AND this secondary
         verification succeeded, so the vtable pointer is valid.  */
    }

  return test_value;
}

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

static void
vtv_fail (const char *msg, void **data_set_ptr, void *vtbl_ptr)
{
  int fd;

  fd = open ("/dev/tty", O_WRONLY);
  if (fd != -1)
    {
      char buffer[120];
      int buf_len;
      const char *format_str =
            "*** Unable to verify vtable pointer (0x%p) in set (0x%p) *** \n";

      snprintf (buffer, sizeof (buffer), format_str, vtbl_ptr, *data_set_ptr);
      buf_len = strlen (buffer);
      write (fd, buffer, buf_len);
      close (fd);
    }

  __vtv_really_fail (msg);
}

void
__vtv_verify_fail (void **data_set_ptr, void *vtbl_ptr)
{
  const char *msg = "Potential vtable pointer corruption detected!!";
  vtv_fail (msg, data_set_ptr, vtbl_ptr);
}
