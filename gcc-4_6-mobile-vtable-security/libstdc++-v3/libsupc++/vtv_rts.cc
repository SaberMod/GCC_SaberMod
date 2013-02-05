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

/* No need to mark the following variables with VTV_PROTECTED_VAR.
   These are either const or are only used for debugging/tracing.
   debugging/tracing will not be ON on production environments */
static const int debug_hash = HASHTABLE_STATS;
// TODO: Make sure debugging messages under this guard dont use malloc!
static const int debug_functions = 0;
static const int debug_init = 0;
static const int debug_verify_vtable = 0;

#ifdef VTV_DEBUG
static int init_log_fd = -1;
static int verify_vtable_log_fd = -1;
static void __vtv_verify_fail_debug (void **, const void *, const char *);
static char debug_log_message[1024];
#endif

// TODO: should this be under VTV_DEBUG?
static int vtv_failures_log_fd = -1;
#if HASHTABLE_STATS
static int set_log_fd = -1;
#endif

/* Put the following variables in a rel.ro section so that the are
   protected.  They are explicitly unprotected and protected again by
   calls to VTV_unprotect and VTV_protect */

#ifdef __GTHREAD_MUTEX_INIT
// TODO: NEED TO PROTECT THIS VAR  !!!!!!!!!!!!!!!!!!!
static __gthread_mutex_t change_permissions_lock = __GTHREAD_MUTEX_INIT;
#else
// TODO: NEED TO PROTECT THIS VAR  !!!!!!!!!!!!!!!!!!!
static __gthread_mutex_t change_permissions_lock;
#endif


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
typedef vtv_set_handle * vtv_set_handle_handle; 

struct mprotect_data {
  int prot_mode;
  unsigned long page_size;
};

static void
log_error_message (const char *log_msg, bool generate_backtrace);

static ssize_t
ReadPersistent (int fd, void *buf, size_t count)
{
  char *buf0 = (char *) buf;
  size_t num_bytes = 0;
  while (num_bytes < count)
    {
      int len;
      len = read (fd, buf0 + num_bytes, count - num_bytes);
      if (len < 0)
        return -1;
      if (len == 0)
        break;
      num_bytes += len;
    }

  return num_bytes;
}

static ssize_t
ReadFromOffset (int fd, void *buf, const size_t count,
                const off_t offset)
{
  off_t off = lseek (fd, offset, SEEK_SET);
  if (off != (off_t) -1)
    return ReadPersistent (fd, buf, count);
  return -1;
}

static void
log_memory_protection_data (char *message)
{
  static int log_fd = -1;

  if (log_fd == -1)
    log_fd = vtv_open_log ("vtv_memory_protection_data.log");

  vtv_add_to_log (log_fd, "%s", message);
}

/* This is the callback function used by dl_iterate_phdr (which is
   called from VTV_unprotect_vtable_vars and VTV_protect_vtable_vars).
   It attempts to find the binary file on disk for the INFO record
   that dl_iterate_phdr passes in; open the binary file, and read its
   section header information.  If the file contains a
   ".vtable_map_vars" section, read the section offset and size.  Use
   the section offset and size, in conjunction with the data in INFO
   to locate the pages in memory where the section is.  Call
   'mprotect' on those pages, setting the protection either to
   read-only or read-write, depending on what's in DATA.  */

static int
dl_iterate_phdr_callback (struct dl_phdr_info *info, size_t,
                          void *data)
{
  mprotect_data * mdata = (mprotect_data *) data;
  off_t map_sect_offset = 0;
  ElfW(Word) map_sect_len = 0;
  ElfW(Addr) start_addr = 0;
  int j;
  bool found = false;
  char buffer[PATH_MAX];
  char program_name[PATH_MAX];
  char *cptr;
  const ElfW(Phdr) *phdr_info = info->dlpi_phdr;
  const ElfW(Ehdr) *ehdr_info = (const ElfW(Ehdr) *)
    (info->dlpi_addr + info->dlpi_phdr[0].p_vaddr - info->dlpi_phdr[0].p_offset);

  /* Check to see if this is the record for the Linux Virtual Dynamic
     Shared Object (linux-vdso.so.1), which exists only in memory (and
     therefore cannot be read from disk).  */

  if (strcmp (info->dlpi_name, "linux-vdso.so.1") == 0)
    return 0;

  if (strlen (info->dlpi_name) == 0
      && info->dlpi_addr != 0)
    return 0;

  /* Get the name of the main executable.  This may or may not include
     arguments passed to the program.  Find the first space, assume it
     is the start of the argument list, and change it to a '\0'. */
  snprintf (program_name, sizeof (program_name), program_invocation_name);

  /* Find the first non-escaped space in the program name and make it
     the end of the string.  */
  cptr = strchr (program_name, ' ');
  if (cptr != NULL && cptr[-1] != '\\')
    cptr[0] = '\0';

  if (phdr_info->p_type == PT_PHDR
    || phdr_info->p_type == PT_LOAD)
    {
      if (ehdr_info->e_shoff && ehdr_info->e_shnum)
        {
          const char *map_sect_name = ".vtable_map_vars";
          int name_len = strlen (map_sect_name);
          int fd = -1;

          /* Attempt to open the binary file on disk.  */
          if (strlen (info->dlpi_name) == 0)
            {
              /* info->dlpi_name is "" for the main binary, so we need
                 to use program name instead for that.  */
              if (strlen (program_name) > 0)
                {
                  if (phdr_info->p_type == PT_PHDR)
                    fd = open (program_name, O_RDONLY);
                }
              else
                /* If this function is called from something in the
                   preinit array, then program_invocation_name has not
                   been initialized yet, so we have to get the name of
                   the main executable from somewhere else.  The
                   following works on linux systems.  */
                fd = open ("/proc/self/exe", O_RDONLY);
            }
          else
            fd = open (info->dlpi_name, O_RDONLY);

          /* VTV_ASSERT (fd != -1); */
          if (fd != -1)
          {

          /* Find the section header information in the file.  */
          ElfW(Half) strtab_idx = ehdr_info->e_shstrndx;
          ElfW(Shdr) shstrtab;
          off_t shstrtab_offset = ehdr_info->e_shoff +
                                         (ehdr_info->e_shentsize * strtab_idx);
          ssize_t bytes_read = ReadFromOffset (fd, &shstrtab,
                                               sizeof (shstrtab),
                                               shstrtab_offset);

          VTV_ASSERT (bytes_read == sizeof (shstrtab));
          ElfW(Shdr) sect_hdr;

          /* Loop through all the section headers, looking for one
             whose name is ".vtable_map_vars".  */
          for (int i = 0; i < ehdr_info->e_shnum && !found; ++i)
            {
              off_t offset = ehdr_info->e_shoff +
                                                  (ehdr_info->e_shentsize * i);
              bytes_read = ReadFromOffset (fd, &sect_hdr,
                                           sizeof (sect_hdr), offset);
              VTV_ASSERT (bytes_read == sizeof (sect_hdr));

              char header_name[64];
              off_t name_offset = shstrtab.sh_offset + sect_hdr.sh_name;

              bytes_read = ReadFromOffset (fd, &header_name, 64,
                                           name_offset);
              VTV_ASSERT (bytes_read != 0);
              if (memcmp (header_name, map_sect_name, name_len) == 0)
                {
                  /* We found the section; get its load offset and
                     size.  */
                  map_sect_offset = sect_hdr.sh_addr;
                  map_sect_len = sect_hdr.sh_size - mdata->page_size;
                  found = true;
                }
            }
          close (fd);
          } /* if fd != -1 */
        }
      start_addr = (const ElfW(Addr)) info->dlpi_addr + map_sect_offset;
    }

  if (debug_functions)
    {
      snprintf (buffer, sizeof(buffer),
                "  Looking at load module %s to change permissions to %s\n",
                ((strlen (info->dlpi_name) == 0) ? program_name
                                                 : info->dlpi_name),
                (mdata->prot_mode & PROT_WRITE) ? "READ/WRITE"
                                                : "READ-ONLY");
      log_memory_protection_data (buffer);
    }

  /* See if we actually found the section.  */
  if (start_addr && map_sect_len)
    {
      ElfW(Addr) relocated_start_addr = start_addr;
      ElfW(Word) size_in_memory = map_sect_len;

      if ((relocated_start_addr != 0)
          && (size_in_memory != 0))
        {
          /* Calculate the address & size to pass to mprotect. */
          ElfW(Addr) mp_low = relocated_start_addr & ~(mdata->page_size - 1);
          size_t mp_size = size_in_memory - 1;

          /* Change the protections on the pages for the section.  */
          if (mprotect ((void *) mp_low, mp_size, mdata->prot_mode) == -1)
            {
              if (debug_functions)
                {
                  snprintf (buffer, sizeof (buffer),
                            "Failed called to mprotect for %s error: ",
                            (mdata->prot_mode & PROT_WRITE) ?
                            "READ/WRITE" : "READ-ONLY");
                  log_memory_protection_data (buffer);
                  perror(NULL);
                }
              VTV_error();
            }
          else if (debug_functions)
            {
              snprintf (buffer, sizeof (buffer),
                        "mprotect'ed range [%p, %p]\n",
                        (void *) mp_low, (char *) mp_low + mp_size);
              log_memory_protection_data (buffer);
            }
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

#ifndef __GTHREAD_MUTEX_INIT
static void
initialize_change_permissions_mutexes ()
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&change_permissions_lock);
}
#endif

// Variables needed for getting the statistics about the hashtable set
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

static void
log_set_stats()
{
#if HASHTABLE_STATS
  if (set_log_fd == -1)
    set_log_fd = vtv_open_log("vtv_set_stats.log");

  vtv_add_to_log(set_log_fd, "---\n%s\n",
                 insert_only_hash_tables_stats().c_str());
#endif
}

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

      VTV_unprotect_vtable_vars ();
      VTV_malloc_init ();
      VTV_malloc_unprotect ();

    }
  else if (perm == __VLTP_READ_ONLY)
    {
      if (debug_hash)
        log_set_stats();

      VTV_malloc_protect ();
      VTV_protect_vtable_vars ();

      __gthread_mutex_unlock(&change_permissions_lock);

    }
}

struct insert_only_hash_map_allocator {
  void*
  alloc(size_t n) const
  {
    return VTV_malloc(n);
  }

  void
  dealloc(void* p, size_t ) const
  {
    VTV_free(p);
  }
};

// Explicitly instantiate this class since this file is compiled with
// -fno-implicit-templates
template class insert_only_hash_map <vtv_set_handle *, 
                                    insert_only_hash_map_allocator >;
typedef insert_only_hash_map <vtv_set_handle *, insert_only_hash_map_allocator > s2s;
typedef const s2s::key_type  vtv_symbol_key;

static s2s * vtv_symbol_unification_map VTV_PROTECTED_VAR = NULL;

const unsigned long SET_HANDLE_HANDLE_BIT = 0x2;
static inline bool
is_set_handle_handle(void * ptr)
{
  return ((unsigned long)ptr & SET_HANDLE_HANDLE_BIT) == SET_HANDLE_HANDLE_BIT;
}

static inline vtv_set_handle * 
ptr_from_set_handle_handle(void * ptr)
{
  return (vtv_set_handle *)((unsigned long)ptr & ~SET_HANDLE_HANDLE_BIT);
}

static inline vtv_set_handle_handle
set_handle_handle(vtv_set_handle * ptr)
{
  return (vtv_set_handle_handle)((unsigned long)ptr | SET_HANDLE_HANDLE_BIT);
}

// Ideally it would be nice if the library always provided the 2
// versions of the runtime libraries. However, when we use VTV_DEBUG
// we want to make sure that only the debug versions are being
// used. We could change this once the code is more stable
#ifdef VTV_DEBUG

/* This routine initializes a set handle to a vtable set. It makes
sure that there is only one set handle for a particular set by using a
map from set name to pointer to set handle. Since there will be
multiple copies of the pointer to the set handle (one per compilation
unit that uses it), it makes sure to initialize all the pointers to
the set handle so that the set handle is unique. To make this a little
more efficient and avoid a level of indirection in some cases, the
first pointer to handle for a particular handle becomes the handle
itself and the other pointers will point to the set handle.

TODO: Draw a picture here
*/

void __VLTInitSetSymbolDebug(void ** set_handle_ptr, 
                             const void * set_symbol_key, 
                             size_t size_hint)
{
  VTV_DEBUG_ASSERT(set_handle_ptr);

  if (vtv_symbol_unification_map == NULL)
  {
    // TODO: what is the best initial size for this?
    vtv_symbol_unification_map = s2s::create(1024);
    VTV_DEBUG_ASSERT(vtv_symbol_unification_map);
  }

  vtv_set_handle * handle_ptr = (vtv_set_handle *)set_handle_ptr;
  vtv_symbol_key * symbol_key_ptr = (vtv_symbol_key *)set_symbol_key;

  const s2s::value_type * map_value_ptr = 
      vtv_symbol_unification_map->get(symbol_key_ptr);
  char buffer [200];
  if (map_value_ptr == NULL)
  {
    if (*handle_ptr != NULL)
    {
      snprintf(buffer, sizeof(buffer), 
              "*** Found non-NULL local set ptr %p missing for symbol %.*s",
               *handle_ptr, symbol_key_ptr->n, symbol_key_ptr->bytes);
      log_error_message(buffer, true);
      VTV_DEBUG_ASSERT(0);
    }
  }
  else if (*handle_ptr != NULL && 
           (handle_ptr != *map_value_ptr && 
            ptr_from_set_handle_handle(*handle_ptr) != *map_value_ptr))
  {
    VTV_DEBUG_ASSERT(*map_value_ptr != NULL);
    snprintf(buffer, sizeof(buffer), 
             "*** Found diffence between local set ptr %p and set ptr %p"
            "for symbol %.*s", 
             *handle_ptr, *map_value_ptr, 
            symbol_key_ptr->n, symbol_key_ptr->bytes);
    log_error_message(buffer, true);
    VTV_DEBUG_ASSERT(0);
  }
  else if (*handle_ptr == NULL)
  {
    // This case is possible.  The local vtable map variable has not
    // been initialized
  }

  if (*handle_ptr != NULL)
    {
      if (!is_set_handle_handle(*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle(*set_handle_ptr);
      vtv_sets::resize(size_hint, handle_ptr);
      return;
    }

  VTV_DEBUG_ASSERT(*handle_ptr == NULL);
  if (map_value_ptr != NULL)
    {
      if (*map_value_ptr == handle_ptr)
        vtv_sets::resize (size_hint, *map_value_ptr);
      else
        {
          // The one level handle to the set already exists. So, we
          // are adding one level of indirection here and we will
          // store a pointer to the one level handle here.
          vtv_set_handle_handle * handle_handle_ptr = 
              (vtv_set_handle_handle *)handle_ptr;
          *handle_handle_ptr = set_handle_handle(*map_value_ptr);
          VTV_DEBUG_ASSERT(*handle_handle_ptr != NULL);
          // The handle can itself be NULL if the set has only
          // been initiazlied with size hint == 1
          // VTV_DEBUG_ASSERT(
          //     *ptr_from_set_handle_handle(*handle_handle_ptr)
          //     != NULL);
          vtv_sets::resize(size_hint, *map_value_ptr);
        }
    }
  else
    {
      // We will create a new set. So, in this case handle_ptr is
      // the one level pointer to the set handle.

      // Create copy of map name in case the memory where this
      // comes from gets unmapped by dlclose
      size_t map_key_len = symbol_key_ptr->n + sizeof(vtv_symbol_key);
      void * map_key = VTV_malloc(map_key_len);
      memcpy(map_key, symbol_key_ptr, map_key_len);

      s2s::value_type * value_ptr;
      vtv_symbol_unification_map = 
          vtv_symbol_unification_map->find_or_add_key(
              (vtv_symbol_key *)map_key,
              &value_ptr);
      *value_ptr = handle_ptr;

      // TODO: verify return value
      vtv_sets::create(size_hint, handle_ptr);
      VTV_DEBUG_ASSERT(size_hint <= 1 || *handle_ptr != NULL);
    }

  if (debug_init)
  {
    if (init_log_fd == -1)
      init_log_fd = vtv_open_log("vtv_init.log");

    vtv_add_to_log(init_log_fd, 
                   "Init handle:%p for symbol:%.*s hash:%u size_hint:%lu"
                  "number of symbols:%lu \n",
                   set_handle_ptr, symbol_key_ptr->n, symbol_key_ptr->bytes, 
                   symbol_key_ptr->hash, size_hint, 
                  vtv_symbol_unification_map->size());
  }
}


void
__VLTRegisterPairDebug (void ** set_handle_ptr, 
                       const void * vtable_ptr,
                        const char * set_symbol_name, 
                       const char * vtable_name)
{
  VTV_DEBUG_ASSERT(set_handle_ptr != NULL);
  // *set_handle_ptr can be NULL if the call to InitSetSymbol had a
  // *size hint of 1

  int_vptr vtbl_ptr = (int_vptr) vtable_ptr;
  VTV_DEBUG_ASSERT(vtv_symbol_unification_map != NULL);

  vtv_set_handle * handle_ptr;
  if (!is_set_handle_handle(*set_handle_ptr))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle(*set_handle_ptr);

  // TODO: verify return value?
  vtv_sets::insert(vtbl_ptr, handle_ptr);

  if (debug_init)
    {
      if (init_log_fd == -1)
       init_log_fd = vtv_open_log("vtv_init.log");

      vtv_add_to_log(init_log_fd, 
                     "Registered %s : %s (%p) 2 level deref = %s\n",
                    set_symbol_name, vtable_name, vtbl_ptr, 
                     is_set_handle_handle(*set_handle_ptr) ? "yes" : "no" );
    }
}

#ifndef VTV_STATIC_VERIFY

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

#endif // VTV_STATIC_VERIFY

/* This function is called from __VLTVerifyVtablePointerDebug; it sends 
   as much debugging information as it can to the error log file, then calls
   vtv_fail.  */
static void
__vtv_verify_fail_debug (void **set_handle_ptr, const void *vtbl_ptr, 
                         const char *debug_msg)
{
  log_error_message (debug_msg, false);

  // Call the public interface in case it has been overwritten by user.
  __vtv_verify_fail(set_handle_ptr, vtbl_ptr);

  log_error_message ("Returned from __vtv_verify_fail."
                     " Secondary verification succeeded.\n", false);
}

#else 

void __VLTInitSetSymbol(void ** set_handle_ptr, 
                        const void * set_symbol_key,
                        size_t size_hint)
{
  vtv_set_handle * handle_ptr = (vtv_set_handle *)set_handle_ptr;
  if (*handle_ptr != NULL)
    {
      if (!is_set_handle_handle(*set_handle_ptr))
        handle_ptr = (vtv_set_handle *) set_handle_ptr;
      else
        handle_ptr = ptr_from_set_handle_handle(*set_handle_ptr);
      vtv_sets::resize(size_hint, handle_ptr);
      return;
    }

  if (vtv_symbol_unification_map == NULL)
    vtv_symbol_unification_map = s2s::create(1024);

  vtv_symbol_key * symbol_key_ptr = (vtv_symbol_key *)set_symbol_key;
  const s2s::value_type * map_value_ptr = 
      vtv_symbol_unification_map->get(symbol_key_ptr);

  if (map_value_ptr != NULL)
    {
      if (*map_value_ptr == handle_ptr)
       vtv_sets::resize(size_hint, *map_value_ptr);
      else
       {
         // The one level handle to the set already exists. So, we
         // are adding one level of indirection here and we will
         // store a pointer to the one level pointer here.
         vtv_set_handle_handle * handle_handle_ptr = 
           (vtv_set_handle_handle *)handle_ptr;
         *handle_handle_ptr = set_handle_handle(*map_value_ptr);
         vtv_sets::resize(size_hint, *map_value_ptr);
       }
    }
  else
    {
      // We will create a new set. So, in this case handle_ptr is
      // the one level pointer to the set handle.

      // Create copy of map name in case the memory where this
      // comes from gets unmapped by dlclose
      size_t map_key_len = symbol_key_ptr->n + sizeof(vtv_symbol_key);
      void * map_key = VTV_malloc(map_key_len);
      memcpy(map_key, symbol_key_ptr, map_key_len);

      s2s::value_type * value_ptr;
      vtv_symbol_unification_map = 
       vtv_symbol_unification_map->find_or_add_key(
            (vtv_symbol_key *)map_key, &value_ptr);
      *value_ptr = handle_ptr;

      // TODO: verify return value
      vtv_sets::create(size_hint, handle_ptr);
    }
}

void 
__VLTRegisterPair (void **set_handle_ptr, const void *vtable_ptr)
{
  int_vptr vtbl_ptr = (int_vptr) vtable_ptr;

  vtv_set_handle * handle_ptr;
  if (!is_set_handle_handle(*set_handle_ptr))
    handle_ptr = (vtv_set_handle *) set_handle_ptr;
  else
    handle_ptr = ptr_from_set_handle_handle(*set_handle_ptr);

  // TODO: verify return value?
  vtv_sets::insert(vtbl_ptr, handle_ptr);
}

#ifndef VTV_STATIC_VERIFY

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
#endif // VTV_EMPTY_VERIFY

  return vtable_ptr;
}

#endif // VTV_STATIC_VERIFY

#endif // VTV_DEBUG

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
vtv_fail (const char *msg, void **data_set_ptr, const void *vtbl_ptr)
{
  char buffer[128];
  int buf_len;
  const char *format_str =
      "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";

  snprintf (buffer, sizeof (buffer), format_str, vtbl_ptr,
            is_set_handle_handle(*data_set_ptr) ?
              ptr_from_set_handle_handle(*data_set_ptr) :
              *data_set_ptr);
  buf_len = strlen (buffer);
  // Send to stderr
  write (2, buffer, buf_len);

#ifndef VTV_NO_ABORT
  __vtv_really_fail (msg);
#endif
}


/* Open error logging file, if not already open, and write vtable verification
   failure messages to the log file.  */
static void
log_error_message (const char *log_msg, bool generate_backtrace)
{
  if (vtv_failures_log_fd == -1)
    vtv_failures_log_fd = vtv_open_log ("vtable_verification_failures.log");

  if (vtv_failures_log_fd == -1)
    return;

  vtv_add_to_log (vtv_failures_log_fd, "%s", log_msg);

  if (generate_backtrace)
    {
#define STACK_DEPTH 20
      void *callers[STACK_DEPTH];
      int actual_depth = backtrace (callers, STACK_DEPTH);
      backtrace_symbols_fd (callers, actual_depth, vtv_failures_log_fd);
    }
}


/* Send information about what we were trying to do when verification failed to
   the error log, then call vtv_fail.  This function can be overwritten/replaced
   by the user, to implement a secondary verification function instead.*/
void
__vtv_verify_fail (void **data_set_ptr, const void *vtbl_ptr)
{
  char log_msg[256];
  snprintf (log_msg, sizeof (log_msg), "Looking for vtable %p in set %p.\n",
            vtbl_ptr,
            is_set_handle_handle(*data_set_ptr) ?
              ptr_from_set_handle_handle(*data_set_ptr) :
              *data_set_ptr);
  log_error_message (log_msg, false);

  const char *format_str =
            "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";
  snprintf (log_msg, sizeof (log_msg), format_str, vtbl_ptr, *data_set_ptr);
  log_error_message (log_msg, false);
  log_error_message ("  Backtrace: \n", true);

  const char *fail_msg = "Potential vtable pointer corruption detected!!\n";
  vtv_fail (fail_msg, data_set_ptr, vtbl_ptr);
}
