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

/* The following variables are used only for debugging and performance
   tuning purposes. Therefore they do not need to be "protected".
   They cannot be used to attack the vtable verification system and if
   they become corrupted it will not affect the correctness or
   security of any of the rest of the vtable verification feature.  */

static const bool debug_hash = HASHTABLE_STATS;
/* TODO: Make sure debugging messages under this guard dont use malloc!  */
static const int debug_functions = 0;

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

#ifdef __GTHREAD_MUTEX_INIT
/* TODO: NEED TO PROTECT THIS VAR  !!!!!!!!!!!!!!!!!!!  */
static __gthread_mutex_t change_permissions_lock = __GTHREAD_MUTEX_INIT;
#else
/* TODO: NEED TO PROTECT THIS VAR  !!!!!!!!!!!!!!!!!!!  */
static __gthread_mutex_t change_permissions_lock;
#endif


/* Records for caching the section header information that we have
   read out of the file(s) on disk (in dl_iterate_phdr_callback), to
   avoid having to re-open and re-read the same file multiple
   times.  */

struct sect_hdr_data
{
  ElfW (Addr) dlpi_addr; /* The header address in the INFO record,
                            passed in from dl_iterate_phdr.  */
  ElfW (Addr) mp_low;    /* Start address of the .vtable_map_vars
                            section in memory.  */
  size_t mp_size;        /* Size of the .vtable_map_vars section in
                            memory.  */
};

/* Array for caching the section header information, read from file,
   to avoid re-opening and re-reading the same file over-and-over
   again.  */

#define MAX_ENTRIES 250
// TODO: Should these 2 be HIDDEN?
struct sect_hdr_data sect_info_cache[MAX_ENTRIES] VTV_PROTECTED_GLOBAL_VAR;

unsigned int num_cache_entries VTV_PROTECTED_GLOBAL_VAR = 0;

/* This function takes the LOAD_ADDR for an object opened by the
   dynamic loader, and checks the array of cached file data to see if
   there is an entry with the same addres.  If it finds such an entry,
   it returns the record for that entry; otherwise it returns
   NULL.  */

static struct sect_hdr_data *
search_cached_file_data (ElfW (Addr) load_addr)
{
  unsigned int i;
  for (i = 0; i < num_cache_entries; ++i)
    {
      if (sect_info_cache[i].dlpi_addr == load_addr)
        return &(sect_info_cache[i]);
    }

  return NULL;
}

/* This function tries to read COUNT bytes out of the file referred to
   by FD into the buffer BUF.  It returns the actual number of bytes
   it succeeded in reading.  */

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

/* This function tries to read COUNT bytes, starting at OFFSET from
   the file referred to by FD, and put them into BUF.  It calls
   ReadPersistent to help it do so.  It returns the actual number of
   bytes read, or -1 if it fails altogether.  */

static ssize_t
ReadFromOffset (int fd, void *buf, const size_t count, const off_t offset)
{
  off_t off = lseek (fd, offset, SEEK_SET);
  if (off != (off_t) -1)
    return ReadPersistent (fd, buf, count);
  return -1;
}

/* The function takes a MESSAGE and attempts to write it to the vtable
   memory protection log (for debugging purposes).  If the file is not
   open, it attempts to open the file first.  Sometimes multiple
   processes may be attempting to write to the log file at the same
   time, so we may attempt to open multiple log files (with versioned
   names) if the first open fails.  */

static void
log_memory_protection_data (char *message)
{
  static int log_fd = -1;

  if (log_fd == -1)
    log_fd = __vtv_open_log ("vtv_memory_protection_data.log");

  __vtv_add_to_log (log_fd, "%s", message);
}


static void
read_section_offset_and_length (struct dl_phdr_info *info,
                                const char *sect_name,
                                int mprotect_flags,
                                off_t *sect_offset,
                                ElfW (Word) *sect_len)
{
  char program_name[PATH_MAX];
  char *cptr;
  bool found = false;
  struct sect_hdr_data *cached_data = NULL;
  const ElfW (Phdr) *phdr_info = info->dlpi_phdr;
  const ElfW (Ehdr) *ehdr_info =
    (const ElfW (Ehdr) *) (info->dlpi_addr + info->dlpi_phdr[0].p_vaddr
                           - info->dlpi_phdr[0].p_offset);


  /* Get the name of the main executable.  This may or may not include
     arguments passed to the program.  Find the first space, assume it
     is the start of the argument list, and change it to a '\0'. */
  snprintf (program_name, sizeof (program_name), program_invocation_name);

  /* Check to see if we already have the data for this file.  */
  cached_data = search_cached_file_data (info->dlpi_addr);

  if (cached_data)
    {
      *sect_offset = cached_data->mp_low;
      *sect_len = cached_data->mp_size;
      return;
    }

  /* Find the first non-escaped space in the program name and make it
     the end of the string.  */
  cptr = strchr (program_name, ' ');
  if (cptr != NULL && cptr[-1] != '\\')
    cptr[0] = '\0';

  if ((phdr_info->p_type == PT_PHDR || phdr_info->p_type == PT_LOAD)
      && (ehdr_info->e_shoff && ehdr_info->e_shnum))
    {
      int name_len = strlen (sect_name);
      int fd = -1;

      /* Attempt to open the binary file on disk.  */
      if (strlen (info->dlpi_name) == 0)
        {
          /* If the constructor initialization function was put into
             the preinit array, then this function will get called
             while handling preinit array stuff, in which case
             program_invocation_name has not been initialized.  In
             that case we can get the filename of the executable from
             "/proc/self/exe".  */
          if (strlen (program_name) > 0)
            {
              if (phdr_info->p_type == PT_PHDR)
                fd = open (program_name, O_RDONLY);
            }
          else
            fd = open ("/proc/self/exe", O_RDONLY);
        }
      else
        fd = open (info->dlpi_name, O_RDONLY);

      if (fd != -1)
        {

          /* Find the section header information in the file.  */
          ElfW (Half) strtab_idx = ehdr_info->e_shstrndx;
          ElfW (Shdr) shstrtab;
          off_t shstrtab_offset = ehdr_info->e_shoff +
                                         (ehdr_info->e_shentsize * strtab_idx);
          size_t bytes_read = ReadFromOffset (fd, &shstrtab, sizeof (shstrtab),
                                              shstrtab_offset);
          VTV_ASSERT (bytes_read == sizeof (shstrtab));

          ElfW (Shdr) sect_hdr;

          /* Loop through all the section headers, looking for one whose
             name is sect_name */

          for (int i = 0; i < ehdr_info->e_shnum && !found; ++i)
            {
              off_t offset = ehdr_info->e_shoff + (ehdr_info->e_shentsize * i);

              bytes_read = ReadFromOffset (fd, &sect_hdr, sizeof (sect_hdr),
                                           offset);

              VTV_ASSERT (bytes_read == sizeof (sect_hdr));

              char header_name[64];
              off_t name_offset = shstrtab.sh_offset +  sect_hdr.sh_name;

              bytes_read = ReadFromOffset (fd, &header_name, 64, name_offset);

              VTV_ASSERT (bytes_read > 0);

              if (memcmp (header_name, sect_name, name_len) == 0)
                {
                  /* We found the section; get its load offset and
                     size.  */
                  *sect_offset = sect_hdr.sh_addr;
                  *sect_len = sect_hdr.sh_size - VTV_PAGE_SIZE;
                  found = true;
                }
            }
          close (fd);
        }
    }

  if (*sect_offset != 0 && *sect_len != 0)
    {
      /* Calculate the page location in memory, making sure the
         address is page-aligned.  */
      ElfW (Addr) start_addr = (const ElfW (Addr)) info->dlpi_addr
                                                                 + *sect_offset;
      *sect_offset = start_addr & ~(VTV_PAGE_SIZE - 1);
      *sect_len = *sect_len - 1;

      /* Since we got this far, we must not have found these pages in
         the cache, so add them to it.  NOTE: We could get here either
         while making everything read-only or while making everything
         read-write.  We will only update the cache if we get here on
         a read-write (to make absolutely sure the cache is writable
         -- also the read-write pass should come before the read-only
         pass).  */
      if ((mprotect_flags & PROT_WRITE)
          && num_cache_entries < MAX_ENTRIES)
        {
          sect_info_cache[num_cache_entries].dlpi_addr = info->dlpi_addr;
          sect_info_cache[num_cache_entries].mp_low = *sect_offset;
          sect_info_cache[num_cache_entries].mp_size = *sect_len;
          num_cache_entries++;
        }
    }
}

/* This is the callback function used by dl_iterate_phdr (which is
   called from VTV_unprotect_vtable_vars and VTV_protect_vtable_vars).
   It attempts to find the binary file on disk for the INFO record
   that dl_iterate_phdr passes in; open the binary file, and read its
   section header information.  If the file contains a
   ".vtable_map_vars" section, read the section offset and size.  Use
   the secttion offset and size, in conjunction with the data in INFO
   to locate the pages in memory where the section is.  Call
   'mprotect' on those pages, setting the protection either to
   read-only or read-write, depending on what's in DATA.  */

static int
dl_iterate_phdr_callback (struct dl_phdr_info *info,
                          size_t unused __attribute__((__unused__)),
                          void *data)
{
  int *mprotect_flags = (int *) data;
  off_t map_sect_offset = 0;
  ElfW (Word) map_sect_len = 0;
  char buffer[PATH_MAX];
  char program_name[PATH_MAX];
  const char *map_sect_name = VTV_PROTECTED_VARS_SECTION;

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

  read_section_offset_and_length (info, map_sect_name, *mprotect_flags,
                                  &map_sect_offset, &map_sect_len);

  if (debug_functions)
    {
      snprintf (buffer, sizeof(buffer),
                "  Looking at load module %s to change permissions to %s\n",
                ((strlen (info->dlpi_name) == 0) ? program_name
                                                 : info->dlpi_name),
                (*mprotect_flags & PROT_WRITE) ? "READ/WRITE" : "READ-ONLY");
      log_memory_protection_data (buffer);
    }

  /* See if we actually found the section.  */
  if (map_sect_offset && map_sect_len)
    {
      unsigned long long start;
      int result;

      if (debug_functions)
        {
          snprintf (buffer, sizeof (buffer),
                    "  (%s): Protecting %p to %p\n",
                    ((strlen (info->dlpi_name) == 0) ? program_name
                     : info->dlpi_name),
                    (void *) map_sect_offset,
                    (void *) (map_sect_offset + map_sect_len));
          log_memory_protection_data (buffer);
        }

      /* Change the protections on the pages for the section.  */

      start = get_cycle_count ();
      result = mprotect ((void *) map_sect_offset, map_sect_len,
                         *mprotect_flags);
      accumulate_cycle_count (&__vtv_stats.mprotect_cycles, start);
      if (result == -1)
        {
          if (debug_functions)
            {
              snprintf (buffer, sizeof (buffer),
                        "Failed called to mprotect for %s error: ",
                        (*mprotect_flags & PROT_WRITE) ?
                        "READ/WRITE" : "READ-ONLY");
              log_memory_protection_data (buffer);
            }
          VTV_error();
        }
      else
        {
          if (debug_functions)
           {
              snprintf (buffer, sizeof (buffer),
                        "mprotect'ed range [%p, %p]\n",
                        (void *) map_sect_offset,
                        (char *) map_sect_offset + map_sect_len);
              log_memory_protection_data (buffer);
            }
        }
      increment_num_calls (&__vtv_stats.num_calls_to_mprotect);
      __vtv_stats.num_pages_protected +=
          (map_sect_len + VTV_PAGE_SIZE - 1) / VTV_PAGE_SIZE;
    }

  return 0;
}

/* This function explicitly changes the protection (read-only or read-write)
   on the sect_info_cache, which is used for speeding up look ups in the
   function dl_iterate_phdr_callback.  This data structure needs to be
   explicitly made read-write before any calls  to dl_iterate_phdr_callback,
   because otherwise it may still be read-only when dl_iterate_phdr_callback
   attempts to write to it.

   More detailed explanation:  dl_iterate_phdr_callback finds all the
   .vtable_map_vars sections in all loaded objects (including the main program)
   and (depending on where it was called from) either makes all the pages in the
   sections read-write or read-only.  The sect_info_cache should be in the
   .vtable_map_vars section for libstdc++.so, which means that normally it would
   be read-only until libstdc++.so is processed by dl_iterate_phdr_callback
   (on the read-write pass), after which it will be writable.  But if any loaded
   object gets processed before libstdc++.so, it will attempt to update the
   data cache, which will still be read-only, and cause a seg fault.  Hence
   we need a special function, called before dl_iterate_phdr_callback, that
   will make the data cache writable.  */

static void
change_protections_on_phdr_cache (int protection_flag)
{
  char * low_address = (char *)&sect_info_cache;
  size_t cache_size = MAX_ENTRIES * sizeof (struct sect_hdr_data);

  low_address = (char *)((unsigned long)low_address & ~(VTV_PAGE_SIZE -1));
  size_t protection_size = (char *)&sect_info_cache - low_address + cache_size;

  if (mprotect ((void *) low_address, protection_size, protection_flag) == -1)
    VTV_error ();
}

/* Unprotect all the vtable map vars and other side data that is used
   to keep the core hash_map data. All of these data have been put
   into relro sections */

static void
VTV_unprotect_vtable_vars (void)
{
#ifndef VTV_NO_MPROTECT
  int mprotect_flags;

  mprotect_flags = PROT_READ | PROT_WRITE;
  change_protections_on_phdr_cache (mprotect_flags);
  dl_iterate_phdr (dl_iterate_phdr_callback, (void *) &mprotect_flags);
#endif
}

/* Protect all the vtable map vars and other side data that is used
   to keep the core hash_map data. All of these data have been put
   into relro sections */

static void
VTV_protect_vtable_vars (void)
{
#ifndef VTV_NO_MPROTECT
  int mprotect_flags;

  mprotect_flags = PROT_READ;
  dl_iterate_phdr (dl_iterate_phdr_callback, (void *) &mprotect_flags);
  change_protections_on_phdr_cache (mprotect_flags);
#endif
}

#ifndef __GTHREAD_MUTEX_INIT
static void
initialize_change_permissions_mutexes ()
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&change_permissions_lock);
}
#endif

/* Record statistics about the hash table sets, for debugging.  */

static void
log_set_stats (void)
{
#if HASHTABLE_STATS
      if (set_log_fd == -1)
        set_log_fd = __vtv_open_log ("vtv_set_stats.log");

      __vtv_add_to_log (set_log_fd, "---\n%s\n",
                        insert_only_hash_tables_stats().c_str());
#endif
}

/* Change the permissions on all the pages we have allocated for the
   data sets and all the ".vtable_map_var" sections in memory (which
   contain our vtable map variables).  PERM indicates whether to make
   the permissions read-only or read-write.  */

extern "C" /* This is only being applied to __VLTChangePermission.  */
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
  __gthread_once_t mutex_once VTV_PROTECTED_HIDDEN_VAR = __GTHREAD_ONCE_INIT;

  __gthread_once (&mutex_once, initialize_change_permissions_mutexes);
#endif

  /* Ordering of these unprotect/protect calls is very important.
     You first need to unprotect all the map vars and side
     structures before you do anything with the core data
     structures (hash_maps) */

  if (perm == __VLTP_READ_WRITE)
    {
      __gthread_mutex_lock (&change_permissions_lock);

      VTV_unprotect_vtable_vars ();
      __vtv_malloc_init ();
      __vtv_malloc_unprotect ();

    }
  else if (perm == __VLTP_READ_ONLY)
    {
      if (debug_hash)
        log_set_stats();

      __vtv_malloc_protect ();
      VTV_protect_vtable_vars ();

      __gthread_mutex_unlock (&change_permissions_lock);
    }
}

static int page_count_2 = 0;

static int
dl_iterate_phdr_count_pages (struct dl_phdr_info *info,
                             size_t unused __attribute__ ((__unused__)),
                             void *data)
{
  int *mprotect_flags = (int *) data;
  off_t map_sect_offset = 0;
  ElfW (Word) map_sect_len = 0;
  const char *map_sect_name = VTV_PROTECTED_VARS_SECTION;

  /* Check to see if this is the record for the Linux Virtual Dynamic
     Shared Object (linux-vdso.so.1), which exists only in memory (and
     therefore cannot be read from disk).  */

  if (strcmp (info->dlpi_name, "linux-vdso.so.1") == 0)
    return 0;

  if (strlen (info->dlpi_name) == 0
      && info->dlpi_addr != 0)
    return 0;

  read_section_offset_and_length (info, map_sect_name, *mprotect_flags,
                                 &map_sect_offset, &map_sect_len);

  /* See if we actually found the section.  */
  if (map_sect_len)
    page_count_2 += (map_sect_len + VTV_PAGE_SIZE - 1) / VTV_PAGE_SIZE;

  return 0;
}

static void
count_all_pages (void)
{
  int mprotect_flags;

  mprotect_flags = PROT_READ;
  page_count_2 = 0;

  dl_iterate_phdr (dl_iterate_phdr_count_pages, (void *) &mprotect_flags);
  page_count_2 += VTV_count_mmapped_pages ();
}

void
__VLTDumpStats (void)
{
  int log_fd = __vtv_open_log ("vtv-runtime-stats.log");

  if (log_fd != -1)
    {
      count_all_pages ();
      __vtv_add_to_log (log_fd,
                        "Calls: mprotect (%d)  regset (%d) regpair (%d)"
                        " verify_vtable (%d)\n",
                        __vtv_stats.num_calls_to_mprotect,
                        __vtv_stats.num_calls_to_regset,
                        __vtv_stats.num_calls_to_regpair,
                        __vtv_stats.num_calls_to_verify_vtable);
      __vtv_add_to_log (log_fd,
                        "Cycles: mprotect (%lld) regset (%lld) "
                        "regpair (%lld) verify_vtable (%lld)\n",
                        __vtv_stats.mprotect_cycles,
                        __vtv_stats.regset_cycles,
                        __vtv_stats.regpair_cycles,
                        __vtv_stats.verify_vtable_cycles);
      __vtv_add_to_log (log_fd,
                        "Pages protected (1): %d\n",
                        __vtv_stats.num_pages_protected);
      __vtv_add_to_log (log_fd, "Pages protected (2): %d\n", page_count_2);

      close (log_fd);
    }
}
