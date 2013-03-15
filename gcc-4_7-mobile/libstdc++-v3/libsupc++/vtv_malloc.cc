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

/* This file is part of the vtable verification runtime library.  It
   contains our memory allocation and deallocation routines, which we
   use in order to keep track of the pages in memory in which our sets
   of valid vtable pointes are stored.  (We need to know the pages so
   we can set the protections on them appropriately).  For more
   information about the vtable verification feature, see the comments
   in vtv_rts.cc.  We use the existing obstack implementation in our
   memory allocation scheme.  */

#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>

#include "vtv_utils.h"
#include "vtv_malloc.h"
#include "obstack.h"

/* Put the following variables in our ".vtable_map_vars" section so
   that they are protected.  They are explicitly unprotected and
   protected again by calls to VTV_unprotect and VTV_protect */

static struct obstack VTV_obstack VTV_PROTECTED_VAR;
static unsigned long page_size VTV_PROTECTED_VAR = 0;
static void *current_chunk VTV_PROTECTED_VAR = 0;
static size_t current_chunk_size VTV_PROTECTED_VAR = 0;
static int malloc_initialized VTV_PROTECTED_VAR = 0;

/* This function goes through all of the pages we have allocated so
   far and calls mprotect to make each page read-only.  */

void
VTV_malloc_protect (void)
{
  struct _obstack_chunk *ci;
  ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
    {
      VTV_DEBUG_ASSERT (((unsigned long) ci & (page_size - 1)) == 0);
      if (mprotect (ci, (ci->limit - (char *) ci), PROT_READ) == -1)
	VTV_error ();
      ci = ci->prev;
    }

#if (VTV_DEBUG_MALLOC == 1)
    VTV_malloc_dump_stats ();
#endif
}

/* This function goes through all of the pages we have allocated so
   far and calls mrpotect to make each page read-write.  */

void
VTV_malloc_unprotect (void)
{
  struct _obstack_chunk * ci;
  ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
    {
      VTV_DEBUG_ASSERT (((unsigned long) ci & (page_size - 1)) == 0);
      if (mprotect (ci, (ci->limit - (char *) ci), PROT_READ | PROT_WRITE)
                                                                         == -1)
	VTV_error ();
      ci = ci->prev;
    }
}

/* Allocates a SIZE-sized chunk of memory that is aligned to a page
   boundary.  The amount of memory requested (SIZE) must be a multiple
   of the page size.  Note: We must use mmap to allocate the memory;
   using malloc here will cause problems.  */

static void *
obstack_chunk_alloc (size_t size)
{
  /* Increase size to the next multiple of page_size.   */
  size = (size + (page_size - 1)) & (~(page_size - 1));
  VTV_DEBUG_ASSERT ((size & (page_size - 1)) == 0);
  void *allocated;

  if ((allocated = mmap (NULL, size, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS,  -1, 0)) == 0)
    VTV_error ();

  VTV_DEBUG_ASSERT (((unsigned long) allocated & (page_size - 1)) == 0);

  current_chunk = allocated;
  current_chunk_size = size;
  return allocated;
}

static void
obstack_chunk_free (size_t size)
{
  /* Do nothing. For our purposes there should be very little
     de-allocation. */
}

/* This function sets up and initializes the obstack pieces for our
   memory allocation scheme.  */

void
VTV_malloc_init (void)
{
  /* Make sure we only execute the main body of this function ONCE.  */
  if (malloc_initialized)
    return;

  page_size = sysconf (_SC_PAGE_SIZE);
  if (page_size != 4096)
    VTV_error ();

  obstack_chunk_size (&VTV_obstack) = page_size;
  obstack_alignment_mask (&VTV_obstack) = sizeof (long) - 1;
  /* We guarantee that the obstack alloc failed handler will never be
     called because in case the allocation of the chunk fails, it will
     never return */
  obstack_alloc_failed_handler = NULL;

  obstack_init (&VTV_obstack);
  malloc_initialized = 1;
}

/* This is our external interface for the memory allocation.  SIZE is
   the requested number of bytes to be allocated/  */

void *
VTV_malloc (size_t size)
{
  return obstack_alloc (&VTV_obstack, size);
}


/* This is our external interface for memory deallocation.  */

void
VTV_free (void *)
{
  /* Do nothing. We dont care about recovering unneded memory at this
     time.  */
}


/* This is a debugging function tat collects statistics about our
   memory allocation.  */
void
VTV_malloc_stats (void)
{
  int count = 0;
  struct _obstack_chunk * ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
    {
      count++;
      ci = ci->prev;
    }
  fprintf (stderr,
	   "VTV_malloc_stats:\n  Page Size = %lu bytes\n  "
	   "Number of pages = %d\n", page_size, count);
}

/* This is a debugging function.  It writes out our memory allocation
   statistics to a log file.  */

void
VTV_malloc_dump_stats (void)
{
  static int fd = -1;

  if (fd == -1)
    fd = vtv_open_log ("/tmp/vtv_mem_protection.log");
  if (fd == -1)
    return;

  int count = 0;
  struct _obstack_chunk * ci = (struct _obstack_chunk *) current_chunk;
  while (ci)
    {
      count++;
      ci = ci->prev;
    }

  vtv_add_to_log (fd, "VTV_malloc_protect protected=%d pages\n", count);
}
