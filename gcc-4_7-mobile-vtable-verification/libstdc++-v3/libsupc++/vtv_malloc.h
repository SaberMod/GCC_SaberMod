/* Copyright (C) 2012, 2013
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

#ifndef _VTV_MALLOC_H
#define _VTV_MALLOC_H 1

#include <stdlib.h>

/* Alignment mask for any object returned by the VTV memory pool */
#ifdef __LP64__
#define VTV_ALIGNMENT_MASK (0x7)
#else
#define VTV_ALIGNMENT_MASK (0x3)
#endif

#ifdef __x86_64__
static inline unsigned long
rdtsc ()
{
  unsigned long long hi, lo;

  asm volatile ("rdtsc" : "=a" (lo), "=d" (hi));
  return hi << 32 | lo;
}
#elif defined (__i386__)
static inline unsigned long long
rdtsc ()
{
  unsigned long long var;

  asm volatile ("rdtsc" : "=A" (var));

  return var;
}
#else 
static inline unsigned long long
rdtsc ()
{
  /* Create an empty function for unknown architectures, so that the
     calls to this function in vtv_malloc.c and vtv_rts.c do not cause
     compilation errors.  */
  return ((unsigned long long) 0);
}
#endif


/* The following variables are used only for debugging and performance tuning
   purposes. Therefore they do not need to be "protected".  They cannot be used
   to attack the vtable verification system and if they become corrupted it will
   not affect the correctness or security of any of the rest of the vtable
   verification feature.  */

extern unsigned int num_calls_to_mprotect;
extern unsigned int num_pages_protected;
extern unsigned int num_calls_to_register_pair;
extern unsigned int num_calls_to_init_set;
extern unsigned long long mprotect_cycles;
extern unsigned long long register_pair_cycles;
extern unsigned long long init_set_cycles;


/* Function declarations.  */

extern void VTV_malloc_init (void);
extern void *VTV_malloc (size_t size);
extern void VTV_free (void * ptr);
extern void VTV_malloc_protect (void);
extern void VTV_malloc_unprotect (void);
extern void VTV_malloc_stats (void);
extern void VTV_malloc_dump_stats (void);
extern int  VTV_count_mmapped_pages (void);

#endif /* vtv_malloc.h */
