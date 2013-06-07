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


/* Function declarations.  */

extern void VTV_malloc_init (void);
extern void * __vtv_malloc (size_t size) __attribute__ ((malloc));
/* Do nothing. We dont care about recovering unneded memory */
static inline void __vtv_free (void *) {};
extern void VTV_malloc_protect (void);
extern void VTV_malloc_unprotect (void);

extern void VTV_malloc_stats (void);
extern void VTV_malloc_dump_stats (void);
extern int  VTV_count_mmapped_pages (void);

#endif /* vtv_malloc.h */
