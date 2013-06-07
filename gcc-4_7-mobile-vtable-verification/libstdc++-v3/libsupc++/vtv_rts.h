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

#ifndef _VTV_RTS_H
#define _VTV_RTS_H 1

#include <cstdlib>

#ifdef VTV_STATIC_VERIFY
#define VTV_VISIBILITY_ATTR "internal"
#else
#define VTV_VISIBILITY_ATTR "default"
#endif

// Not using the "hot" attribute by default because it seems to cause
// some conflict with the function ordering file used at link
// time. Specifying an oder for these routines in the ordering file
// does not seem to work if marked with "hot"
#ifdef VTV_USE_HOT_ATTR
#define VTV_HOT_ATTR hot
#else
#define VTV_HOT_ATTR
#endif

/* These prototypes need to be kept in sync with the compiler-
   generated declarations in vtable-class-hierarchy.c.  */

#ifdef VTV_DEBUG

extern void __vtv_register_set_debug (void **, const void *, std::size_t,
                                      std::size_t, void **)
    __attribute__ ((visibility (VTV_VISIBILITY_ATTR), VTV_HOT_ATTR, leaf));

extern void __vtv_register_pair_debug (void **, const void *, size_t,
                                       const void *, const char *,
                                       const char *)
    __attribute__ ((visibility (VTV_VISIBILITY_ATTR), VTV_HOT_ATTR, leaf));

extern const void *__vtv_verify_vtable_pointer_debug (void **, const void *,
						      const char *,
                                                      const char *)
    __attribute__ ((visibility (VTV_VISIBILITY_ATTR), VTV_HOT_ATTR, leaf,
		    const));

#else

extern void __vtv_register_set (void **, const void *, std::size_t, std::size_t,
                                void **)
    __attribute__ ((visibility (VTV_VISIBILITY_ATTR), VTV_HOT_ATTR, leaf));

extern void __vtv_register_pair (void **, const void *, size_t, const void *)
    __attribute__ ((visibility (VTV_VISIBILITY_ATTR), VTV_HOT_ATTR, leaf));

/* This is the most performance critical routine. We use every attribute
   we can use to improve its performance
   - aligned(64). This function body is pretty small we want it to use the
     smallest number of cache lines
   - "const". this function return value only depends on the value of
     incoming arguments and the value of global memory but this global
     memory is "constant". So we can say that the return value only
     logically depends on the incoming arguments
*/
extern const void *__vtv_verify_vtable_pointer (void **, const void *)
    __attribute__ ((visibility (VTV_VISIBILITY_ATTR), VTV_HOT_ATTR, leaf,
		    const, aligned(64)));

#endif

#endif /* _VTV_RTS_H */
