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

#ifndef _VTV_RTS_H
#define _VTV_RTS_H 1

/* This prototype needs to be kept in synch with the compiler generated
   declaration in vtable-class-hierarchy.c */

/* could have used an enumeration here but it just makes it more difficult
   for the compiler to generated a call to this */
#define __VLTP_READ_ONLY  0
#define __VLTP_READ_WRITE 1

void
__VLTChangePermission (int);

void *
__VLTRegisterPair (void **, void *, int, char *, int, char *, int);

void *
__VLTVerifyVtablePointer (void **, void *);

#endif /* _VTV_RTS_H */
