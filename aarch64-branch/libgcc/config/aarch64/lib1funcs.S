/* libgcc routines for AArch64

   Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* We need to know what prefix to add to function names.  */

#ifndef __USER_LABEL_PREFIX__
#error  __USER_LABEL_PREFIX__ not defined
#endif

/* ANSI concatenation macros.  */

#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

/* Use the right prefix for global labels.  */

#define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

#ifdef __ELF__
#define TYPE(x) .type SYM(x),function
#define SIZE(x) .size SYM(x), . - SYM(x)
#define LSYM(x) .x
#else
#define TYPE(x)
#define SIZE(x)
#define LSYM(x) x
#endif

.macro FUNC_START name
	.text
	.globl SYM (__\name)
	TYPE (__\name)
	.align 2
SYM (__\name):
.endm

.macro FUNC_END name
	SIZE (__\name)
.endm

#ifdef L_aarch64_sync_cache_range
#if defined __linux__
	FUNC_START aarch64_sync_cache_range
	mov	x3, 0
	mov	x8, 0x1002
	svc	0
	RET
	FUNC_END aarch64_sync_cache_range
#else
#error "This is only for AARCH64 GNU/Linux"
#endif
#endif