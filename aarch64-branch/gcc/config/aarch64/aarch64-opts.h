/* Copyright (C) 2011, 2012 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Definitions for option handling for AArch64.  */

#ifndef GCC_AARCH64_OPTS_H
#define GCC_AARCH64_OPTS_H

/* The various cores that implement AArch64.  */
enum aarch64_processor
{
#define AARCH64_CORE(NAME, IDENT, ARCH, FLAGS, COSTS) \
  IDENT,
#include "aarch64-cores.def"
#undef AARCH64_CORE
  /* Used to indicate that no processor has been specified.  */
  generic,
  /* Used to mark the end of the processor table.  */
  aarch64_none
};

/* TLS types.  */
enum aarch64_tls_type {
  TLS_TRADITIONAL,
  TLS_DESCRIPTORS
};

#endif
