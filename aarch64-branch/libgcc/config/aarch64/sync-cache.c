/* Machine description for AArch64 architecture.
   Copyright (C) 2012 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

void
__aarch64_sync_cache_range (const void *base, const void *end)
{
  unsigned int cache_info = 0;

  /* CTR_EL0 is the same as AArch32's CTR which contains log2 of the
     icache size in [3:0], and log2 of the dcache line in [19:16].  */
  asm volatile ("mrs\t%0, ctr_el0":"=r" (cache_info));

  unsigned int icache_lsize = 1 << (cache_info & 0xF);
  unsigned int dcache_lsize = 1 << ((cache_info >> 16) & 0xF);

  /* Loop over the address range, clearing one cache line at once.
     Data cache must be flushed to unification first to make sure the
     instruction cache fetches the updated data.  'end' is exclusive,
     as per the GNU definition of __clear_cache.  */

  const char *address;
  for (address = base; address < (const char *) end; address += dcache_lsize)
    asm volatile ("dc\tcvau, %0"
		  :
		  : "r" (address)
		  : "memory");

  asm volatile ("dsb\tish" : : : "memory");

  for (address = base; address < (const char *) end; address += icache_lsize)
    asm volatile ("ic\tivau, %0"
		  :
		  : "r" (address)
		  : "memory");

  asm volatile ("dsb\tish; isb" : : : "memory");
}
