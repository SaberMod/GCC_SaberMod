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

#ifndef _VTV_THREADED_HASH_H
#define _VTV_THREADED_HASH_H 1

#include <bits/c++config.h>
#include <ext/concurrence.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

struct vlt_hash_bucket {
  void *data;
  struct vlt_hash_bucket *next;
};

struct vlt_hashtable {
  volatile uint32_t data_size;
  uint32_t num_elts;
  uint32_t power_of_2;
  __gthread_mutex_t mutex;
  struct vlt_hash_bucket ** volatile data;
};

/* Main hash table interface */

extern struct vlt_hashtable *vlt_hash_init_table (int);
extern void vlt_hash_insert (struct vlt_hashtable *, void *);
static inline uint32_t vlt_hash_pointer (void *pointer);
static inline void * vlt_bucket_find (struct vlt_hash_bucket *slot,
                                      void * value, struct vlt_hash_bucket * end);
static inline void * vlt_hash_find (struct vlt_hashtable *, void *);



/* for debugging purposes... */

extern void dump_hashing_statistics (void);
extern void dump_bucket_info (struct vlt_hash_bucket *, uint32_t, uint32_t *, FILE *);
extern void dump_table_to_file (struct vlt_hashtable *, uint32_t, FILE *);
extern void dump_table (struct vlt_hashtable *, uint32_t);
extern void dump_table_to_filename (struct vlt_hashtable *, uint32_t, char *);
extern void dump_table_to_vtbl_map_file (struct vlt_hashtable *,uint32_t,
                                         char *, uint32_t);


static inline uint32_t
vlt_hash_pointer (void *pointer)
{
  uint32_t result;
  intptr_t numeric = (intptr_t) pointer;

#if __SIZEOF_POINTER__ == 8
  numeric += numeric >> 32;
#elif __SIZEOF_POINTER__ != 4
#error "Unsupported pointer size."
#endif
  result = numeric;
  result += result >> 15;
  result += result >> 10;
  result += result >> 6;
  result += result >> 3;
  result *= 0x953653a5u;
  result += result >> 11;
  return result;
}

static inline void *
vlt_bucket_find (struct vlt_hash_bucket *slot, void * value,
                 struct vlt_hash_bucket * end)
{
  struct vlt_hash_bucket *current;

  for (current = slot; current != end; current = current->next)
    if (current->data == value)
      return current;

  return NULL;
}

static inline void *
vlt_hash_find (struct vlt_hashtable *table, void *value)
{
  struct vlt_hash_bucket **data = table->data;
  /* NOTE:  data[-1] contains hash mask for the table. */
  uint64_t hash_mask = (uint64_t) (data[-1]);
  uint32_t hash = vlt_hash_pointer (value);
  uint32_t new_index = hash & (uint32_t) hash_mask;

  return (void *) vlt_bucket_find (data[new_index], value, NULL);
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _VTV_THREADED_HASH_H */
