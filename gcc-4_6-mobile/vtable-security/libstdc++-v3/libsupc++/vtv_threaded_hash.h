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

#include <pthread.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

struct vlt_hash_bucket {
  void *data;
  struct vlt_hash_bucket *next;
};

struct vlt_hashtable {
  uint32_t data_size;
  uint32_t num_elts;
  uint32_t power_of_2;
  uint32_t hash_mask;
  pthread_mutex_t mutex;
  struct vlt_hash_bucket **data;
};

enum vlt_hash_access_kind {
  TABLE_INSERT,
  TABLE_FIND
};

/* Main hash table interface */

extern struct vlt_hashtable *vlt_hash_init_table (int);
extern void vlt_hash_insert (struct vlt_hashtable *, void *);
extern void *vlt_hash_find (struct vlt_hashtable *, void *);

/* for debugging purposes... */

extern void dump_hashing_statistics (void);
extern void dump_bucket_info (struct vlt_hash_bucket *, uint32_t, uint32_t *, FILE *);
extern void dump_table_to_file (struct vlt_hashtable *, uint32_t, FILE *);
extern void dump_table (struct vlt_hashtable *, uint32_t);
extern void dump_table_to_filename (struct vlt_hashtable *, uint32_t, char *);
extern void dump_table_to_vtbl_map_file (struct vlt_hashtable *,uint32_t,
                                         char *, uint32_t);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _VTV_THREADED_HASH_H */
