/* TO DO:  Add copyright, etc. */

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
