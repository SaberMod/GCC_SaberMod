#include <stdint.h>

#include "vtv_malloc.h"
#include "vtv_utils.h"
#include "vtv_set.h"
#include "vtv_map.h"


/* Types needed by insert_only_hash_sets.  */

#ifdef VTV_32BIT_SETS
typedef uint32_t int_vptr;
#else
typedef uintptr_t int_vptr;
#endif

static inline int_vptr
vptr_to_int_vptr (const void * vptr)
{
#ifdef VTV_32BIT_SETS
  VTV_DEBUG_ASSERT ((vptr & (0x7)) == 0);
  return (int_vptr)((uintptr_t)vptr);
  /*  return (int_vptr)((uintptr_t)vptr >> 3); */
#else
  return (int_vptr)vptr;
#endif
}

/* The set of valid vtable pointers for each virtual class is stored
   in a hash table.  This is the hashing function used for the hash
   table.  For more information on the implementation of the hash
   table, see the class insert_only_hash_sets in vtv_set.h.  */

struct vptr_hash
  {
    /* Hash function, used to convert vtable pointer, V, (a memory
       address) into an index into the hash table.  */
    size_t
    operator() (int_vptr v) const
      {
        const uint32_t x = 0x7a35e4d9;
        const int shift = (sizeof (v) == 8) ? 23 : 21;
        v = x * v;
        return v ^ (v >> shift);
      }
  };

/* This is the memory allocator used to create the hash table data
   sets of valid vtable pointers.  We use __vtv_malloc in order to keep
   track of which pages have been allocated, so we can update the
   protections on those pages appropriately.  See the class
   insert_only_hash_sets in vtv_set.h for more information.  */

struct vptr_set_alloc
  {
    /* Memory allocator operator.  N is the number of bytes to be
       allocated.  */
    void *
    operator() (size_t n) const
      {
        return __vtv_malloc (n);
      }
  };

/* Instantiate the template classes (in vtv_set.h) for our particular
   hash table needs.  */
typedef insert_only_hash_sets<int_vptr, vptr_hash, vptr_set_alloc> vtv_sets;
typedef vtv_sets::insert_only_hash_set vtv_set;
typedef vtv_set * vtv_set_handle;
typedef vtv_set_handle * vtv_set_handle_handle;

/* This is the memory allocator used to create the hash table that
   maps from vtable map variable name to the data set that vtable map
   variable should point to.  This is part of our vtable map variable
   symbol resolution, which is necessary because the same vtable map
   variable may be created by multiple compilation units and we need a
   method to make sure that all vtable map variables for a particular
   class point to the same data set at runtime.  */

struct insert_only_hash_map_allocator
  {
    /* N is the number of bytes to allocate.  */
    void *
    alloc (size_t n) const
    {
      return __vtv_malloc (n);
    }

    /* P points to the memory to be deallocated; N is the number of
       bytes to deallocate.  */
    void
    dealloc (void *p, size_t n __attribute__((__unused__))) const
    {
      __vtv_free (p);
    }
  };

/* Explicitly instantiate this class since this file is compiled with
   -fno-implicit-templates.  These are for the hash table that is used
   to do vtable map variable symbol resolution.  */
template class insert_only_hash_map <vtv_set_handle *,
                                     insert_only_hash_map_allocator >;
typedef insert_only_hash_map <vtv_set_handle *,
                              insert_only_hash_map_allocator > s2s;


const unsigned long SET_HANDLE_HANDLE_BIT = 0x2;

/* In the case where a vtable map variable is the only instance of the
   variable we have seen, it points directly to the set of valid
   vtable pointers.  All subsequent instances of the 'same' vtable map
   variable point to the first vtable map variable.  This function,
   given a vtable map variable PTR, checks a bit to see whether it's
   pointing directly to the data set or to the first vtable map
   variable.  */

static inline bool
is_set_handle_handle (void * ptr)
{
  return ((unsigned long) ptr & SET_HANDLE_HANDLE_BIT)
                                                      == SET_HANDLE_HANDLE_BIT;
}

/* Returns the actual pointer value of a vtable map variable, PTR (see
   comments for is_set_handle_handle for more details).  */

static inline vtv_set_handle *
ptr_from_set_handle_handle (void * ptr)
{
  return (vtv_set_handle *) ((unsigned long) ptr & ~SET_HANDLE_HANDLE_BIT);
}

/* Given a vtable map variable, PTR, this function sets the bit that
   says this is the second (or later) instance of a vtable map
   variable.  */

static inline vtv_set_handle_handle
set_handle_handle (vtv_set_handle * ptr)
{
  return (vtv_set_handle_handle) ((unsigned long) ptr | SET_HANDLE_HANDLE_BIT);
}
