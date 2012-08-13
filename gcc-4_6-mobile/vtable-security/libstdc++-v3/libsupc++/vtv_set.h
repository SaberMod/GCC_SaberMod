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

//
// Code in this file manages a collection of insert-only sets.  We have only
// tested the case where Key is uintptr_t, though it theoretically should work
// for some other cases.  All odd keys are reserved, and must not be inserted
// into any of the sets.  This code is intended primarily for sets of
// pointers, and the code is optimized for small sets (including size 0 and 1),
// but regardless of the set size, insert() and contains() have close to O(1)
// speed in practice.
//
// Recommended multithreaded use of a set:
//
// For speed, we want to use a lock-free test for set membership.  The code
// handles simultaneous reads and inserts, as long as at most one insertion
// is in progress at a time.  After an insert, other threads may not immediately
// "see" the inserted key if they perform a lock-free read, so we recommend
// retrying, as explained below.
//
// Also, to make data corruption less likely, we recommend using a "normal"
// RW page as well as one or pages that are typically RO but that can be
// switched to RW and back as needed.  The latter pages should contain
// sets.  The former should contain a lock, L, and an int or similar,
// num_writers.  Then, to insert, something like this would be safe:
//  o Acquire L.
//  o Increment num_writers; if that made it 1, change pages to RW.
//  o Release L.
//  o while (there are insertions to do in some set, S) {
//      acquire L;
//      do some insertions in S;
//      release L;
//    }
//  o Acquire L.
//  o Decrement num_writers; if that made it 0, change pages to RO.
//  o Release L.
//
// And to check if the set contains some key, one could use
//   set.contains(key) ||
//     ({ Acquire L; bool b = set.contains(key); Release L; b; })
//
// In this scheme, the number of threads with reads in progress isn't tracked,
// so old sets can never be deleted.  In addition, on some architectures
// the intentionally racy reads might cause contains() to return true when
// it should have returned false.  This should be no problem on x86, and most
// other machines, where reading or writing an aligned uintptr_t is atomic.
// E.g., on those machines, if *p is 0 and one thread does *p = x while
// another reads *p, the read will see either 0 or x.
//
// To make the above easier, the insert_only_hash_sets class provides an
// interface to manipulate any number of hash sets.  One shouldn't create
// objects of that class, as it has no member data and its methods are static.
//
// So the recommended model is to have a single lock, a single num_writers
// variable, and some number of sets.  If lock contention becomes a problem
// then the sets can be divided into k groups, each of which has a lock and a
// num_writers variable; or each set can be represented as a set of values
// that equal 0 mod m, a set of values that equal 1 mod m, ..., plus a set
// of values that equal m-1 mod m.
//
// However, we expect most or all uses of this code to call contains() much more
// frequently than anything else, so lock contention is likely to be low.
#include <assert.h>
#include <algorithm>

template<typename Key, class HashFcn, class Alloc>
class insert_only_hash_sets {
 public:
  typedef Key key_type;
  typedef size_t size_type;
  typedef Alloc alloc_type;
  enum { illegal_key = 1 };
  enum { min_capacity = 4 };

  // Do not directly use insert_only_hash_set.  Instead, use the static methods
  // below to create and manipulate objects of the following class.
  //
  // Implementation details: each set is represented by a pointer plus, perhaps,
  // out-of-line data, which would be an object of type insert_only_hash_set.
  // For a pointer, s, the interpretation is:
  //  s == NULL means empty set,
  //  lsb(s) == 1 means a set with one element, which is (uintptr_t)s - 1, and
  //  otherwise s is a pointer of type insert_only_hash_set*.
  // So, to increase the size of a set we have to change s and/or *s.
  // To check if a set contains some key we have to examine s and possibly *s.
  class insert_only_hash_set {
   public:
    // Insert a key.  The key must not be a reserved key.
    static inline insert_only_hash_set*
    insert (key_type key, insert_only_hash_set* s);

    // Create an empty set.
    static inline insert_only_hash_set*
    create (size_type capacity);

    // Return whether the given key is present.  If key is illegal_key
    // then either true or false may be returned, but for all other
    // reserved keys false will be returned.
    static bool
    contains (key_type key, const insert_only_hash_set* s)
    {
      return s != NULL &&
          (singleton(s) ? (singleton_key(key) == s) : s->contains(key));
    }

    // Return a set's size.
    static size_type
    size (const insert_only_hash_set* s)
    { return (s == NULL) ? 0 : (singleton(s) ? 1 : s->num_entries); }

   private:
    // Return whether a set has size 1.
    static bool singleton(const insert_only_hash_set* s)
    { return (uintptr_t)s & 1; }

    // Return the representation of a singleton set containing the given key.
    static insert_only_hash_set* singleton_key(key_type key)
    { return (insert_only_hash_set*)((uintptr_t)key + 1); }

    // Given a singleton set, what key does it contain?
    static key_type extract_singleton_key(const insert_only_hash_set* s)
    {
      assert(singleton(s));
      return (key_type)((uintptr_t)s - 1);
    }

    volatile key_type&
    key_at_index (size_type index)
    { return buckets[index]; }

    key_type
    key_at_index (size_type index) const
    { return buckets[index]; }

    size_type
    next_index (size_type index, size_type indices_examined) const
    { return (index + indices_examined) & (num_buckets - 1); }

    inline void
    insert_no_resize (key_type key);

    inline bool
    contains (key_type key) const;

    inline insert_only_hash_set*
    resize_if_necessary ();

    size_type num_buckets;  // Must be a power of 2 not less than min_capacity.
    volatile size_type num_entries;
    volatile key_type buckets[0];  // Actual array size is num_buckets.
  };

  // Create an empty set with the given capacity.  Requires that n be 0 or a
  // power of 2.  If 1 < n < min_capacity then treat n as min_capacity.
  // Sets *handle.  Returns true unless the allocator fails.  Subsequent
  // operations on this set should use the same handle.
  static inline bool
  create (size_type n, insert_only_hash_set** handle);

  // Insert a key.  *handle is unmodified unless (1) a resize occurs, or
  // (2) the set was initially empty. Returns true unless the allocator fails
  // during a resize.  If the allocator fails during a resize then the set is
  // reset to be the empty set.  The key must not be a reserved key.
  static inline bool
  insert (key_type key, insert_only_hash_set** handle);

  // Check for the presence of a key.  If the last arg is
  // NULL then no locking is done here.  Otherwise, an "optimistic"
  // lookup is done without the given lock, and if that doesn't
  // find the key then the lookup is repeated while holding the given lock.
  // If key is illegal_key then either true or false may be returned, but
  // for all other reserved keys false will be returned.
  static inline bool
  contains (key_type key,
            insert_only_hash_set** handle);

  // Return the size of the given set.
  static size_type
  size (const insert_only_hash_set** handle)
  { return insert_only_hash_set::size(*handle); }

  static bool
  is_reserved_key (key_type key)
  { return ((uintptr_t)key % 2) == 1; }

  static void
  dump_statistics()
  {
    // TODO
  }
};

template<typename Key, class HashFcn, class Alloc>
typename insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set*
insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set::insert (
    key_type key, insert_only_hash_set* s)
{
  assert(!is_reserved_key(key));
  if (s == NULL) return singleton_key(key);
  if (singleton(s))
    {
      const key_type old_key = extract_singleton_key(s);
      if (old_key == key) return s;
      // Grow from size 1 to size 2.
      s = create(2);
      if (s == NULL) return NULL;
      s->insert_no_resize(old_key);
      s->insert_no_resize(key);
      assert(size(s) == 2);
      return s;
    }
  s = s->resize_if_necessary();
  if (s != NULL) s->insert_no_resize(key);
  return s;
}

template<typename Key, class HashFcn, class Alloc>
typename insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set*
insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set::create (
    size_type capacity)
{
  assert(capacity > 1 && (capacity & (capacity - 1)) == 0);
  assert(sizeof(insert_only_hash_set) == 2 * sizeof(size_type));// remove?
  capacity = std::max<size_type>(capacity, min_capacity);
  const size_t num_bytes = sizeof(insert_only_hash_set) +
      sizeof(key_type) * capacity;
  alloc_type alloc;
  insert_only_hash_set* result = (insert_only_hash_set*)alloc(num_bytes);
  result->num_buckets = capacity;
  result->num_entries = 0;
  for (size_type i = 0; i < capacity; i++)
    result->buckets[i] = (key_type)illegal_key;
  return result;
}

template<typename Key, class HashFcn, class Alloc>
void
insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set::insert_no_resize (key_type key)
{
  HashFcn hasher;
  const size_type capacity = num_buckets;
  assert(capacity >= min_capacity);
  assert(!is_reserved_key(key));
  size_type index = hasher(key) & (capacity - 1);
  key_type k = key_at_index(index);
  size_type indices_examined = 0;
  while (k != key)
    {
      ++indices_examined;
      if (k == (key_type)illegal_key)
        {
          key_at_index(index) = key;
          ++num_entries;
          return;
        }
      assert(indices_examined < capacity);
      index = next_index(index, indices_examined);
      k = key_at_index(index);
    }
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set::contains (key_type key) const
{
  HashFcn hasher;
  const size_type capacity = num_buckets;
  size_type index = hasher(key) & (capacity - 1);
  key_type k = key_at_index(index);
  size_type indices_examined = 0;
  while (k != key)
    {
      ++indices_examined;
      if (/*UNLIKELY*/(k == (key_type)illegal_key ||
                       indices_examined == capacity)) return false;
      index = next_index(index, indices_examined);
      k = key_at_index(index);
    }
  return true;
}

template<typename Key, class HashFcn, class Alloc>
typename insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set*
insert_only_hash_sets<Key, HashFcn, Alloc>::insert_only_hash_set::resize_if_necessary ()
{
  assert(num_buckets >= min_capacity);
  size_type unused = num_buckets - num_entries;
  if (unused < (num_buckets >> 2))
    {
      size_type new_num_buckets = num_buckets * 2;
      insert_only_hash_set *s = create(new_num_buckets);
      for (size_type i = 0; i < num_buckets; i++)
        if (buckets[i] != (key_type)illegal_key)
          s->insert_no_resize(buckets[i]);
      assert(size(this) == size(s));
      return s;
    }
  else
    return this;
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::create (size_type n, insert_only_hash_set** handle)
{
  if (n <= 1)
    {
      *handle = NULL;
      return true;
    }
  *handle = insert_only_hash_set::create(n);
  return *handle != NULL;
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::insert (key_type key, insert_only_hash_set** handle)
{
  *handle = insert_only_hash_set::insert(key, *handle);
  return *handle != NULL;
}

template<typename Key, class HashFcn, class Alloc>
bool
insert_only_hash_sets<Key, HashFcn, Alloc>::contains (key_type key,
                                                      insert_only_hash_set** handle)
{
  return insert_only_hash_set::contains(key, *handle);
}
