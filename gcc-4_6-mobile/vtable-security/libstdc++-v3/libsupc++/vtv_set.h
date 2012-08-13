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

#ifndef _VTV_SET_H
#define _VTV_SET_H 1

#ifdef __cplusplus
extern "C" {
#endif

#include "vtv_utils.h"
#include "vtv_threaded_hash.h"


class vtv_set_handle {

public:
  vtv_set_handle();

  bool is_null() const;
  bool is_null_v() const volatile;

  enum kind {
    no_set = 0,
    singleton_set = 1,
    hash_set = 2
  };

  kind get_kind() const;
  kind get_kind_v() const volatile;

  void * get_singleton() const;
  vlt_hashtable * get_hash_map() const;

  void set_singleton(void * singleton);
  void set_hash_map(vlt_hashtable * hash_map);

private:

  static const unsigned long type_bits = 2;
  static const unsigned long type_bits_mask = (1 << type_bits) - 1;
  static const unsigned long pointer_bits_mask = ~type_bits_mask;

  void * get_pointer() const;
  void * get_pointer_v() const volatile;

  void set_raw_value(kind, void *);

  unsigned long raw_value;
};

inline
vtv_set_handle::vtv_set_handle()
    : raw_value(0)
{
}

inline vtv_set_handle::kind
vtv_set_handle::get_kind() const
{
  return (kind)(raw_value & type_bits_mask);
}

inline vtv_set_handle::kind
vtv_set_handle::get_kind_v() const volatile
{
  return (kind)(raw_value & type_bits_mask);
}

inline void *
vtv_set_handle::get_pointer() const
{
  return (void *) (raw_value & pointer_bits_mask);
}

inline void *
vtv_set_handle::get_pointer_v() const volatile
{
  return (void *) (raw_value & pointer_bits_mask);
}

inline void
vtv_set_handle::set_raw_value(kind set_kind, void * pointer)
{
  raw_value = (unsigned long)pointer | (unsigned long)set_kind;
}

inline bool
vtv_set_handle::is_null() const
{
  //  VTV_DEBUG_ASSERT(get_pointer() != 0 || get_kind() == 0);
  return raw_value == 0;
}

inline bool
vtv_set_handle::is_null_v() const volatile
{
  //  VTV_DEBUG_ASSERT((get_pointer_v() != 0) || get_kind_v() == 0);
  return raw_value == 0;
}

inline void *
vtv_set_handle::get_singleton() const
{
  VTV_DEBUG_ASSERT(get_kind() == singleton_set);
  return (void *)get_pointer();
}

inline vlt_hashtable *
vtv_set_handle::get_hash_map() const
{
  VTV_DEBUG_ASSERT(get_kind() == hash_set);
  return (vlt_hashtable *)get_pointer();
}

inline void
vtv_set_handle::set_singleton(void * singleton)
{
  set_raw_value(singleton_set, singleton);
}

inline void
vtv_set_handle::set_hash_map(vlt_hashtable * hash_set_map)
{
  set_raw_value(hash_set, hash_set_map);
}

vtv_set_handle vtv_set_init(void * value, int initial_size_hint);
void vtv_set_insert(vtv_set_handle * handle, void * value, int size_hint);

static inline bool
vtv_set_find(vtv_set_handle handle, void * value)
{
  vtv_set_handle::kind handle_kind = handle.get_kind();
  if (handle_kind == vtv_set_handle::singleton_set)
    return (value == handle.get_singleton());
  if (handle_kind == vtv_set_handle::hash_set)
    return vlt_hash_find(handle.get_hash_map(), value) != NULL;
  VTV_ASSERT(0);
}

void vtv_set_dump_statistics();

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* _VTV_SET_H */
