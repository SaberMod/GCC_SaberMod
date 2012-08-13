#include <stddef.h>

#include "vtv_set.h"
#include "vtv_threaded_hash.h"

static unsigned num_singletons_allocated = 0;
static unsigned num_singleton_hash_transitions = 0;
static unsigned num_hashes_allocated = 0;

vtv_set_handle
vtv_set_init(void * value, int initial_size_hint)
{
  vtv_set_handle new_set;
  if (initial_size_hint <= 1)
    {
      new_set.set_singleton(value);
      num_singletons_allocated++;
    }
  else
    {
      vlt_hashtable * hash = vlt_hash_init_table(initial_size_hint);
      vlt_hash_insert(hash, value);
      new_set.set_hash_map(hash);
      num_hashes_allocated++;
    }
  return new_set;
}

void
vtv_set_insert(vtv_set_handle * handle_ptr, void * value, int size_hint)
{
  vtv_set_handle::kind handle_kind = handle_ptr->get_kind();
  if (handle_kind == vtv_set_handle::singleton_set)
    {
      void * singleton = handle_ptr->get_singleton();

      // check if we need to transform the singleton into a hash table.
      if (singleton != value)
        {
#if defined __GTHREAD_MUTEX_INIT
          static __gthread_mutex_t set_var_mutex VTV_PROTECTED_VAR
              = __GTHREAD_MUTEX_INIT;
#else
          abort();
          __GTHREAD_MUTEX_INIT_FUNCTION(&set_var_mutex);
#endif
          __gthread_mutex_lock (&set_var_mutex);

          // only do something if the handle was not changed by another thread.
          if (handle_ptr->get_kind_v() == vtv_set_handle::singleton_set)
            {
              VTV_DEBUG_ASSERT(singleton == handle_ptr->get_singleton());
              vlt_hashtable * hash_ptr = vlt_hash_init_table(size_hint);
              vlt_hash_insert(hash_ptr, singleton);
              vlt_hash_insert(hash_ptr, value);

              vtv_set_handle new_handle;
              new_handle.set_hash_map(hash_ptr);

              // always do this last to avoid race conditions.
              *handle_ptr = new_handle;

              num_singleton_hash_transitions++;
            }

          __gthread_mutex_unlock (&set_var_mutex);
        }

      return;
    }

  if (handle_kind == vtv_set_handle::hash_set)
    {
      vlt_hash_insert(handle_ptr->get_hash_map(), value);
      return;
    }

  VTV_ASSERT(0);
}

void
vtv_set_dump_statistics()
{
  static FILE * fp = 0;
  static bool first_time = true;
  static const char * stats_file = "/tmp/vtv-set-statistics.log";

  if (first_time)
  {
    remove(stats_file);
    first_time = false;
  }

  fp = fopen(stats_file, "a");
  if (!fp)
    return;

  fprintf(fp, "Set statistics\n\n");
  fprintf(fp, "  # singletons allocated: %u\n", num_singletons_allocated);
  fprintf(fp, "  # hashes allocated: %u\n", num_hashes_allocated);
  fprintf(fp, "  # singleton to hash transitions: %u\n", num_singleton_hash_transitions);
  fprintf(fp, "\n");

  dump_hashing_statistics(fp);

  fprintf(fp, "\n----\n\n");

  fclose(fp);
}
