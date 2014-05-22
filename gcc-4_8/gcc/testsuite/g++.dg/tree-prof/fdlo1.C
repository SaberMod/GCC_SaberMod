/* Ensure that parameter profiling behaves as expected.  */
/* { dg-options "-fprofile-use-parameters -fdump-tree-profile_estimate-all" } */

#include<stdio.h>

class Container
{
public:
  Container();
  void resize (size_t s);

private:

/* Profiling specific section:  */

  typedef struct ProfCounter {
    long long total;
    int count;
  } ProfCounter;

  static void profile_init (void) __attribute__((profile_init));
  static void profile_handler (void);
  static void profile_update (void *, long long value);
  static ProfCounter *counter_;
};

Container::ProfCounter *Container::counter_;

Container::Container()
{
#ifndef BUF_SIZE
#define BUF_SIZE 32
#endif
  fprintf (stderr, "Container BUF_SIZE is %d\n", BUF_SIZE);
}

void
Container::profile_init (void)
{
  /* Counter creation */
  counter_ = new ProfCounter ();
  counter_->total = 0;
  counter_->count = 0;

  /* Register handler:  */
  __builtin_profile_register_handler (&profile_handler);
}

void
Container::profile_handler (void)
{
  /* Process counter_ data */
  long long optimal_buffer_size = counter_->total/counter_->count;

  /* Now record the optimal buffer size */
  __builtin_profile_record_parameter ("BUF_SIZE", optimal_buffer_size);
}

void
Container::resize (size_t s)
{
  __builtin_profile_invoke (profile_update, counter_, s);
}

void
Container::profile_update (void * profile_counter, long long data)
{
  ProfCounter *p = (ProfCounter *) profile_counter;
  p->total += data;
  p->count++;
}

int main()
{
  Container s;
  s.resize(10);
  s.resize(20);
  return 0;
}

/* { dg-final-use { scan-tree-dump "note: Add -DBUF_SIZE=15 from profile" "profile_estimate"} } */
/* { dg-final-use { cleanup-tree-dump "profile_estimate" } } */
