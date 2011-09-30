// Test template methods in the presence of cloned constructors.
// Regression test for bugfix.  
// { dg-do compile }
// { dg-options "-Wthread-safety -O3" }

#include "thread_annot_common.h"

class Foo;
void do_something(void* a);

class Foo {
  Mutex mu_;

  // with optimization turned on, ipa-sra should eliminate the hidden 
  // "this" argument, thus invalidating EXCLUSIVE_LOCKS_REQUIRED.
  inline void clone_me_ipasra(Foo* f) EXCLUSIVE_LOCKS_REQUIRED(mu_) { 
    do_something(f);
  }
  
  void foo(Foo* f);
};

void Foo::foo(Foo* f) {
  mu_.Lock();
  // in the cloned version, it looks like the required lock is f->mu_
  // we should detect this and ignore the attribute.  
  clone_me_ipasra(f);  
  mu_.Unlock();
}

