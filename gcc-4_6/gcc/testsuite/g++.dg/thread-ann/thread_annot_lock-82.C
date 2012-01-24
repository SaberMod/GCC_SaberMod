// Regression tests: fix ICE issues when IPA-SRA deletes formal 
// function parameters. 
// { dg-do compile }
// { dg-options "-Wthread-safety -O3" }

#include "thread_annot_common.h"

class Foo;
void do_something(void* a);

class Foo {
  Mutex mu_;
  int a GUARDED_BY(mu_);
  int b;

  // with optimization turned on, ipa-sra should eliminate the hidden 
  // "this" argument, thus invalidating EXCLUSIVE_LOCKS_REQUIRED.
  inline void clone_me_ipasra(Foo* f) EXCLUSIVE_LOCKS_REQUIRED(mu_) { 
    do_something(f);
  }
  
  void foo(Foo* f);
  void bar();
};

void Foo::foo(Foo* f) {
  mu_.Lock();
  // in the cloned version, it looks like the required lock is f->mu_
  // we should detect this and ignore the attribute.  
  clone_me_ipasra(f);  
  mu_.Unlock();
}


class SCOPED_LOCKABLE DummyMutexLock {
public:
  // IPA-SRA should kill the parameters to these functions
  explicit DummyMutexLock(Mutex* mutex) EXCLUSIVE_LOCK_FUNCTION(mutex) {}
  ~DummyMutexLock() UNLOCK_FUNCTION() {}
};


void Foo::bar() {
  // Matches two warnings:
  DummyMutexLock dlock(&mu_);  // { dg-warning "lock attribute has been removed by optimization" }
  a = 1;  // warning here should be suppressed, due to errors handling dlock
  b = 2;  // { dg-warning "unlock attribute has been removed by optimization" }
}
