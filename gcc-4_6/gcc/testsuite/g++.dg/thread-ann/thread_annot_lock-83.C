// Regression test for bugfix, where shared locks are not properly 
// removed from locksets if a "universal lock" is present. 
// { dg-do compile }
// { dg-options "-Wthread-safety" }

#include "thread_annot_common.h"

class Foo;

class Bar {
public:
  Foo*  foo;
  Mutex mu_;

  // foo->mu_ is not visible at this point in the code. 
  // so the attribute will become a "universal lock."
  void bar() EXCLUSIVE_LOCKS_REQUIRED(foo->mu_);
};


class Foo {
public:
  Mutex mu_;
  int a;
};


void Bar::bar() {
  ReaderMutexLock rlock(&mu_);
}

