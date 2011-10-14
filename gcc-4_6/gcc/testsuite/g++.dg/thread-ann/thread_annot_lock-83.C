// Regression test for two bugfixes.
// Bugfix 1:  Shared locks are not properly removed from locksets 
// if a "universal lock" is present. 
// Bugfix 2:  Canonicalization does not properly store the lock in 
// the hash table if the lock function is attached to a base class.  
// { dg-do compile }
// { dg-options "-Wthread-safety" }

#include "thread_annot_common.h"

class Foo;

/* Bugfix 1 */
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


/* Bugfix 2 */
class LOCKABLE Base {
public:
  Mutex mu_;
  
  void Lock()   EXCLUSIVE_LOCK_FUNCTION()   { mu_.Lock();   }
  void Unlock() UNLOCK_FUNCTION()           { mu_.Unlock(); }
};

class Derived : public Base {
public: 
  int b;
};

void doSomething(Derived *d) {
  d->Lock();
  d->Unlock();
};

