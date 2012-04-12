// Regression test, handle trylock on virtual method. 
// { dg-do compile }
// { dg-options "-Wthread-safety" }

#include "thread_annot_common.h"

class LOCKABLE Lock {
 public:
 virtual ~Lock() {}
 virtual bool TryLock() EXCLUSIVE_TRYLOCK_FUNCTION(true) { return true; }
 void Unlock() UNLOCK_FUNCTION() {}
};


void foo() {
  Lock x;
  Lock *p = &x;
  if (p->TryLock()) {
    p->Unlock();
  }
}

