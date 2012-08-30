// Test template methods in the presence of cloned constructors.
// Regression test for bugfix.  
// { dg-do compile }
// { dg-options "-Wthread-safety -O" }

#include "thread_annot_common.h"

Mutex mu_;
Mutex mu2_;

class Foo {
  Foo() LOCKS_EXCLUDED(mu_)	{ }
  
  template <class T>
  void bar(T* t) LOCKS_EXCLUDED(mu2_) { }
};

