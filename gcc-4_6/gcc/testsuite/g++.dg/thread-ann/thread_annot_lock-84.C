// Test lock expressions involving array elements.  
// { dg-do compile }
// { dg-options "-Wthread-safety" }

#include "thread_annot_common.h"

struct Foo {
  Mutex mu_;
  int a GUARDED_BY(mu_);
  
  static void foo1(Foo* foos, int n);
  static void foo2(Foo* foos, int n);
};

void Foo::foo1(Foo* foos, int n) {
  for (int i = 0; i < n; ++i) {
    foos[i].mu_.Lock();
    foos[i].a = 0;
    foos[i].mu_.Unlock();
  }
}

void Foo::foo2(Foo* foos, int n) {
  for (int i = 0; i < n-1; ++i) {
    foos[i].mu_.Lock();
    foos[i+1].a = 0;    // { dg-warning "Writing to variable" }
    foos[i].mu_.Unlock();
  }
}
