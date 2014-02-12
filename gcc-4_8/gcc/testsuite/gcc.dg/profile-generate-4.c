/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -fprofile-generate -fno-tree-loop-im -fdump-tree-ivopts-details-blocks" } */

/* Because gcov counter related var has data race for multithread program,
   compiler should prevent them from affecting program correctness. So
   PROF_edge_counter variable should not be used as induction variable, or
   else IVOPT may use such variable to compute loop boundary.  */

void *ptr;
int N;

void foo(void *t) {
  int i;
  for (i = 0; i < N; i++) {
    t = *(void **)t;
  }
  ptr = t;
}

/* { dg-final { scan-tree-dump-times "ssa name PROF_edge_counter" 0 "ivopts"} } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
