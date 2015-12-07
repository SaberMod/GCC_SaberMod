/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "bndint" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

struct S
{
  int a;
  int b;
  int c;
};

int test (struct S *ps)
{
  int *pi = &ps->b;
  return *pi;
}
