/* { dg-options "-O2 -fopt-info=2 -fdisable-tree-einline -Werror" } */
#include <stdio.h>

int sum;

void foo(int a)
{
  sum += a;
}

int main()
{
  int i;

  for (i=0;i<10000;i++)
    foo (i);

  printf ("sum is %d\n", sum);

  return 0;
}
