/* Call the caller of __builtin_dispatch indirectly.  Specify the
   feature test function as a function pointer.  Make sure cloning
   still happens. */

/* { dg-do run } */
/* { dg-options "-O2 -fclone-hot-version-paths -fdump-tree-optimized" } */

int __attribute__ ((version_selector))
featureTest ()
{
  return 1;
}

int
foo ()
{
  return 0;
}

int
bar ()
{
  return 1;
}

int __attribute__ ((hot))
dispatch ()
{
  int (*funcp)() = featureTest;
  int ret = __builtin_dispatch (funcp, (void *)foo, (void *)bar);
  return ret;
}

int __attribute__ ((hot))
main (int argc, char **argv)
{
  int (*ptr)() = dispatch;
  return (*ptr)();
}

/* { dg-final { scan-tree-dump "dispatchv\.clone\.0" "optimized" } } */
/* { dg-final { scan-tree-dump "dispatchv\.clone\.1" "optimized" } } */
/* { dg-final { scan-tree-dump "main\.clone\.0" "optimized" } } */
/* { dg-final { scan-tree-dump "main\.clone\.1" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
