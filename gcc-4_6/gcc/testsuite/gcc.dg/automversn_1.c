/* Check that the auto_clone pass works correctly.  Function foo must be cloned
   because it is hot and has a vectorizable store.  */

/* { dg-options "-O2 -ftree-vectorize -mvarch=core2 -fdump-tree-auto_clone" } */
/* { dg-do run } */

char a[16];

int __attribute__ ((hot)) __attribute__ ((noinline))
foo (void)
{
  int i;
  for (i = 0; i< 16; i++)
    a[i] = 0;
  return 0;
}

int
main ()
{
  return foo ();
}


/* { dg-final { scan-tree-dump "foo\.autoclone\.original" "auto_clone" } } */
/* { dg-final { scan-tree-dump "foo\.autoclone\.0" "auto_clone" } } */
/* { dg-final { cleanup-tree-dump "auto_clone" } } */
