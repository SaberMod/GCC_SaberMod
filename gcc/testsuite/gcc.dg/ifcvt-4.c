/* { dg-options "-fdump-rtl-ce1 -O2" } */
int
foo (int x, int y, int a)
{
  int i = x;
  int j = y;
  /* Try to make taking the branch likely.  */
  __builtin_expect (x > y, 1);
  if (x > y)
    {
      i = a;
      j = i;
    }
  return i * j;
}
/* { dg-final { scan-rtl-dump "2 true changes made" "ce1" } } */
