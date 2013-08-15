/* { dg-do compile } */
/* { dg-options "-O2 -mtune=corei7 -mmemcpy-strategy=loop:299:align,libcall:-1:align" } */
/* { dg-final { scan-assembler-times "memcpy" 2  } } */

char a[2048];
char b[2048];
void t (void)
{
  __builtin_memcpy (a, b, 300);
}
