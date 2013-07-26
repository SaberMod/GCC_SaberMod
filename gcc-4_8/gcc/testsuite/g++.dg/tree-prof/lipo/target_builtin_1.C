/* Test case to check if LIPO and target specific builtins work fine.  */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } { "*" } { "" } } */
/* { dg-options "-O2" } */

__attribute__((target("sse4.2")))
unsigned int crc_aux (unsigned int A, unsigned int B)
{
  return __builtin_ia32_crc32qi (A, B);
}
