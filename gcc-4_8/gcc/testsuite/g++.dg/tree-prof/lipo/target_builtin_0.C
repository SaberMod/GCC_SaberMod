/* Test case to check if LIPO and target specific builtins work fine.  */
/* { dg-options "-O2" } */

__attribute__((target("sse4.2")))
unsigned int crc_aux (unsigned int A, unsigned int B);

int main ()
{
  return crc_aux (0, 0);
}
