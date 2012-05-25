/* { dg-do compile } */
/* { dg-options "-O2" } */

extern double fma (double, double, double);
extern float fmaf (float, float, float);

double test1 (double x, double y, double z)
{
  return fma (x, y, z);
}

float test2 (float x, float y, float z)
{
  return fmaf (x, y, z);
}

/* { dg-final { scan-assembler-times "fmadd\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fmadd\ts\[0-9\]" 1 } } */
