/* Test the vqdmlslh_laneq_s16 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

extern void abort (void);

int
main (void)
{
  int32_t arg1;
  int16_t arg2;
  int16x8_t arg3;
  int32_t actual;
  int32_t expected;

  arg1 = -2147450881;
  arg2 = 32767;
  arg3 = vcombine_s16 (vcreate_s16 (0x359d7fff00007fffULL),
                       vcreate_s16 (0xe678ffff00008000ULL));

  actual = vqdmlslh_laneq_s16 (arg1, arg2, arg3, 4);
  expected = -32769;

  if (expected != actual)
    abort ();

  return 0;
}


/* { dg-final { scan-assembler-times "sqdmlsl\[ \t\]+\[sS\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[4\\\]\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
