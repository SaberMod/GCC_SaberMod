/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "stdint.h"
#include "vect-ld1r.x"

DEF (float)
DEF (double)

/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.4s"} } */
/* { dg-final { scan-assembler "ldr\\t\d\[0-9\]+"} } */
/* { dg-final { scan-assembler "ld1r\\t\{v\[0-9\]+\.2d"} } */

