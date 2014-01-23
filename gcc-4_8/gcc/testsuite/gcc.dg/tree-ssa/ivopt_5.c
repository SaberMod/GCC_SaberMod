/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -m64 -fdump-tree-ivopts-details" } */

/* Make sure only one iv is selected after IVOPT.  */

#include <x86intrin.h>
extern __m128i arr[], d[];
void test (void)
{
    unsigned int b;
    for (b = 0; b < 1000; b += 2) {
      __m128i *p = (__m128i *)(&d[b]);
      __m128i a = _mm_load_si128(&arr[4*b+3]);
      __m128i v = _mm_loadu_si128(p);
      v = _mm_xor_si128(v, a);
      _mm_storeu_si128(p, v);
    }
}

/* { dg-final { scan-tree-dump-times "PHI <ivtmp" 1 "ivopts"} } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
