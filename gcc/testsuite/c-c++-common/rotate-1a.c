/* { dg-do run } */
/* { dg-options "-O2 -Wno-overflow" } */

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

#ifndef ROTATE_N
#define ROTATE_N "rotate-1.c"
#endif

#include ROTATE_N

unsigned int expected[] = {
0x91a2b3c0, 0x91a2b3c0, 0x2468acf0, 0x91a2b3c, 0xb3c2, 0xb3c2, 0xc3, 0xc3,
0x91a2b3c0, 0x91a2b3c0, 0x2468acf0, 0x91a2b3c, 0xb3c2, 0xb3c2, 0xc3, 0xc3,
0x91a2b3c0, 0x91a2b3c0, 0x2468acf0, 0x91a2b3c, 0xb3c2, 0xb3c2, 0xc3, 0xc3,
0x91a2b3c0, 0x91a2b3c0, 0x2468acf0, 0x91a2b3c, 0xb3c2, 0xb3c2, 0xc3, 0xc3,
0x2468acf, 0x2468acf, 0x91a2b3c, 0x2468acf0, 0xacf, 0xacf, 0xf, 0xf,
0x2468acf, 0x2468acf, 0x91a2b3c, 0x2468acf0, 0xacf, 0xacf, 0xf, 0xf,
0x2468acf, 0x2468acf, 0x91a2b3c, 0x2468acf0, 0xacf, 0xacf, 0xf, 0xf,
0x2468acf, 0x2468acf, 0x91a2b3c, 0x2468acf0, 0xacf, 0xacf, 0xf, 0xf };

#define F(n) __typeof (f##n) f##n __attribute__((noinline, noclone));
#define D(n) F(n##0) F(n##1) F(n##2) F(n##3) F(n##4) F(n##5) F(n##6) F(n##7) F(n##8) F(n##9)
#define ALL \
F(1) F(2) F(3) F(4) F(5) F(6) F(7) F(8) F(9) \
D(1) D(2) D(3) D(4) D(5) F(60) F(61) F(62) F(63) F(64)
ALL

int
main ()
{
#if __CHAR_BIT__ != 8 || __SIZEOF_SHORT__ != 2 || __SIZEOF_INT__ != 4
  return 0;
#else
#undef F
#define F(n) if ((unsigned int) f##n (0x12345678U, 3) != expected[n - 1]) abort ();
  ALL
  return 0;
#endif
}
