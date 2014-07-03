/* Test MIPS MSA ASE instructions */
/* { dg-do compile } */
/* { dg-options "-mips32r2 -mfp64 -mhard-float -mmsa" } */

typedef signed char v16i8 __attribute__ ((vector_size(16)));
typedef short v8i16 __attribute__ ((vector_size(16)));
typedef int v4i32 __attribute__ ((vector_size(16)));
typedef long long v2i64 __attribute__ ((vector_size(16)));
typedef float v4f32 __attribute__ ((vector_size(16)));
typedef double v2f64 __attribute__ ((vector_size(16)));

typedef signed char v8i8 __attribute__ ((vector_size(8)));
typedef short v4i16 __attribute__ ((vector_size(8)));
typedef int v2i32 __attribute__ ((vector_size(8)));
typedef float v2f32 __attribute__ ((vector_size(8)));

typedef signed char v4i8 __attribute__ ((vector_size(4)));
typedef short v2i16 __attribute__ ((vector_size(4)));

typedef long long i64;
typedef int i32;
typedef short i16;
typedef signed char i8;
typedef double f64;
typedef float f32;

#define DECLARE(TYPE) TYPE TYPE ## _0, TYPE ## _1, TYPE ## _2;
#define RETURN(TYPE) TYPE test0_ ## TYPE () { return TYPE ## _0; }
#define ASSIGN(TYPE) void test1_ ## TYPE (TYPE i) { TYPE ## _1 = i; }
#define ADD(TYPE) TYPE test2_ ## TYPE (TYPE i, TYPE j) { return i + j; }
#define SUB(TYPE) TYPE test3_ ## TYPE (TYPE i, TYPE j) { return i - j; }
#define MUL(TYPE) TYPE test4_ ## TYPE (TYPE i, TYPE j) { return i * j; }
#define DIV(TYPE) TYPE test5_ ## TYPE (TYPE i, TYPE j) { return i / j; }
#define MOD(TYPE) TYPE test6_ ## TYPE (TYPE i, TYPE j) { return i % j; }
#define MINUS(TYPE) TYPE test7_ ## TYPE (TYPE i) { return -i; }
#define XOR(TYPE) TYPE test8_ ## TYPE (TYPE i, TYPE j) { return i ^ j; }
#define OR(TYPE) TYPE test9_ ## TYPE (TYPE i, TYPE j) { return i | j; }
#define AND(TYPE) TYPE test10_ ## TYPE (TYPE i, TYPE j) { return i & j; }
#define BIT_COMPLEMENT(TYPE) TYPE test11_ ## TYPE (TYPE i) { return ~i; }
#define SHIFT_RIGHT(TYPE) TYPE test12_ ## TYPE (TYPE i, TYPE j) { return i >> j; }
#define SHIFT_LEFT(TYPE) TYPE test13_ ## TYPE (TYPE i, TYPE j) { return i << j; }
#define EQ(TYPE) TYPE test14_ ## TYPE (TYPE i, TYPE j) { return i == j; }
#define NEQ(TYPE) TYPE test15_ ## TYPE (TYPE i, TYPE j) { return i != j; }
#define LT(TYPE) TYPE test16_ ## TYPE (TYPE i, TYPE j) { return i < j; }
#define LEQ(TYPE) TYPE test17_ ## TYPE (TYPE i, TYPE j) { return i <= j; }
#define GT(TYPE) TYPE test18_ ## TYPE (TYPE i, TYPE j) { return i > j; }
#define GEQ(TYPE) TYPE test19_ ## TYPE (TYPE i, TYPE j) { return i >= j; }

#define ADD_I(TYPE) TYPE test20_ ## TYPE (TYPE i) { return i + 37; }
#define SUB_I(TYPE) TYPE test21_ ## TYPE (TYPE i) { return i - 37; }
#define MUL_I(TYPE) TYPE test22_ ## TYPE (TYPE i) { return i * 37; }
#define DIV_I(TYPE) TYPE test23_ ## TYPE (TYPE i) { return i / 37; }
#define MOD_I(TYPE) TYPE test24_ ## TYPE (TYPE i) { return i % 37; }
#define XOR_I(TYPE) TYPE test25_ ## TYPE (TYPE i) { return i ^ 37; }
#define OR_I(TYPE) TYPE test26_ ## TYPE (TYPE i) { return i | 37; }
#define AND_I(TYPE) TYPE test27_ ## TYPE (TYPE i) { return i & 37; }
#define SHIFT_RIGHT_I(TYPE) TYPE test28_ ## TYPE (TYPE i) { return i >> 3; }
#define SHIFT_LEFT_I(TYPE) TYPE test29_ ## TYPE (TYPE i) { return i << 3; }

#define ADD_F(TYPE) TYPE test30_ ## TYPE (TYPE i) { return i + 37.0; }
#define SUB_F(TYPE) TYPE test31_ ## TYPE (TYPE i) { return i - 37.0; }
#define MUL_F(TYPE) TYPE test32_ ## TYPE (TYPE i) { return i * 37.0; }
#define DIV_F(TYPE) TYPE test33_ ## TYPE (TYPE i) { return i / 37.0; }

#define SHUFFLE1(TYPE) TYPE test34_ ## TYPE (TYPE i, TYPE mask) { return __builtin_shuffle (i, mask); }
#define SHUFFLE2(TYPE) TYPE test35_ ## TYPE (TYPE i, TYPE j, TYPE mask) { return __builtin_shuffle (i, j, mask); }

#define REAL_SHUFFLE1(TYPE, MASK_TYPE) TYPE test36_ ## TYPE (TYPE i, MASK_TYPE mask) { return __builtin_shuffle (i, mask); }
#define REAL_SHUFFLE2(TYPE, MASK_TYPE) TYPE test37_ ## TYPE (TYPE i, TYPE j, MASK_TYPE mask) { return __builtin_shuffle (i, j, mask); }

#define ITERATE_FOR_ALL_INT_VECTOR_TYPES(FUNC) \
  FUNC (v16i8) \
  FUNC (v8i16) \
  FUNC (v4i32) \
  FUNC (v2i64) \
  FUNC (v8i8) \
  FUNC (v4i16) \
  FUNC (v2i32) \
  FUNC (v4i8) \
  FUNC (v2i16)

#define ITERATE_FOR_ALL_INT_SCALAR_TYPES(FUNC) \
  FUNC (i64) \
  FUNC (i32) \
  FUNC (i16) \
  FUNC (i8)

#define ITERATE_FOR_ALL_INT_TYPES(FUNC) \
  ITERATE_FOR_ALL_INT_VECTOR_TYPES(FUNC) \
  ITERATE_FOR_ALL_INT_SCALAR_TYPES(FUNC)

#define ITERATE_FOR_ALL_REAL_VECTOR_TYPES(FUNC) \
  FUNC (v4f32) \
  FUNC (v2f64) \
  FUNC (v2f32)

#define ITERATE_FOR_ALL_REAL_SCALAR_TYPES(FUNC) \
  FUNC (f64) \
  FUNC (f32)

#define ITERATE_FOR_ALL_REAL_TYPES(FUNC) \
  ITERATE_FOR_ALL_REAL_VECTOR_TYPES(FUNC) \
  ITERATE_FOR_ALL_REAL_SCALAR_TYPES(FUNC)

#define ITERATE_FOR_ALL_TYPES(FUNC) \
  ITERATE_FOR_ALL_INT_TYPES(FUNC) \
  ITERATE_FOR_ALL_REAL_TYPES(FUNC)

ITERATE_FOR_ALL_TYPES (DECLARE)
ITERATE_FOR_ALL_TYPES (RETURN)
ITERATE_FOR_ALL_TYPES (ASSIGN)
ITERATE_FOR_ALL_TYPES (ADD)
ITERATE_FOR_ALL_TYPES (SUB)
ITERATE_FOR_ALL_TYPES (MUL)
ITERATE_FOR_ALL_TYPES (DIV)
ITERATE_FOR_ALL_TYPES (MINUS)
ITERATE_FOR_ALL_INT_TYPES (MOD)
ITERATE_FOR_ALL_INT_TYPES (XOR)
ITERATE_FOR_ALL_INT_TYPES (OR)
ITERATE_FOR_ALL_INT_TYPES (AND)
ITERATE_FOR_ALL_INT_TYPES (BIT_COMPLEMENT)
ITERATE_FOR_ALL_INT_TYPES (SHIFT_RIGHT)
ITERATE_FOR_ALL_INT_TYPES (SHIFT_LEFT)
ITERATE_FOR_ALL_INT_TYPES (ADD_I)
ITERATE_FOR_ALL_INT_TYPES (SUB_I)
ITERATE_FOR_ALL_INT_TYPES (MUL_I)
ITERATE_FOR_ALL_INT_TYPES (DIV_I)
ITERATE_FOR_ALL_INT_TYPES (MOD_I)
ITERATE_FOR_ALL_INT_TYPES (XOR_I)
ITERATE_FOR_ALL_INT_TYPES (OR_I)
ITERATE_FOR_ALL_INT_TYPES (AND_I)
ITERATE_FOR_ALL_INT_TYPES (SHIFT_RIGHT_I)
ITERATE_FOR_ALL_INT_TYPES (SHIFT_LEFT_I)
ITERATE_FOR_ALL_REAL_TYPES (ADD_F)
ITERATE_FOR_ALL_REAL_TYPES (SUB_F)
ITERATE_FOR_ALL_REAL_TYPES (MUL_F)
ITERATE_FOR_ALL_REAL_TYPES (DIV_F)
ITERATE_FOR_ALL_INT_VECTOR_TYPES (SHUFFLE1)
ITERATE_FOR_ALL_INT_VECTOR_TYPES (SHUFFLE2)
REAL_SHUFFLE1 (v2f64, v2i64)
REAL_SHUFFLE2 (v2f64, v2i64)
REAL_SHUFFLE1 (v4f32, v4i32)
REAL_SHUFFLE2 (v4f32, v4i32)
REAL_SHUFFLE1 (v2f32, v2i32)
REAL_SHUFFLE2 (v2f32, v2i32)
ITERATE_FOR_ALL_TYPES (EQ)
ITERATE_FOR_ALL_TYPES (NEQ)
ITERATE_FOR_ALL_TYPES (LT)
ITERATE_FOR_ALL_TYPES (LEQ)
ITERATE_FOR_ALL_TYPES (GT)
ITERATE_FOR_ALL_TYPES (GEQ)
