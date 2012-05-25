/* Machine description for AArch64 architecture.
   Copyright (C) 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define _FP_W_TYPE_SIZE		64
#define _FP_W_TYPE		unsigned long
#define _FP_WS_TYPE		signed long
#define _FP_I_TYPE		int

typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));
#define TI_BITS (__CHAR_BIT__ * (int)sizeof(TItype))

/* The type of the result of a floating point comparison.  This must
   match __libgcc_cmp_return__ in GCC for the target.  */
typedef int __gcc_CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));
#define CMPtype __gcc_CMPtype

#define _FP_MUL_MEAT_Q(R,X,Y)				\
  _FP_MUL_MEAT_2_wide(_FP_WFRACBITS_Q,R,X,Y,umul_ppmm)

#define _FP_DIV_MEAT_Q(R,X,Y)	_FP_DIV_MEAT_2_udiv(Q,R,X,Y)

#define _FP_NANFRAC_S		((_FP_QNANBIT_S << 1) - 1)
#define _FP_NANFRAC_D		((_FP_QNANBIT_D << 1) - 1)
#define _FP_NANFRAC_Q		((_FP_QNANBIT_Q << 1) - 1), -1
#define _FP_NANSIGN_S		0
#define _FP_NANSIGN_D		0
#define _FP_NANSIGN_Q		0

#define _FP_KEEPNANFRACP 1

/* This appears to be in line with the VFP conventions in the v7-a
   ARM-ARM. Need to check with the v8 version.  */
#define _FP_CHOOSENAN(fs, wc, R, X, Y, OP)			\
  do {								\
    if ((_FP_FRAC_HIGH_RAW_##fs(X) & _FP_QNANBIT_##fs)		\
	&& !(_FP_FRAC_HIGH_RAW_##fs(Y) & _FP_QNANBIT_##fs))	\
      {								\
	R##_s = Y##_s;						\
	_FP_FRAC_COPY_##wc(R,Y);				\
      }								\
    else							\
      {								\
	R##_s = X##_s;						\
	_FP_FRAC_COPY_##wc(R,X);				\
      }								\
    R##_c = FP_CLS_NAN;						\
  } while (0)

#define	__LITTLE_ENDIAN	1234
#define	__BIG_ENDIAN	4321

#if defined __AARCH64EB__
# define __BYTE_ORDER __BIG_ENDIAN
#else
# define __BYTE_ORDER __LITTLE_ENDIAN
#endif


/* Define ALIASNAME as a strong alias for NAME.  */
# define strong_alias(name, aliasname) _strong_alias(name, aliasname)
# define _strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

#ifdef __ARM_EABI__
/* Rename functions to their EABI names.  */
/* The comparison functions need wrappers for EABI semantics, so
   leave them unmolested.  */
#define __negsf2	__aeabi_fneg
#define __subsf3	__aeabi_fsub
#define __addsf3	__aeabi_fadd
#define __floatunsisf	__aeabi_ui2f
#define __floatsisf	__aeabi_i2f
#define __floatundisf	__aeabi_ul2f
#define __floatdisf	__aeabi_l2f
#define __mulsf3	__aeabi_fmul
#define __divsf3	__aeabi_fdiv
#define __unordsf2	__aeabi_fcmpun
#define __fixsfsi	__aeabi_f2iz
#define __fixunssfsi	__aeabi_f2uiz
#define __fixsfdi	__aeabi_f2lz
#define __fixunssfdi	__aeabi_f2ulz
#define __floatdisf	__aeabi_l2f

#define __negdf2	__aeabi_dneg
#define __subdf3	__aeabi_dsub
#define __adddf3	__aeabi_dadd
#define __floatunsidf	__aeabi_ui2d
#define __floatsidf	__aeabi_i2d
#define __extendsfdf2	__aeabi_f2d
#define __truncdfsf2	__aeabi_d2f
#define __floatundidf	__aeabi_ul2d
#define __floatdidf	__aeabi_l2d
#define __muldf3	__aeabi_dmul
#define __divdf3	__aeabi_ddiv
#define __unorddf2	__aeabi_dcmpun
#define __fixdfsi	__aeabi_d2iz
#define __fixunsdfsi	__aeabi_d2uiz
#define __fixdfdi	__aeabi_d2lz
#define __fixunsdfdi	__aeabi_d2ulz
#define __floatdidf	__aeabi_l2d

#endif /* __ARM_EABI__ */
