/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                            'l i b m _ n e e d s'
 *
 *                                      C
 *
 * File 'libm_needs.c'                                                 by Mar.
 *
 * Miscellany of functions needed for the standrad Linux 'libm'
 * (glibc-2.1.1).
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/
#include <errno.h>
#include <unistd.h>

//-------------------------------------------------------------------------
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */
/*
 * isnan(x) returns 1 is x is nan, else 0;
 * no branching!
 */
typedef unsigned int u_int32_t;
typedef int int32_t;
typedef union
{
  double value;
  struct
  {
    u_int32_t lsw;
    u_int32_t msw;
  } parts;
} ieee_double_shape_type;

#define EXTRACT_WORDS(ix0,ix1,d)                                \
do {                                                            \
  ieee_double_shape_type ew_u;                                  \
  ew_u.value = (d);                                             \
  (ix0) = ew_u.parts.msw;                                       \
  (ix1) = ew_u.parts.lsw;                                       \
} while (0)

int __isnan(double x)
{
        int32_t hx,lx;
        EXTRACT_WORDS(hx,lx,x);
        hx &= 0x7fffffff;
        hx |= (u_int32_t)(lx|(-lx))>>31;
        hx = 0x7ff00000 - hx;
        return (int)((u_int32_t)(hx))>>31;
}

/*
 * isnanl(x) returns 1 is x is nan, else 0;
 * no branching!
 */
typedef union
{
  long double value;
  struct
  {
    u_int32_t lsw;
    u_int32_t msw;
    unsigned int sign_exponent:16;
    unsigned int empty:16;
  } parts;
} ieee_long_double_shape_type;

#define GET_LDOUBLE_WORDS(exp,ix0,ix1,d)                        \
do {                                                            \
  ieee_long_double_shape_type ew_u;                             \
  ew_u.value = (d);                                             \
  (exp) = ew_u.parts.sign_exponent;                             \
  (ix0) = ew_u.parts.msw;                                       \
  (ix1) = ew_u.parts.lsw;                                       \
} while (0)

int __isnanl(long double x)
{
        int32_t se,hx,lx;
        GET_LDOUBLE_WORDS(se,hx,lx,x);
        se = (se & 0x7fff) << 1;
        lx |= hx & 0x7fffffff;
        se |= (u_int32_t)(lx|(-lx))>>31;
        se = 0xfffe - se;
        return (int)((u_int32_t)(se))>>16;
}

//-------------------------------------------------------------------------
/* Copyright (C) 1991, 1994, 1995, 1996, 1998 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */
void
__assert_fail (const char *assertion, const char *file, unsigned int line,
               const char *function)
{

  /* Print the message.  */
  (void) printf ("%s:%u: %s%sAssertion `%s' failed.\n",
                  file, line,
                  function ? function : "", function ? ": " : "",
                  assertion);

  //abort ();
}

extern int *__errno_location (void)
{
  return pthread_errno();
}

//-------------------------------------------------------------------------
