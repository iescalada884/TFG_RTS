/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                's e t j m p'
 *
 *                                      H
 *
 * File 'setjmp.h'                                                    by MAR.
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
 *--------------------------------------------------------------------------*/

#ifndef	_MARTE_SETJMP_H_
#define _MARTE_SETJMP_H_

#include <sys/cpp_macros.h>

CPP_BEGIN_DECLS

typedef void *jmp_buf;
//typedef marte_jmp_buf_t jmp_buf; XXX

void longjmp(jmp_buf env, int val);
int setjmp(jmp_buf env);
// POSIX: It is unspecified whether longjmp() restores the signal
// mask, leaves the signal mask unchanged, or restores it to its value
// at the time setjmp() was called.

#define _longjmp longjmp
#define _setjmp  setjmp
// POSIX: The _longjmp() and _setjmp() functions shall be equivalent
// to longjmp() and setjmp(), respectively, with the additional
// restriction that _longjmp() and _setjmp() shall not manipulate the
// signal mask.

CPP_END_DECLS
#endif /* _MARTE_SETJMP_H_ */
