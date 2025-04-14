/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                         'e r r o r _ c h e c k s . h'
 *
 *                                      H
 *
 * File 'error_checks.h'                                              by MAR.
 *
 * Macros to check error codes in POSIX functions and to assert
 * conditons.
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

#ifndef	_MARTE_MISC_ERROR_CHECKS_H_
#define _MARTE_MISC_ERROR_CHECKS_H_ 1
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// Macros 'CHK' and 'CHK_INFO'
//
// Error check for POSIX functions that do NOT modify "errno"
// ("modern" POSIX functions). Functions that return zero if
// successful; otherwise, they return an error number to indicate the
// error.
#define CHK(p) { int ret;                                      \
                 if ((ret = p)) {			       \
                   printe ("Error:"#p":%s\n", strerror (ret)); \
                   exit (-1);                                  \
                 }                                             \
               }

#define CHK_INFO(p) { int ret;                                      \
                      if ((ret = p)) {                              \
                        printe ("Error:"#p":%s\n", strerror (ret)); \
                      }                                             \
                    }

// Macros 'CHKE' and 'CHKE_INFO'
//
// Error check for POSIX functions that modify "errno". Functions that
// return zero or a positive value if successful; otherwise, if an
// error occurs, they return the value -1.
#define CHKE(p) {if ((p)==-1) {perror (#p); exit (-1);}}
#define CHKE_INFO(p) {if ((p)==-1) {perror (#p);}}

// Macros 'ASSERT' AND 'ASSERT_INFO'.
//
// They are enabled when 'ENABLE_ASSERTIONS' is defined
#ifdef ENABLE_ASSERTIONS

// When the condition (c) is false the message (m) is displayed
#define ASSERT(c,m) {if (!(c)) \
                      {printe("ASSERT:("#c") failed. "m"\n"); exit (-1);}}
#define ASSERT_INFO(c,m) {if (!(c)) \
                           {printe("ASSERT:("#c") failed. "m"\n");}}

#else

#define ASSERT(c,m)
#define ASSERT_INFO(c,m)

#endif


#endif // _MARTE_MISC_ERROR_CHECKS_H_
