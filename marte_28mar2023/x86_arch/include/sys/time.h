/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                               's y s / t i m e'
 *
 *                                      H
 *
 * File 'sys/time.h'                                                  by MAR.
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
#ifndef _MARTE_SYS_TIME_H_
#define _MARTE_SYS_TIME_H_

#include <sys/cpp_macros.h>
#include <sys/types.h>
#include <sys/marte_general_constants.h>

CPP_BEGIN_DECLS
struct timeval {
  time_t      tv_sec;
  suseconds_t tv_usec;
};
# define _STRUCT_TIMEVAL 1

struct itimerval{
  struct timeval it_interval; // Timer interval.
  struct timeval it_value;    // Current value.
};

// Interval timers
#define ITIMER_REAL _MARTE_ITIMER_REAL
//    Decrements in real time.
#define ITIMER_VIRTUAL _MARTE_ITIMER_VIRTUAL
//    Decrements in process virtual time.
#define ITIMER_PROF _MARTE_ITIMER_PROF
//    Decrements both in process virtual time and when the system is
//    running on behalf of the process.
int   getitimer(int, struct itimerval *value);
int   setitimer(int, const struct itimerval * value,
		struct itimerval * ovalue);

int   gettimeofday(struct timeval *, void *);
CPP_END_DECLS
#endif /* _MARTE_SYS_TIME_H_ */
