/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                             'g e t i t i m e r'
 *
 *                                      C
 *
 * File 'getitimer_c.c'                                               by MAR.
 *
 * Function 'getitimer()'. Implemented using timers based on MONOTONIC_CLOCK.
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
#include <sys/time.h>
#include <time.h>
#include <errno.h>
#include <stdio.h>
#include <misc/error_checks.h>

#define INDEX(w)  (w-ITIMER_REAL)
//  Assumes ITIMER_REAL, ITIMER_VIRTUAL and ITIMER_PROF are three
//  consecutive values.

extern timer_t timerid[3]; // defined in 'setitimer_c.c'

/*-----------------*
 *--  getitimer  --*
 *-----------------*/
int getitimer(int which, struct itimerval *value)
{
  struct itimerspec value_ts;

  // Error check
  if ((INDEX(which) < 0) || (INDEX(which) > 2)) {
    errno = EINVAL;
    return -1;
  }
  if (!value) {
    errno = EFAULT;
    return -1;
  }
  errno = 0;

  if (!timerid[INDEX(which)]) {
    // timer hasn't been programmed by application yet
    value->it_value.tv_sec     = 0;
    value->it_value.tv_usec    = 0;
    value->it_interval.tv_sec  = 0;
    value->it_interval.tv_usec = 0;
  } else {
    CHKE( timer_gettime (timerid[INDEX(which)], &value_ts) );
    value->it_value.tv_sec     = value_ts.it_value.tv_sec;
    value->it_value.tv_usec    = value_ts.it_value.tv_nsec / 1000;
    value->it_interval.tv_sec  = value_ts.it_interval.tv_sec;
    value->it_interval.tv_usec = value_ts.it_interval.tv_nsec / 1000;
  }
  return 0;
}
