/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.9 2014-08-21
 *
 *                    't i m e s p e c _ o p e r a t i o n s'
 *
 *                                      H
 *
 * File 'timespec_operations.h'                                        by MAR.
 *
 * Some basic operations with the type 'timespec'.
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
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

#ifndef _MARTE_MISC_TIMESPEC_OPERATIONS_H_
#define _MARTE_MISC_TIMESPEC_OPERATIONS_H_

#include <stdio.h>
#include <time.h>

CPP_BEGIN_DECLS // AL

static inline int smaller_timespec (const struct timespec *t1,
                                    const struct timespec *t2)
{
        return t1->tv_sec < t2->tv_sec ||
               (t1->tv_sec == t2->tv_sec && t1->tv_nsec < t2->tv_nsec);
}

static inline int smaller_or_equal_timespec (const struct timespec *t1,
                                             const struct timespec *t2)
{
        return t1->tv_sec < t2->tv_sec || (t1->tv_sec == t2->tv_sec &&
                        t1->tv_nsec <= t2->tv_nsec);
}

static inline void incr_timespec (struct timespec *t1, const struct timespec *t2)
{
        t1->tv_sec += t2->tv_sec;
        t1->tv_nsec += t2->tv_nsec;
        if (t1->tv_nsec >= 1000000000) {
                t1->tv_sec ++;
                t1->tv_nsec -= 1000000000;
        }
}

static inline void decr_timespec (struct timespec *t1, const struct timespec *t2)
{
        if (t1->tv_nsec < t2->tv_nsec) {
                t1->tv_sec -= t2->tv_sec + 1;
                t1->tv_nsec = t1->tv_nsec + 1000000000 - t2->tv_nsec;
        } else {
                t1->tv_sec -= t2->tv_sec;
                t1->tv_nsec -= t2->tv_nsec;
        }
}

static inline void  add_timespec (struct timespec *s,
                                  const struct timespec *t1,
                                  const struct timespec*t2)
{
        s->tv_sec  = t1->tv_sec  + t2->tv_sec;
        s->tv_nsec = t1->tv_nsec + t2->tv_nsec;
        if (s->tv_nsec >= 1000000000) {
                s->tv_sec ++;
                s->tv_nsec -= 1000000000;
        }
}

static inline void div_timespec (struct timespec *s,
                                 const struct timespec *dividend,
                                 const struct timespec *divisor)
{
        long long ldividend = dividend->tv_sec*1000000000 + dividend->tv_nsec;
        long long ldivisor  = divisor->tv_sec*1000000000  + divisor->tv_nsec;
        long long result;

        result = ldividend / ldivisor;

        s->tv_sec = result / 1000000000;
        s->tv_nsec = result % 1000000000;
}

static inline void mult_timespec (struct timespec *s,
                                  const struct timespec *t1,
                                  const struct timespec *t2)
{
        long long lt1 = t1->tv_sec*1000000000 + t1->tv_nsec;
        long long lt2 = t2->tv_sec*1000000000 + t2->tv_nsec;
        long long result;

        result = lt1 / lt2;

        s->tv_sec = result / 1000000000;
        s->tv_nsec = result % 1000000000;
}

typedef long long unsigned nanosecs_t;
extern char str_timespec_s[40];
static inline char * show_timespec_s (struct timespec *ts)
{
        nanosecs_t ns = (nanosecs_t)(ts->tv_sec) * 1000000000 + ts->tv_nsec;
        nanosecs_t s = ns / 1000000000;
        nanosecs_t ms = ns % 1000000000;
        nanosecs_t us = ns % 1000000;
        ns = ns % 1000;
        sprintf (str_timespec_s, "%3lus%3lums%3luus%3luns", (unsigned long)s,
                 (unsigned long)(ms / 1000000), (unsigned long)(us / 1000),
                 (unsigned long)ns);
        return str_timespec_s;
}

//--------------------//
// timespec_to_double //
//--------------------//

static inline double timespec_to_double(const struct timespec *time)
{
        return time->tv_nsec*0.000000001 + (double)time->tv_sec;
}

//--------------------//
// double_to_timespec //
//--------------------//

static inline void double_to_timespec(double time, struct timespec *ts)
{
        ts->tv_sec = (long) time;
        ts->tv_nsec = (long)((time - (double)ts->tv_sec) * 1000000000);
}

CPP_END_DECLS // AL

#endif /* _MARTE_MISC_TIMESPEC_OPERATIONS_H_ */
