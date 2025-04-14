/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                  't i m e'
 *
 *                                      H
 *
 * File 'time.h'                                                       by MAR.
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
#ifndef _MARTE_TIME_H_
#define _MARTE_TIME_H_

#include <sys/cpp_macros.h>
#include <sys/types.h>
#include <sys/marte_general_constants.h>

CPP_BEGIN_DECLS

struct timespec {
  time_t tv_sec;
  int tv_nsec;
};

struct itimerspec {
  struct timespec it_interval;
  struct timespec it_value;
};

#define CLOCK_REALTIME           _MARTE_CLOCK_REALTIME
#define CLOCK_MONOTONIC          _MARTE_CLOCK_MONOTONIC
#define CLOCK_THREAD_CPUTIME_ID  _MARTE_CLOCK_THREAD_CPUTIME_ID
#define CLOCK_INTERRUPTS_CPUTIME _MARTE_CLOCK_INTERRUPTS_CPUTIME

#define TIMER_ABSTIME           _MARTE_TIMER_ABSTIME

int clock_settime (clockid_t clock_id, const struct timespec *tp);
int clock_gettime (clockid_t clock_id, struct timespec *tp);
int clock_getres (clockid_t clock_id, struct timespec *res);

#include <signal.h>
int timer_create (clockid_t clock_id, struct sigevent *evp, timer_t *timerid);
int timer_delete (timer_t timerid);

int timer_settime (timer_t timerid, int flags,
		   const struct itimerspec *value, struct itimerspec *ovalue);
int timer_gettime (timer_t timerid, struct itimerspec *value);
int timer_getoverrun (timer_t timerid);

int nanosleep (const struct timespec *rqtp, struct timespec *rmtp);
int clock_nanosleep (clockid_t clock_id, int flags,
		     const struct timespec *rqtp, struct timespec *rmtp);

struct tm {
	int	tm_sec;		/* seconds after the minute [0-60] */
	int	tm_min;		/* minutes after the hour [0-59] */
	int	tm_hour;	/* hours since midnight [0-23] */
	int	tm_mday;	/* day of the month [1-31] */
	int	tm_mon;		/* months since January [0-11] */
	int	tm_year;	/* years since 1900 */
	int	tm_wday;	/* days since Sunday [0-6] */
	int	tm_yday;	/* days since January 1 [0-365] */
	int	tm_isdst;	/* Daylight Savings Time flag */
};

time_t     time (time_t *tloc);

char      *asctime (const struct tm *tm);
char      *ctime (const time_t *timep);
struct tm *gmtime (const time_t *clock);
struct tm *localtime (const time_t *clock);
time_t     mktime (struct tm *timeptr);

// not implemented in MaRTE
clock_t clock(void);
double difftime(time_t time1, time_t time0);
size_t strftime(char *s, size_t max, const char *format,
		const struct tm *tm);


CPP_END_DECLS
#endif /* _MARTE_TIME_H_ */
