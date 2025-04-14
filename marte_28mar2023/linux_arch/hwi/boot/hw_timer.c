/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                              'h w _ t i m e r'
 *
 *                                      C
 *
 * File 'hw_timer.c'                            By Miguel Ángel Masmano Tello
 *                                                 and MAR.
 *
 * This file simulates the clock hardware interruption using timers
 * and signals.
 *
 * In order to achieve better performance high resolution timers
 * patch can be used.
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
 * ----------------------------------------------------------------------
 * The idea of running MaRTE OS as a Linux process is due to Miguel Angel
 * Masmano     Tello     (Universidad    Politecnica     de     Valencia)
 * <mimastel@doctor.upv.es>.
 * He  is  also  the  author  of   most  of  the  code  involved  in  the
 * implementation of the hardware interface for this architecture.
 *
 *--------------------------------------------------------------------------*/

//#define PRN_DBG(p) p;  // print debug messages on console
#define PRN_DBG(p) {}

//#define CONFIG_USE_HIGH_RESOLTION_TIMERS
#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
#define _POSIX_TIMERS
#define USED_CLOCK CLOCK_MONOTONIC_HR

#include "posix_time.h"

// #define SIG_TIMER SIGUSR2 now in "sig_timer.h"

#else

// #define SIG_TIMER SIGALRM now in "sig_timer.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include "asm-i386/msr.h"
#include "asm-i386/arch_time.h"

extern int linux_setitimer(int which, const struct itimerval *value,
			   struct itimerval *oldvalue);
extern int linux_open(const char *camino, int flags);
extern int linux_close(int fd);
extern int linux_read(int fd, void *buf, int nbytes);

#endif

#include <stdio.h>
#include <signal.h>
#include <time.h>
#include "marte_functions.h"

/*----------------*
 *--  hwtime_t  --*
 *----------------*/
typedef long long unsigned hwtime_t;


/*--------------------------------------*
 *--  hw_timer and hwtimer_hwtime_Hz  --*
 *--------------------------------------*/
#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
/* hw_timer is the POSIX timer which emulates the HW timer */
static  timer_t hw_timer;
const hwtime_t hw_timer_hwtime_Hz = NSECS_Per_SEC; // hwtime is a count of
                                                   // nanosecons
#else
hwtime_t hw_timer_hwtime_Hz; // set in 'hw_timer_init ()'
#endif


#ifndef CONFIG_USE_HIGH_RESOLTION_TIMERS

/*------------------------*
 *-- get_processor_freq --*
 *------------------------*/
static hwtime_t get_processor_freq (void) {
  int procfd;
  float cpu_freq = 0;

  /* Get the processor frequency.  */
  procfd = linux_open ("/proc/cpuinfo", O_RDONLY);
  if (procfd != -1) {
    char buf[2000];
    ssize_t n;

    if ((n = linux_read (procfd, buf, sizeof (buf))) > 0) {
      char *cpuhz;

      buf[sizeof (buf) - 1] = '\0';
      cpuhz = (char *) strstr (buf, "cpu MHz");
      if (cpuhz != NULL) {
	cpuhz += 7;

	while (! isdigit (*cpuhz))
	  ++cpuhz;

	sscanf (cpuhz, "%f", &cpu_freq);
      }
    }

    linux_close (procfd);

    return (hwtime_t) (cpu_freq * USECS_PER_SEC);
  }
  return 0;
}

#endif


/*---------------------------*
 *--  hw_timer_get_hwtime  --*
 *---------------------------*/
hwtime_t hw_timer_get_hwtime (void) {
#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
  struct timespec clock_time;

  linux_clock_gettime (USED_CLOCK, &clock_time);
  return timespec_to_ns (&clock_time);
#else
  hwtime_t tsc_time;
  rdtscll (tsc_time);
  return tsc_time;
#endif
}

/*----------------------------*
 *-- get_hwtimer_resolution --*
 *----------------------------*/
static hwtime_t get_hwtimer_resolution (void) {
#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
  struct timespec clock_res;
  linux_clock_getres (USED_CLOCK, &clock_res);
  return timespec_to_ns (&clock_res);
#else
  return 10 * NSECS_PER_MSEC; // 10 msec
#endif
}



/*------------------*
 *-- setclockmode --*
 *------------------*/
#define CLOCK_MODE_PERIODIC 1
#define CLOCK_MODE_ONESHOT 2
static int setclockmode (int mode, hwtime_t hwt)
{
  // Convert hwt to timespec
  struct timespec ts;
  ts.tv_sec  = (time_t) (hwt / hw_timer_hwtime_Hz);
  ts.tv_nsec = (long)   (((hwt - ts.tv_sec * hw_timer_hwtime_Hz) *
                          NSECS_PER_SEC) /
                         hw_timer_hwtime_Hz);
  PRN_DBG (printc ("setclockmode: hwt:%ds%ldns(%lld)\n",
		   ts.tv_sec, ts.tv_nsec, hwt));
  PRN_DBG (printc ("              hwtime_Hz:%lld\n", hw_timer_hwtime_Hz));
  PRN_DBG (printc ("              sizeof (hwtime_t):%d\n", sizeof (hwtime_t)));
#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
  struct itimerspec tc = {{0,0},{0,0}};
#else
  struct itimerval ti = {{0, 0}, {0, 0}};
#endif

#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
  switch (mode) {
  case CLOCK_MODE_ONESHOT:
    tc.it_value.tv_sec = ts.tv_sec;
    tc.it_value.tv_nsec = ts.tv_nsec;
    break;
  case CLOCK_MODE_PERIODIC:
    tc.it_value.tv_sec = ts.tv_sec;
    tc.it_value.tv_nsec = ts.tv_nsec;
    tc.it_interval.tv_sec = ts.tv_sec;
    tc.it_interval.tv_nsec = ts.tv_nsec;
    break;
  default:
    return -1;
  }

  if (linux_timer_settime (hw_timer, 0, &tc, NULL)) {
    perror (">>> timer_settime() error");
    return -1;
  }

#else // not using HIGH_RESOLTION_TIMERS

  if (hwt != 0 && ts.tv_nsec < 1000) {
    // value should be unless 1us in order to not disarm the timer
    ts.tv_nsec = 1000;
  }

  switch (mode) {
  case CLOCK_MODE_ONESHOT:
    ti.it_value.tv_sec = ts.tv_sec;
    ti.it_value.tv_usec = (long int) (ts.tv_nsec / 1000);
    break;
  case CLOCK_MODE_PERIODIC:
    ti.it_value.tv_sec = ts.tv_sec;
    ti.it_value.tv_usec = (long int) (ts.tv_nsec / 1000);
    ti.it_interval.tv_sec = ts.tv_sec;
    ti.it_interval.tv_usec = (long int) (ts.tv_nsec / 1000);
    break;
  default:
    return -1;
  }

  PRN_DBG (printc ("linux_setitimer. sec:%d, usec:%ld\n",ti.it_value.tv_sec, ti.it_value.tv_usec));
  if (linux_setitimer (ITIMER_REAL, &ti, NULL)) {
    perror (">>> setitimer () error");
    return -1;
  }
#endif
  return 0;
}


/*----------------------------*
 *-- hw_timer_program_timer --*
 *----------------------------*/
void hw_timer_program_timer (hwtime_t hwt) {
  setclockmode (CLOCK_MODE_ONESHOT, hwt);
}


/*-------------------*
 *-- hw_timer_init --*
 *-------------------*/
// Called from 'hardware_interface.gpb'.
// this function give value to 'hw_timer_hwtime_Hz':
//       - if 'hwtime_hz' is  0:  'hw_timer_hwtime_Hz = get_processor_freq ()'
//       - if 'hwtime_hz' not 0:  'hw_timer_hwtime_Hz = hwtime_Hz'
int hw_timer_init (int hwtime_hz) {
#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
  struct sigevent sigev;
#endif

  printc ("Initializing Virtual HW timer: ");
#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
  printc ("Using High Resoltion patch\n");
#else
  printc ("Using Itimers\n");
  hw_timer_hwtime_Hz = get_processor_freq ();
  if (hw_timer_hwtime_Hz == 0) {
    printe ("Error getting CPU Frequency\n");
    return -1;
  }
  if (hwtime_hz)
    // forget value of 'hw_timer_hwtime_Hz' and assign 'hwtime_hz'
    // instead
    hw_timer_hwtime_Hz = hwtime_hz;

  printc ("   CPU Frequency %llu Hz\n", hw_timer_hwtime_Hz);
#endif
  printc ("   Timer Resolution is %lld nsec\n",
	  (long long) get_hwtimer_resolution ());


#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS
  /* SIGUSR2 will be used as interruption */
  sigev.sigev_notify = SIGEV_SIGNAL;
  sigev.sigev_signo = SIG_TIMER;
  /* Now POSIX timer is created*/
  if (linux_timer_create (USED_CLOCK, &sigev, &hw_timer)) {
    printe (">>> error:timer_create(),\n    Are High-Res-Timers installed in your kernel?\n");
    return -1;
  }

#endif

  // This only if you prefer to use a periodic timer. In this case
  // 'hw_timer_program_timer' shouldn't do anything at all
  // printe ("   Programming Virtual Timer as a periodic timer\n");
  /* HW timer will be initialized as a periodic timer */
  // setclockmode (CLOCK_MODE_PERIODIC, NSECS_PER_MSEC * 1100);

  return 0;
}
