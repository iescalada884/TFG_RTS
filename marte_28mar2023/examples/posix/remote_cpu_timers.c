/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                     'r e m o t e _ c p u _ t i m e r s'
 *
 *                                      C
 *
 * File 'remote_cpu_timers.c'                                        by MAR.
 *
 * Test CPU timers created in a different thread.
 *
 * The main thread creates two threads and a periodic CPU time timer
 * for each (only one thread if JUST_ONE_THREAD is defined). The main
 * thread wait for timer's signals (SIGUSR1 and SIGUSR2) and counts
 * them.
 *
 * Each thread executes for a while (EXEC_TIME) and then sleeps
 * (SLEEP_TIME). With this behavior the test is more complete, since
 * there are intervals in which only the idle thread is active.
 *
 * The timer(s) are set to send a signal each TIMER_PERIOD_NS
 * nanoseconds.
 *
 * 
 * Test de los timers de CPU creados por otra tarea
 *
 * Desde la thread principal se crean dos threads y se programa un
 * timer periodico de tiempo de CPU para cada una (sólo un thread si
 * se define JUST_ONE_THREAD). La tarea principal espera las señales
 * de los timers (SIGUSR1 y SIGUSR2) y cuenta las que se producen.
 *
 * Cada tarea ejecuta por un rato (EXEC_TIME) y luego se duerme
 * (SLEEP_TIME). Con este comportamiento el test es más completo, ya
 * que de habra intervalos en los que ejecuta la idle thread.
 *
 * El o los timers se programan para enviar una señal cada
 * TIMER_PERIOD_NS nanosegundos.
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

#include <stdio.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
//#include <debug_marte.h>  // for debugging in x86 arch
#include <misc/load.h>
#include <misc/timespec_operations.h>

//#define JUST_ONE_THREAD 1

#define NUM_OF_SIGNALS_WAITED 7
#define EXEC_TIME 0.06 // in secs
#define SLEEP_TIME 1.0 // in secs
#define TIMER_PERIOD_NS 30000000 // In nsec.
#define TIMER_PERIODS_BY_EXEC ((int)(EXEC_TIME/  \
                               ((float)TIMER_PERIOD_NS/1000000000)))

#define ERROR(s) {perror (s); exit(1);}

/* thread_body */
void * thread_body (void *arg)
{
  struct timespec sleep_time = {(int)SLEEP_TIME, 
				(int)((SLEEP_TIME -
				       (int)SLEEP_TIME)*1000000000)};
  if (clock_gettime (CLOCK_THREAD_CPUTIME_ID, (struct timespec *)arg))
    perror ("clock_gettime (CPU)");
  while (1) {
    eat (EXEC_TIME);
    nanosleep (&sleep_time, NULL);
  }
}

/* main */
int main()
{
  pthread_t t1, t2;
  timer_t timer1, timer2;
  clockid_t clock1, clock2;
  sigset_t set, set_wait;
  int received_sig;
  int sig_counter_1, sig_counter_2;
  struct sigevent event;
  struct timespec tpini, tpend;
  struct timespec cputini1, cputend1, cputini2, cputend2;
  struct itimerspec its = {{0, TIMER_PERIOD_NS}, {0, TIMER_PERIOD_NS}};
  int policy;
  struct sched_param param;
 
  printf ("\nAdjusting 'eat()' function...\n\n");
  adjust (); 
  printf ("Test starts...\n");
 
  // Block all signals
  if (sigfillset (&set))
    ERROR ("error in sigemptyset\n");
  if (pthread_sigmask (SIG_BLOCK, &set, NULL))
    ERROR ("error in pthread_sigmask");
  // Configure the set of signals to wait (SIGUSR1 and SIGUSR2)
  if (sigemptyset (&set_wait))
    ERROR ("error in sigemptyset");
  if (sigaddset (&set_wait, SIGUSR1))
    ERROR ("error in sigaddset\n");
  if (sigaddset (&set_wait, SIGUSR2)) 
    ERROR ("error in sigaddset\n");

  // Raise main priority
  if (pthread_getschedparam (pthread_self (), &policy, &param))
    ERROR ("pthread_getschedparam");
  param.sched_priority = 20;
  if (pthread_setschedparam (pthread_self (), policy, &param))
    ERROR ("pthread_setschedparam");
  


  // Debug
  //init_serial_communication_with_gdb (SERIAL_PORT_1);
  //set_break_point_here;

  // Create first task and its asociated CPU timer
  if (pthread_create (&t1, NULL, thread_body,  &cputini1))
    ERROR("error in pthread_create 1");
  event.sigev_signo = SIGUSR1;
  event.sigev_notify = SIGEV_SIGNAL;
  event.sigev_value.sival_int = 0;
  if (pthread_getcpuclockid (t1, &clock1))
    ERROR ("pthread_getcpuclockid 1");
  if (timer_create (clock1, &event, &timer1))
    ERROR ("timer_create 1");
  if (timer_settime (timer1, 0, &its, NULL))
    ERROR ("timer_settime 1");

#ifndef JUST_ONE_THREAD
  // Create second task and its asociated CPU timer
  if (pthread_create (&t2, NULL, thread_body, &cputini2))
    ERROR("error in pthread_create 2");
  event.sigev_signo = SIGUSR2;
  event.sigev_notify = SIGEV_SIGNAL;
  event.sigev_value.sival_int = 0;
  if (pthread_getcpuclockid (t2, &clock2))
    ERROR ("pthread_getcpuclockid 2");
  if (timer_create (clock2, &event, &timer2))
    ERROR ("timer_create 2");
  if (timer_settime (timer2, 0, &its, NULL))
    ERROR ("timer_settime 2");
#endif // JUST_ONE_THREAD

  if (clock_gettime (CLOCK_REALTIME, &tpini))
    ERROR ("error in clock_gettime\n");
  //printf ("t1: %ds, %dns\n", tpini.tv_sec, tpini.tv_nsec);

  // Wait for a number of signals
  sig_counter_1 = 0;
  sig_counter_2 = 0;
  do {
    if (sigwait (&set_wait, &received_sig))
      ERROR ("sigwait");
    if (received_sig == SIGUSR1)
      sig_counter_1++;
    else if (received_sig == SIGUSR2)
      sig_counter_2++;
    else
      ERROR ("Unexpected signal!!");
  } while (sig_counter_1 + sig_counter_2 < NUM_OF_SIGNALS_WAITED);


  if (clock_gettime (CLOCK_REALTIME, &tpend))
    ERROR("error in clock_gettime");

  // Show results
  printf("Test ends. Received %d SIGUSR1 and %d SIGUSR2.\n", 
	 sig_counter_1, sig_counter_2);
  // printf ("t1: %ds, %dns", tpini.tv_sec, tpini.tv_nsec);
  // printf ("    t2: %ds, %dns\n", tpend.tv_sec, tpend.tv_nsec);
  printf ("\ntini:%s", show_timespec_s (&tpini));
  printf ("  tend:%s", show_timespec_s (&tpend));
  decr_timespec (&tpend, &tpini);
  printf ("  dif:%s\n", show_timespec_s (&tpend));


  // CPU time
  if (clock_gettime (clock1, &cputend1))
    ERROR("clock_gettime 1");
  printf ("\ncputini1:%s", show_timespec_s (&cputini1));
  printf ("  cputend1:%s", show_timespec_s (&cputend1));
  decr_timespec (&cputend1, &cputini1);
  printf ("  dif1:%s\n", show_timespec_s (&cputend1));
#ifndef JUST_ONE_THREAD
  if (clock_gettime (clock2, &cputend2))
    ERROR("clock_gettime 2");
  printf ("cputini2:%s", show_timespec_s (&cputini2));
  printf ("  cputend2:%s", show_timespec_s (&cputend2));
  decr_timespec (&cputend2, &cputini2);
  printf ("  dif2:%s\n", show_timespec_s (&cputend2));
#endif // JUST_ONE_THREAD
  printf ("\nExpected total CPU time consumed:%ldns", 
	  (long)NUM_OF_SIGNALS_WAITED*TIMER_PERIOD_NS);
  printf (" (Compare with dif1 or dif1+dif2)\n");

  printf ("\n Slight differences in times can be due to 'eat()' function adjustment.\n");

  
  exit (0);
}
