/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                            'e d f _ t h r e a d s'
 *
 *                                      C
 *
 * File 'edf_threads.c'                                                by MAR.
 *
 * Uses the Application-Defined Scheduling Interface to implement a
 * simple EDF scheduler. Create an EDF scheduler and two scheduled
 * threads. The scheduler thread code is in 'edf_sched.c'.
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
#include <unistd.h>
#include <time.h>
#include <pthread.h>
#include <sched.h>
#include <misc/load.h>
#include <misc/error_checks.h>
//#include <debug_marte.h> // debug

#include "edf_sched.h"

//#include <debug_marte.h>

/*  Scheduled thread */
void * periodic (void * arg)
{
  float amount_of_work = *(float *) arg;
  while (1) {
    /* do useful work */
    eat (amount_of_work);
    posix_appsched_invoke_scheduler (0);
  }
}

#define MAIN_PRIO 5
int main ()
{
  pthread_attr_t attr;
  struct sched_param param;
  struct edf_sched_param user_param;
  pthread_t sched, t1, t2;
  float load1, load2;

  adjust ();

  // debug
  // init_serial_communication_with_gdb (SERIAL_PORT_1);
  // set_break_point_here;

  /* Set main task base priority */
  param.sched_priority = MAIN_PRIO;
  CHK( pthread_setschedparam (pthread_self (), SCHED_FIFO, &param) );
  
  /* Creation of the scheduler thread */
  pthread_attr_init (&attr);
  pthread_attr_setschedpolicy (&attr, SCHED_FIFO);
  pthread_attr_setappschedulerstate (&attr, PTHREAD_APPSCHEDULER); 
  param.sched_priority = MAIN_PRIO - 1;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&sched, &attr, edf_scheduler, NULL) );
  pthread_attr_destroy(&attr);

  /* Creation of one scheduled thread */
  pthread_attr_init (&attr);
  pthread_attr_setschedpolicy (&attr, SCHED_APP);
  CHK( pthread_attr_setappscheduler (&attr, sched) );
  user_param.period.tv_sec  = 2;  // period = 2s
  user_param.period.tv_nsec = 0;
  load1 = 0.5; // load = 1.0s
  CHK( pthread_attr_setappschedparam (&attr, &user_param, 
				      sizeof(user_param)) );
  param.sched_priority = MAIN_PRIO - 3;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&t1, &attr, periodic, &load1) );

  /* Creation of other scheduled thread */
  pthread_attr_init (&attr);
  pthread_attr_setschedpolicy (&attr, SCHED_APP);
  CHK( pthread_attr_setappscheduler (&attr, sched) );
  user_param.period.tv_sec  = 3;  // period = 5.0s
  user_param.period.tv_nsec = 0;
  load2 = 1.5; // load = 1.5s
  CHK( pthread_attr_setappschedparam (&attr, &user_param, 
				      sizeof(user_param)) );
  param.sched_priority = MAIN_PRIO - 1;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&t2, &attr, periodic, &load2) );
  
  // annoying activations
  /*for (i=0; i<15; i++) {
    struct timespec ts = {0, 700000000};
    nanosleep (&ts, NULL);
    eat (0.3);
    }*/

  CHK( pthread_join (sched, NULL) );

  return 0;
}
