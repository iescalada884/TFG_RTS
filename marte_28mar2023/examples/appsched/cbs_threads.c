/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                            'c b s _ t h r e a d s'
 *
 *                                      C
 *
 * File 'cbs_threads.c'                                                by MAR.
 *
 * Uses the Application-Defined Scheduling Interface to implement an
 * EDF/CBS scheduler. In this example, along with the scheduler, an
 * EDF thread and a CBS thread are created. The scheduler thread code
 * is in 'cbs_sched.c'.
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

#include "cbs_sched.h"

/* EDF thread */
void * periodic_edf_thread (void * arg)
{ 
  float amount_of_work = *(float *) arg; 
  int i;
  while (1) { 
    for (i=0; i<5; i++) {
      eat (amount_of_work/5);
      printf (" EDF%1d ", i);
    }
    posix_appsched_invoke_scheduler (0);
  }
}

/* CBS thread */
void * cbs_thread (void *arg)
{ 
  float amount_of_work = *(float *) arg; 
  int i;
  
  while (1) { 
    for (i=0; i<5; i++) {
      eat (amount_of_work/5);
      printf (" CBS%1d ", i);
    }
    posix_appsched_invoke_scheduler (0);
  }
}

/* trigger CBS thread */
/* This thread sends signals that are caught by the scheduler and
   interpreted as new jobs requirements for the CBS thread. */
void * trigger_cbs_thread (void *arg)
{
  struct timespec ts = {1, 900000000};
  // Produce a CBS job each 'ts' seconds
  while (1) {
    sigqueue (0, NEW_JOB_SIGNAL, (*(union sigval*)arg));
    nanosleep (&ts, NULL);
  }
  return NULL;
}

#define MAIN_PRIO 5
int main ()
{
  pthread_attr_t attr;
  struct sched_param param;
  struct cbs_sched_param user_param;
  pthread_t sched, t2, t3, t4;
  float load;

  adjust (); // from <misc/load.h>

  /* Set main task base priority */
  param.sched_priority = MAIN_PRIO;
  CHK( pthread_setschedparam (pthread_self (), SCHED_FIFO, &param) );

  /* Creation of the scheduler thread */
  pthread_attr_init (&attr);
  pthread_attr_setschedpolicy (&attr, SCHED_FIFO);
  pthread_attr_setappschedulerstate (&attr, PTHREAD_APPSCHEDULER); 
  param.sched_priority = MAIN_PRIO - 1;
  pthread_attr_setschedparam (&attr, &param);
  CHK_FATAL( pthread_create (&sched, &attr, edf_scheduler, NULL) );
  pthread_attr_destroy(&attr);

  /* Creation of one scheduled thread */
  pthread_attr_init (&attr);
  pthread_attr_setschedpolicy (&attr, SCHED_APP);
  pthread_attr_setappschedulerstate (&attr, PTHREAD_REGULAR); 
  CHK( pthread_attr_setappscheduler (&attr, sched) );
  user_param.period.tv_sec  = 2;  // period = 2s
  user_param.period.tv_nsec = 0;
  user_param.deadline.tv_sec  = 2;  // deadline = 2s
  user_param.deadline.tv_nsec = 0;
  user_param.cbs = 0;
  CHK( pthread_attr_setappschedparam (&attr, &user_param, 
				      sizeof (user_param)) );
  param.sched_priority = MAIN_PRIO - 1;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  load = 1.0; // Execution time = 1s
  CHK_FATAL( pthread_create (&t2, &attr, periodic_edf_thread, &load) );

  /* Creation of one cbs thread */
  pthread_attr_init (&attr);
  pthread_attr_setschedpolicy (&attr, SCHED_APP);
  pthread_attr_setappschedulerstate (&attr, PTHREAD_REGULAR); 
  CHK( pthread_attr_setappscheduler (&attr, sched) );
  user_param.period.tv_sec  = 1;  // period = 1s
  user_param.period.tv_nsec = 0;
  user_param.deadline.tv_sec  = 1;  // deadline = 1s
  user_param.deadline.tv_nsec = 0;
  user_param.cbs = 1;
  user_param.mx_budget.tv_sec = 0; // budget = 0.5s
  user_param.mx_budget.tv_nsec = 500000000;
  CHK( pthread_attr_setappschedparam (&attr, &user_param, 
				      sizeof (user_param)) );
  param.sched_priority = MAIN_PRIO - 1;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  load = 0.6;  // Execution time = 0.6s
  CHK_FATAL( pthread_create (&t3, &attr, cbs_thread, &load) );

  /* Create the cbs trigger thread */
  pthread_attr_init (&attr);
  pthread_attr_setschedpolicy (&attr, SCHED_FIFO);
  param.sched_priority = MAIN_PRIO;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK_FATAL( pthread_create (&t4, &attr, trigger_cbs_thread, &t3) );

  CHK( pthread_join (sched, NULL) );

  return 0;
}
