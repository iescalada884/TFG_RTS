/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                           'p c p _ t h r e a d s'
 *
 *                                      C
 *
 * File 'pcp_threads.c'                                                by MAR.
 *
 * Uses the Application-Defined Scheduling Interface to implement
 * Priority Ceiling Protocol mutexes. The scheduler thread is in
 * 'pcp_sched.c'.
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
#include <sched.h>
#include <malloc.h>
#include <misc/load.h>
#include <misc/error_checks.h>

#include "pcp_sched.h"

/* Two different situations taken from paper: L. Sha, R. Rajkumar, and
 * J. P. Lehoczky, "Priority Inheritance Protocols: An Approach to
 * Real-Time Synchronization". In IEEE Transactions on Computers,
 * vol. 39, pp. 1175-1185, Sep. 1990. */
#define SHA_RAJ_LEH_FIG1
//#define SHA_RAJ_LEH_FIG2


#ifdef SHA_RAJ_LEH_FIG1
/*
 * PCP mutexes
 */
pthread_mutex_t mutex0, mutex1, mutex2;
/*
 * pthread_body1
 */
void * pthread_body1 (void * arg)
{
  struct timespec ts = {5, 0};

  nanosleep (&ts, NULL);
  eat (2.0);
  CHK( pthread_mutex_lock (&mutex0) );
  eat (1.0);
  CHK( pthread_mutex_unlock (&mutex0) );
  eat (1.0);

  return NULL;
}

/*
 * pthread_body2
 */
void * pthread_body2 (void * arg)
{
  struct timespec ts = {2, 0};

  nanosleep (&ts, NULL);
  eat (2.0);
  CHK( pthread_mutex_lock (&mutex1) );
  eat (1.0);
  CHK( pthread_mutex_lock (&mutex2) );
  eat (2.0);
  CHK( pthread_mutex_unlock (&mutex2) );
  eat (1.0);
  CHK( pthread_mutex_unlock (&mutex1) );
  eat (1.0);

  return NULL;
}

/*
 * pthread_body3
 */
void * pthread_body3 (void * arg)
{
  eat (1.0);
  CHK( pthread_mutex_lock (&mutex2) );
  eat (3.0);
  CHK( pthread_mutex_lock (&mutex1) );
  eat (1.0);
  CHK( pthread_mutex_unlock (&mutex1) );
  eat (2.0);
  CHK( pthread_mutex_unlock (&mutex2) );
  eat (2.0);

  return NULL;
}
#endif /* SHA_RAJ_LEH_FIG1 */

#ifdef SHA_RAJ_LEH_FIG2
/*
 * PCP mutexes
 */
pthread_mutex_t mutex0, mutex1, mutex2;
/*
 * pthread_body1
 */
void * pthread_body1 (void * arg)
{
  struct timespec ts = {8, 0};

  nanosleep (&ts, NULL);
  eat (2.0);
  CHK( pthread_mutex_lock (&mutex0) );
  eat (1.0);
  CHK( pthread_mutex_unlock (&mutex0) );
  eat (1.0);
  CHK( pthread_mutex_lock (&mutex1) );
  eat (1.0);
  CHK( pthread_mutex_unlock (&mutex1) );
  eat (1.0);

  return NULL;
}

/*
 * pthread_body2
 */
void * pthread_body2 (void * arg)
{
  struct timespec ts = {3, 0};

  nanosleep (&ts, NULL);
  eat (2.0);
  CHK( pthread_mutex_lock (&mutex2) );
  eat (2.0);
  CHK( pthread_mutex_unlock (&mutex2) );
  eat (1.0);

  return NULL;
}

/*
 * pthread_body3
 */
void * pthread_body3 (void * arg)
{
  eat (2.0);
  CHK( pthread_mutex_lock (&mutex2) );
  eat (2.0);
  CHK( pthread_mutex_lock (&mutex1) );
  eat (4.0);
  CHK( pthread_mutex_unlock (&mutex1) );
  eat (2.0);
  CHK( pthread_mutex_unlock (&mutex2) );
  eat (2.0);

  return NULL;
}
#endif /* SHA_RAJ_LEH_FIG2 */

int main () 
{
  struct sched_param param;
  pthread_attr_t attr;
  pthread_t thread, scheduler_thread;

  pthread_mutexattr_t mutex_attr;
  struct pcp_mutex_specific_param mutex_param;

  adjust ();

  // Create scheduler thread
  CHK( pthread_attr_init (&attr) );

  CHK( pthread_attr_setappschedulerstate (&attr, PTHREAD_APPSCHEDULER) );

  CHK( pthread_attr_setschedpolicy (&attr, SCHED_FIFO) );
  param.sched_priority = 28;
  CHK( pthread_attr_setschedparam (&attr, &param) );

  CHK_FATAL( pthread_create (&scheduler_thread, &attr, pcp_scheduler, NULL) );

  // Create application-scheduling mutexes
  CHK( pthread_mutexattr_init (&mutex_attr) );

  CHK( pthread_mutexattr_setprotocol (&mutex_attr, 
				      PTHREAD_APPSCHED_PROTOCOL) );

  CHK( pthread_mutexattr_setappscheduler (&mutex_attr, scheduler_thread) );
#ifdef SHA_RAJ_LEH_FIG1
  mutex_param.ceiling = 26;
  CHK( pthread_mutexattr_setappschedparam (&mutex_attr, &mutex_param,
					   sizeof(mutex_param)) );

  CHK( pthread_mutex_init (&mutex0, &mutex_attr) );

  mutex_param.ceiling = 24;
  CHK( pthread_mutexattr_setappschedparam (&mutex_attr, &mutex_param,
					   sizeof(mutex_param)) );

  CHK( pthread_mutex_init (&mutex1, &mutex_attr) );

  CHK( pthread_mutex_init (&mutex2, &mutex_attr) );
#endif /* SHA_RAJ_LEH_FIG1 */
#ifdef SHA_RAJ_LEH_FIG2
  mutex_param.ceiling = 26;
  CHK( pthread_mutexattr_setappschedparam (&mutex_attr, &mutex_param,
					   sizeof(mutex_param)) );

  CHK( pthread_mutex_init (&mutex0, &mutex_attr) );

  CHK( pthread_mutex_init (&mutex1, &mutex_attr) );

  mutex_param.ceiling = 24;
  CHK( pthread_mutexattr_setappschedparam (&mutex_attr, &mutex_param,
					   sizeof(mutex_param)) );

  CHK( pthread_mutex_init (&mutex2, &mutex_attr) );
#endif /* SHA_RAJ_LEH_FIG2 */
 

  // Create scheduled threads
  CHK( pthread_attr_destroy (&attr) );
  CHK( pthread_attr_init (&attr) );

  CHK( pthread_attr_setappschedulerstate (&attr, PTHREAD_REGULAR) );

  CHK( pthread_attr_setschedpolicy (&attr, SCHED_APP) );

  CHK( pthread_attr_setappscheduler (&attr, scheduler_thread) );

  CHK( pthread_attr_setappschedparam (&attr, NULL, 0) );

  param.sched_priority = 26;
  CHK( pthread_attr_setschedparam (&attr, &param) );

  CHK( pthread_create (&thread, &attr, pthread_body1, NULL) );

  param.sched_priority = 24;
  CHK( pthread_attr_setschedparam (&attr, &param) );

  CHK( pthread_create (&thread, &attr, pthread_body2, NULL) );

  param.sched_priority = 22;
  CHK( pthread_attr_setschedparam (&attr, &param) );

  CHK( pthread_create (&thread, &attr, pthread_body3, NULL) );

  CHK( pthread_join (scheduler_thread, NULL) );

  return 0;
}
