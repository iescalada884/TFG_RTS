/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                             'e d f _ s c h e d'
 *
 *                                      C
 *
 * File 'edf_sched.c'                                                 by MAR.
 *
 * Uses the Application-Defined Scheduling Interface to implement a
 * simple EDF scheduler.
 *
 * The scheduled threads are periodic with deadline equal to period.
 *
 * For each scheduled thread a periodic timer is programmed which
 * spires each time the activation time is reached.
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
#include <pthread.h>
#include <sched.h>
#include <time.h>
#include <malloc.h>
#include <misc/timespec_operations.h>
#include <sys/marte_appsched_event_codes_str.h>
#include <misc/generic_lists.h>
#include <misc/generic_lists_order.h>
#include <misc/error_checks.h>

#include "edf_sched.h"

typedef enum {ACTIVE, BLOCKED, TIMED} th_state_t;

/* Thread-specific data */
typedef struct thread_data {
  struct thread_data * next;
  th_state_t th_state;
  struct timespec period;
  struct timespec next_deadline; /* absolute time */
  int id;
  timer_t timer_id;
  pthread_t thread_id;
} thread_data_t;

/* Scheduling algorithm data */
list_t RQ = NULL;
int threads_count = 1; // to assign a different id to each thread
thread_data_t *current_thread = NULL; // thread currently chosen to execute
pthread_key_t edf_key;

/* 
 * show_RQ
 */
void show_RQ (struct timespec *start_time)
{
  thread_data_t *th = head (RQ);
  struct timespec ts;

  printf ("    RQ: ");
  if (!th) {
    printf ("EMPTY");
    return;
  }
  while (th) {
    ts = th->next_deadline;
    decr_timespec (&ts, start_time);
    printf ("(id:%d, deadline:%2.1fs) ", th->id,
	    ts.tv_sec + ts.tv_nsec / 1000000000.0);
    th = th->next;
  }
}


/*
 * more_urgent_than
 */
int more_urgent_than (void *left, void *right)
{
  return smaller_timespec (&((thread_data_t *)left)->next_deadline, 
			   &((thread_data_t *)right)->next_deadline);
}

/*
 * schedule_next
 */
void schedule_next (posix_appsched_actions_t *actions)
{
  thread_data_t *most_urgent_thread = head (RQ);

  if (most_urgent_thread != current_thread) {
    
    if (most_urgent_thread != NULL) {
      // Activate next thread
      printf (" Activate:%d ", most_urgent_thread->id);
      CHK(posix_appsched_actions_addactivate (actions, 
					      most_urgent_thread->thread_id));
    }
    if (current_thread != NULL && current_thread->th_state != BLOCKED) {
      // Suspend "old" current thread
      printf (" Suspend:%d ", current_thread->id);
      CHK( posix_appsched_actions_addsuspend (actions, 
					      current_thread->thread_id) );
    }

    current_thread = most_urgent_thread;
  }
}

/*
 * add_to_list_of_threads
 */
void add_to_list_of_threads (pthread_t thread_id,
			     const struct timespec *now,
			     posix_appsched_actions_t *actions)
{
  //struct sched_param th_params;
  struct edf_sched_param param;
  size_t param_size;
  thread_data_t *t_data;
  struct sigevent evp;
  struct itimerspec timer_prog;

  // Get specific scheduling parameters
  CHK( pthread_getappschedparam (thread_id, &param, &param_size) );
  
  // Create and give value to the 't_data' struct
  t_data = (thread_data_t *) malloc (sizeof(thread_data_t));
  t_data->period = param.period;
  t_data->th_state = ACTIVE;
  t_data->id = threads_count++;
  add_timespec (&t_data->next_deadline, now, &t_data->period);
  t_data->thread_id = thread_id;

  evp.sigev_notify          = SIGEV_SIGNAL;
  evp.sigev_signo           = SIGUSR1;
  evp.sigev_value.sival_ptr = t_data;
  CHKE( timer_create (CLOCK_REALTIME, &evp, &t_data->timer_id) );

  // Add to ready queue
  enqueue_in_order (t_data, &RQ, more_urgent_than);

  // Assign thread-specific data
  CHK( pthread_setspecific_for (edf_key, thread_id, t_data) );
  
  // Program periodic timer (period = t_data->period)
  timer_prog.it_value = t_data->next_deadline;
  timer_prog.it_interval = t_data->period;
  CHKE( timer_settime (t_data->timer_id, TIMER_ABSTIME, &timer_prog, NULL) );

  // Accept new thread
  CHK( posix_appsched_actions_addaccept (actions, thread_id) );

  printf ("    Add new thread (id:%d, period:%d.%ds)\n", t_data->id, 
	  t_data->period.tv_sec, t_data->period.tv_nsec);
}

/*
 * eliminate_from_list_of_threads
 */
void eliminate_from_list_of_threads (pthread_t thread_id)
{
  thread_data_t *t_data;
  // get thread-specific data
  CHK( pthread_getspecific_from (edf_key, thread_id, (void **)&t_data) );    
  // delete timer
  CHKE( timer_delete (t_data->timer_id) );
  // Remove from scheduling algorithm lists
  if (t_data->th_state == ACTIVE)
    dequeue (t_data, &RQ);
  // Free used memory
  free (t_data);
}

/*
 * make_ready
 */
void make_ready (pthread_t thread_id, const struct timespec *now)
{
  thread_data_t *t_data;
  struct itimerspec timer_prog;
  // get thread-specific data
  CHK( pthread_getspecific_from (edf_key, thread_id, (void **)&t_data) ); 
   
  t_data->th_state = ACTIVE;

  add_timespec (&t_data->next_deadline, now, &t_data->period);

  // Program periodic timer
  timer_prog.it_value = t_data->next_deadline;
  timer_prog.it_interval = t_data->period;
  timer_settime (t_data->timer_id, TIMER_ABSTIME, &timer_prog, NULL);
}

/*
 * make_blocked
 */
void make_blocked (pthread_t thread_id)
{
  thread_data_t *t_data;
  struct itimerspec null_timer_prog = {{0, 0},{0, 0}};
  // get thread-specific data
  CHK( pthread_getspecific_from (edf_key, thread_id, (void **)&t_data) ); 
   
  t_data->th_state = BLOCKED;
  timer_settime (t_data->timer_id, 0, &null_timer_prog, NULL);
}

/*
 * reached_activation_time
 */
void reached_activation_time (thread_data_t *t_data)
{
  switch (t_data->th_state) {
  case TIMED:
    t_data->th_state = ACTIVE;
    incr_timespec (&t_data->next_deadline, &t_data->period);
    enqueue_in_order (t_data, &RQ, more_urgent_than);
    break;
  case BLOCKED:
    break;
  case ACTIVE:
    // Deadline missed
    printf (" Deadline missed in thread:%d !!", t_data->id);
    incr_timespec (&t_data->next_deadline, &t_data->period);
    break;
  default:
    printf (" Invalid state:%d in thread:%d !!", t_data->th_state, t_data->id);
  }
}

/*
 * make_timed
 */
void make_timed (pthread_t thread_id)
{
  thread_data_t *t_data;
  // get thread-specific data
  CHK( pthread_getspecific_from (edf_key, thread_id, (void **)&t_data) ); 
   
  t_data->th_state = TIMED;

  // remove the thread from the ready queue
  dequeue (t_data, &RQ);  
}

/*
 * EDF scheduler thread
 */
void *edf_scheduler (void *arg)
{
  posix_appsched_actions_t actions;
  struct posix_appsched_event event;
  sigset_t waited_signal_set;
  struct timespec now, start_time, dif_time;
    
  // Initialize the 'waited_signal_set'
  CHKE( sigemptyset (&waited_signal_set) );
  CHKE( sigaddset (&waited_signal_set, SIGUSR1) );

  // Create a thread-specific data key
  CHK( pthread_key_create (&edf_key, NULL) );

  // Initialize actions object
  CHK( posix_appsched_actions_init (&actions) );

  CHKE( clock_gettime (CLOCK_REALTIME, &start_time) );

  while (1) {
    /* Show current ready queue status */
    show_RQ (&start_time);

    /* Actions of activation and suspension of threads */
    schedule_next (&actions);
	       
    /* Execute scheduling actions */
    CHK( posix_appsched_execute_actions (&actions, &waited_signal_set, 
					 NULL, &now, &event) );

    /* Initialize actions object */
    CHK( posix_appsched_actions_destroy (&actions) );
    CHK( posix_appsched_actions_init (&actions) );

    /* Show received event */
    dif_time = now;
    decr_timespec (&dif_time, &start_time);
    printf ("\nEvent: %s at %2.1fs\n", appsched_event_codes[event.event_code],
	    dif_time.tv_sec + dif_time.tv_nsec / 1000000000.0);

    /* Process scheduling events */
    switch (event.event_code) {

    case POSIX_APPSCHED_NEW:
      add_to_list_of_threads (event.thread, &now, &actions);
      break;

    case POSIX_APPSCHED_TERMINATE:
      eliminate_from_list_of_threads (event.thread);
      break;

    case POSIX_APPSCHED_READY:
      make_ready (event.thread, &now);
      break;

    case POSIX_APPSCHED_BLOCK:
      make_blocked (event.thread);
      break;

    case POSIX_APPSCHED_EXPLICIT_CALL:
      // The thread has done all its work for the present activation
      make_timed (event.thread);
      break;

    case POSIX_APPSCHED_SIGNAL:
      reached_activation_time (event.event_info.siginfo.si_value.sival_ptr);
    }
  }

  return NULL;
}
