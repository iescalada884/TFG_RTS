/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                              'c b s _ s c h e d'
 *
 *                                      C
 *
 * File 'cbs_sched.c'                                                  by MAR.
 *
 * Uses the Application-Defined Scheduling Interface to implement an
 * EDF-CBS scheduler.
 *
 * The EDF threads are periodic and their deadlines can be
 * different from their periods.
 *
 * For each CBS thread a periodic timer is programmed which spires
 * when its budget gets exhausted.
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
#include <signal.h>
#include <misc/timespec_operations.h>
#include <sys/marte_appsched_event_codes_str.h>
#include <misc/error_checks.h>

#include "cbs_sched.h"

typedef enum {ACTIVE, BLOCKED, TIMED, CBS_IDLE} th_state_t;

struct thread_data_t {
  int id; // To identify threads easily
  pthread_t thread_id;
  th_state_t th_state;
  struct timespec period,deadline; /* relative times */
  struct timespec next_deadline, next_start; /* absolute times */
  int cbs;
  struct timespec mx_budget;
  struct timespec budget, activation_time;
  struct timespec end_budget;
  int number_of_pending_jobs;
  timer_t timer_id;
} thread_data[MAX_THREADS];

int num_of_threads=0;

pthread_key_t cbs_key; // Thread-specific data key

#define END_OF_BUDGET_SIGNAL SIGRTMIN

/*
 * Relative time
 */
struct timespec start_time;
float relative_time (struct timespec *time)
{
    struct timespec dif_time = *time;
    decr_timespec (&dif_time, &start_time);
    return dif_time.tv_sec + dif_time.tv_nsec / 1000000000.0;
}

/*
 * schedule_next
 */
void schedule_next (struct thread_data_t **current_thread,
		    struct timespec *earliest_start,
		    const struct timespec *now)
{
  struct timespec earliest_deadline;
  int t;

  *current_thread = NULL;

  earliest_deadline.tv_sec = -1;
  earliest_start->tv_sec = -1; earliest_start->tv_nsec = 0;
  for (t=0; t<num_of_threads; t++) {
    /* find threads to wake up */
    if (thread_data[t].th_state == TIMED &&
	smaller_or_equal_timespec (&thread_data[t].next_start, now))
      thread_data[t].th_state = ACTIVE;

    /* find next thread to run */
    if (thread_data[t].th_state == ACTIVE &&
	(earliest_deadline.tv_sec == -1 ||
	 smaller_timespec (&thread_data[t].next_deadline, 
			   &earliest_deadline))) {
      earliest_deadline = thread_data[t].next_deadline;
      *current_thread = &thread_data[t];
    }
    
    /* find next thread to activate*/
    if (thread_data[t].th_state == TIMED &&
	(earliest_start->tv_sec == -1 ||
	 smaller_timespec(&thread_data[t].next_start, earliest_start))) 
      *earliest_start = thread_data[t].next_start;
  }
}

/*
 * add_to_list_of_threads
 */
void add_to_list_of_threads (pthread_t thread_id,
			     const struct timespec *now,
			     posix_appsched_actions_t *actions)
{
  struct cbs_sched_param param;
  size_t param_size;
  clockid_t clock_id;
  struct sigevent evp;

  if (num_of_threads<MAX_THREADS) {
    thread_data[num_of_threads].id = num_of_threads+1;
    CHK( pthread_getappschedparam (thread_id, &param, &param_size) );
    thread_data[num_of_threads].period     = param.period;
    thread_data[num_of_threads].deadline   = param.deadline;
    thread_data[num_of_threads].next_start = *now;
    if (param.cbs) {
      thread_data[num_of_threads].th_state = CBS_IDLE;
      thread_data[num_of_threads].next_deadline = *now;
    } else {
      thread_data[num_of_threads].th_state  = ACTIVE;
      add_timespec (&thread_data[num_of_threads].next_deadline,
		    &param.deadline,
		    &thread_data[num_of_threads].next_start);
    }
    thread_data[num_of_threads].thread_id = thread_id;
    thread_data[num_of_threads].cbs = param.cbs;
    thread_data[num_of_threads].mx_budget = param.mx_budget;
    thread_data[num_of_threads].budget = param.mx_budget;
    thread_data[num_of_threads].number_of_pending_jobs = 0;
    // Create a CPU-timer for the CBS threads
    if (param.cbs) {
      evp.sigev_notify          = SIGEV_SIGNAL;
      evp.sigev_signo           = END_OF_BUDGET_SIGNAL;
      evp.sigev_value.sival_ptr = &thread_data[num_of_threads];
      CHK( pthread_getcpuclockid (thread_id, &clock_id) );
      CHKE( timer_create (clock_id, &evp, 
			  &thread_data[num_of_threads].timer_id) );
    }
    // Associate specific information with thread
    CHK( pthread_setspecific_for (cbs_key, thread_id, 
				  &thread_data[num_of_threads]) );

    printf ("id:%d Per:%d.%1ds Deadline:%d.%1ds Start:%2.1fs\n",
	    thread_data[num_of_threads].id, 
	    param.period.tv_sec, param.period.tv_nsec,
	    param.deadline.tv_sec, param.deadline.tv_nsec,
	    relative_time (&thread_data[num_of_threads].next_start));
    num_of_threads++;
    // accept thread
    CHK( posix_appsched_actions_addaccept (actions, thread_id) );

  } else {
    // no more threads allowed => reject thread
    CHK( posix_appsched_actions_addreject (actions, thread_id) );
  }
}

/*
 * eliminate_from_list_of_threads
 */
void eliminate_from_list_of_threads (pthread_t thread_id)
{
  int found=-1;
  int t;

  for (t=0;t<num_of_threads;t++) {
    if (pthread_equal (thread_data[t].thread_id, thread_id)) {
      found=t;    
      // delete timer if CBS
      if (thread_data[t].cbs)
	CHKE( timer_delete (thread_data[t].timer_id) );
      break;
    }
  }
  if (found>=0) {
    num_of_threads--;
    for (t=found;t<num_of_threads;t++) {
      thread_data[t]=thread_data[t+1];
    }
  }
}

/*
 * make_ready
 */
void make_ready (struct thread_data_t *th_data)
{
  th_data->th_state = ACTIVE;
}

/*
 * make_blocked
 */
void make_blocked (struct thread_data_t *th_data)
{
  th_data->th_state = BLOCKED;
}

/*
 * make_timed
 */
void make_timed (struct thread_data_t *th_data, const struct timespec *now)
{
  if (smaller_timespec (&th_data->next_deadline, now))
    printf (" Deadline missed in thread:%d !!", th_data->id);
  if (th_data->cbs) {
    th_data->number_of_pending_jobs--;
    if (!th_data->number_of_pending_jobs) {
      th_data->th_state = CBS_IDLE;
      incr_timespec(&th_data->next_start, &th_data->period);
      incr_timespec(&th_data->next_deadline, &th_data->period);
    }
  } else {
    // Not a CBS thread
    th_data->th_state = TIMED;
    incr_timespec(&th_data->next_start, &th_data->period);
    incr_timespec(&th_data->next_deadline, &th_data->period);
  }
}

/*
 * recharge_cbs
 */
void recharge_cbs (struct thread_data_t * cbs_thread, 
		   const struct timespec * now)
{
  struct itimerspec timer_prog;

  incr_timespec(&cbs_thread->next_deadline, &cbs_thread->period);
  cbs_thread->budget = cbs_thread->mx_budget;

  // reprogram CPU-time timer
  timer_prog.it_value = cbs_thread->mx_budget;
  timer_prog.it_interval.tv_sec = 0; timer_prog.it_interval.tv_nsec = 0;
  CHKE( timer_settime (cbs_thread->timer_id, 0, &timer_prog, NULL) );
}

int enough_time_in_this_period (struct thread_data_t * cbs_thread,
				const struct timespec * now)
{
  struct timespec ratio;
  struct timespec job_ending_time;

  div_timespec (&ratio, &cbs_thread->budget, &cbs_thread->mx_budget);
  mult_timespec (&job_ending_time, &ratio, &cbs_thread->period);
  add_timespec (&job_ending_time, now, &job_ending_time);

  return smaller_timespec (&job_ending_time, &cbs_thread->next_deadline);
}

/*
 * new_job_for_cbs_thread
 */
void new_job_for_cbs_thread (struct thread_data_t * th_data,
			     const struct timespec * now)
{
  struct itimerspec timer_prog;

  /* Calculate next thread activation and make it timed */
  th_data->number_of_pending_jobs++;
  if (th_data->th_state == CBS_IDLE) {
    th_data->th_state = ACTIVE; 
    if (!enough_time_in_this_period (th_data, now)) {
      // Set a new server deadline, recharge its capacity and rearm
      // its "end of budget" timer.
      incr_timespec(&th_data->next_deadline, &th_data->period);
      th_data->budget = th_data->mx_budget;

      // reprogram CPU-time timer
      timer_prog.it_value = th_data->mx_budget;
      timer_prog.it_interval.tv_sec = 0; timer_prog.it_interval.tv_nsec = 0;
      CHKE( timer_settime (th_data->timer_id, 0, &timer_prog, NULL) );
    }
  }
}

/*
 * edf_scheduler
 */
void *edf_scheduler (void *arg)
{
  struct thread_data_t *current_thread = NULL, *next_thread = NULL;
  struct thread_data_t *th_data = NULL;
  struct posix_appsched_event sched_event;
  struct timespec earliest_start;
  struct timespec now;
  posix_appsched_actions_t actions;
  sigset_t waited_signal_set;
  posix_appsched_eventset_t eventset;
    
  // Initialize the 'waited_signal_set'
  CHKE( sigemptyset (&waited_signal_set) );
  CHKE( sigaddset (&waited_signal_set, NEW_JOB_SIGNAL) );
  CHKE( sigaddset (&waited_signal_set, END_OF_BUDGET_SIGNAL) );
    
  /* Init scheduler */
  posix_appsched_fillset (&eventset);
  posix_appsched_delset (&eventset, POSIX_APPSCHED_NEW);
  posix_appsched_delset (&eventset, POSIX_APPSCHED_TERMINATE);
  posix_appsched_delset (&eventset, POSIX_APPSCHED_READY);
  posix_appsched_delset (&eventset, POSIX_APPSCHED_BLOCK);
  posix_appsched_delset (&eventset, POSIX_APPSCHED_EXPLICIT_CALL);
  posix_appsched_delset (&eventset, POSIX_APPSCHED_SIGNAL);
  posix_appsched_delset (&eventset, POSIX_APPSCHED_TIMEOUT);
  CHK( posix_appschedattr_seteventmask (&eventset) );
  CHK( posix_appschedattr_setclock (CLOCK_REALTIME) );
  CHK( posix_appschedattr_setflags (POSIX_APPSCHED_ABSTIMEOUT) );
  clock_gettime (CLOCK_REALTIME, &now);

  /* Get thread-specific data key */
  CHK( pthread_key_create (&cbs_key, NULL) );

  start_time = now;
  CHK( posix_appsched_actions_init (&actions) );

  while (1) {
    schedule_next (&next_thread, &earliest_start, &now);
    printf (" At %2.1f  Next_Th:%d(DL:%2.1f)  Next_Start:%2.1f ", 
	    relative_time (&now), next_thread?next_thread->id:0,
	    next_thread?relative_time (&next_thread->next_deadline):-1.0,
	    earliest_start.tv_sec==-1?-1.0:relative_time (&earliest_start));

    if (next_thread != NULL) {
      // Activate next thread
      printf (" Activate:%d ", next_thread->id);
      CHK( posix_appsched_actions_addactivate (&actions, 
					       next_thread->thread_id) );
    }
    if (current_thread != NULL && 
	!(current_thread == next_thread) &&
	current_thread->th_state != BLOCKED) {
      // Suspend "old" current thread
      printf (" Suspend:%d ", current_thread->id);
      CHK( posix_appsched_actions_addsuspend (&actions,
					      current_thread->thread_id) );
    }

    current_thread = next_thread;
	
       
    /* Execute scheduling actions */
    if (earliest_start.tv_sec != -1) {
      // with timeout
      CHK( posix_appsched_execute_actions (&actions, &waited_signal_set, 
					   &earliest_start,
					   &now, &sched_event) );
    } else
	// without timeout
	CHK( posix_appsched_execute_actions (&actions, &waited_signal_set, 
					     NULL, 
					     &now, &sched_event) );

    /* Reset actions object */
    CHK( posix_appsched_actions_destroy (&actions) );
    CHK( posix_appsched_actions_init (&actions) );

    /* Process scheduling events */
    printf ("\n%s", appsched_event_codes[sched_event.event_code]);
    switch (sched_event.event_code) {
    case POSIX_APPSCHED_NEW :
      add_to_list_of_threads (sched_event.thread, &now, &actions);
      break;

    case POSIX_APPSCHED_TERMINATE :
      eliminate_from_list_of_threads (sched_event.thread);
      break;

    case POSIX_APPSCHED_READY :
      CHK( pthread_getspecific_from (cbs_key, sched_event.thread, 
				     (void **)&th_data) );
      make_ready (th_data);
      break;

    case POSIX_APPSCHED_BLOCK :
      make_blocked (current_thread);
      break;

    case POSIX_APPSCHED_EXPLICIT_CALL :
	printf ("-> END OF CURRENT JOB\n");
      make_timed (current_thread, &now);
      break;

    case POSIX_APPSCHED_SIGNAL :
      switch (sched_event.event_info.siginfo.si_signo) {
      case END_OF_BUDGET_SIGNAL:
	printf ("-> END OF BUDGET\n");
	recharge_cbs (current_thread, &now);
	break;
      case NEW_JOB_SIGNAL:
	printf ("-> NEW JOB\n");
	CHK( pthread_getspecific_from 
	        (cbs_key, 
		 (pthread_t) sched_event.event_info.siginfo.si_value.sival_int,
		 (void **)&th_data) );
	new_job_for_cbs_thread (th_data, &now);
	break;
      default:
	printf ("Unexpected signal!!\n");
      }
      break;
	
    case POSIX_APPSCHED_TIMEOUT :
      printf ("-> REACHED ACTIVATION\n");
      // Does nothing. Threads will be activated by 'schedule_next'
      break;
    } // switch (sched_event.event_code)

  } // while (1)

  return NULL;
}
