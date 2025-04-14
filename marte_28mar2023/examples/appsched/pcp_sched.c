/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                             'p c p _ s c h e d'
 *
 *                                      C
 *
 * File 'pcp_sched.c'                                                 by MAR.
 *
 * Uses the Application-Defined Scheduling Interface to implement
 * Priority Ceiling Protocol mutexes.
 *
 * The scheduler thread here implemented only takes scheduling actions
 * related with the resources, so the scheduled threads are scheduled
 * under the FIFO within priorities policy by the system scheduler.
 *
 * The irrelevant events are discarded.
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
#include <sys/marte_appsched_event_codes_str.h>
#include <misc/load.h>
#include <misc/generic_lists.h>
#include <misc/error_checks.h>

#include "pcp_sched.h"

//#define CONSOLE_MESSAGES 


typedef struct thread_data thread_data_t;
typedef struct mutex_data mutex_data_t;
/* Thread-specific data */
struct thread_data {
  thread_data_t * next;
  int prio; // Current priority
  int base_prio;
  mutex_data_t *mutex_where_blocked;
  pthread_t thread;
};
/* Mutex-specific data */
struct mutex_data {
  mutex_data_t * next;
  list_t blocked_threads;
  thread_data_t *owner;
  int ceiling;
  pthread_mutex_t *mutex_id;
};
 
/*
 * Active mutexes list
 */ 
list_t active_mutexes;

/*
 * system_ceiling_block
 *
 * Returns 1 if the thread cannot lock the mutex due to a system
 * ceiling violation.
 */
int system_ceiling_block (thread_data_t * t_data)
{
  mutex_data_t *m;

  m = head (active_mutexes);
  while (m) {
    if (m->owner && m->owner != t_data && m->ceiling >= t_data->prio)
      // There is a mutex locked by a thread different from 't_data' with a
      // ceiling greater or equal than 't_data' priority.
      return 1; // system-ceiling block
    m = next (m);
  }
  return 0; // no system-ceiling block
}

/*
 * find_thread_ceiling
 *
 * Find the maximum ceiling of the mutexes owned by 't'.
 */
int find_thread_ceiling (thread_data_t * t)
{
  mutex_data_t *m;
  int mx_ceiling = 0;

  m = head (active_mutexes);
  while (m) {
    if (m->owner == t && m->ceiling > mx_ceiling)
      mx_ceiling = m->ceiling;
    m = next (m);
  }
  return mx_ceiling;
}

/*
 * raise_blocking_threads_priorities
 *
 * Raises the priorities of all the threads that are currently blocking 't':
 * threads locking mutexes with ceiling greater or equal than 't'
 * priority.  
 */
void raise_blocking_threads_priorities (thread_data_t *t)
{
  mutex_data_t *m;
  struct sched_param param;

  m = head (active_mutexes);
  while (m) {
    if (m->owner && m->owner != t && m->ceiling >= t->prio) {
      // Mutex locked by a thread different from 't' with a
      // ceiling greater or equal than 't' priority.
      if (m->owner->prio < t->prio) {
	// Raise owner priority
	m->owner->prio = t->prio;
	param.sched_priority = t->prio;
	CHK( pthread_setschedparam (m->owner->thread, SCHED_APP, &param) );

	// Blocked in mutex??
	if (m->owner->mutex_where_blocked) 
	  raise_blocking_threads_priorities (m->owner);	
      }
    }
    m = next (m);
  }
}

/*
 * update_thread_priority
 *
 * Called after locking or unlocking a mutex. Find the new priority of
 * the thread according to its base priority and the priorities of the
 * threads blocked by it.
 */
void update_thread_priority (thread_data_t *t)
{
  mutex_data_t *m;
  thread_data_t *ti;
  int new_prio = t->base_prio;
  int t_ceiling = find_thread_ceiling (t);
  struct sched_param param;

  m = head (active_mutexes);
  while (m) {
    ti = head (m->blocked_threads);
    while (ti) {
      if (ti->prio <= t_ceiling && ti->prio > new_prio)
	new_prio = ti->prio;
      ti = next (ti);
    }      
    m = next (m);
  }

  // Change thread priority ?
  if (t->prio != new_prio) {
    t->prio = new_prio;
    param.sched_priority = new_prio;
    CHK( pthread_setschedparam (t->thread, SCHED_APP, &param) );
  }
}

/*
 * show_thread_data
 */
void show_thread_data (char * msg, thread_data_t *t)
{
#ifdef CONSOLE_MESSAGES
  printf ("%s[p:%d, bp:%d, mb:", msg, t->prio, t->base_prio);
  if (t->mutex_where_blocked) 
    printf ("{c:%d}]", t->mutex_where_blocked->ceiling);
  else 
    printf ("null]");
#endif /* CONSOLE_MESSAGES */
}

/*
 * show_system_status
 */
void show_system_status ()
{
#ifdef CONSOLE_MESSAGES
  mutex_data_t *m = head (active_mutexes);
  thread_data_t *t;
  printf (" <Status:");
  while (m) {
    if (m->owner)
      printf ("{o.bp:%d", m->owner->base_prio);
    else
      printf ("{o:null");
    printf (", c:%d, bt:", m->ceiling);
    t = head (m->blocked_threads);
    while (t) {
      show_thread_data ("", t);
      t = next (t);
    }
    printf ("} ");	
    m = next (m);
  }
  printf (">\n");
#endif /* CONSOLE_MESSAGES */
}

/*
 * show_event
 */
void show_event (int event_code)
{
  //#ifdef CONSOLE_MESSAGES 
  printf (" Event %s\n", appsched_event_codes[event_code]);
  //#endif /* CONSOLE_MESSAGES */
}

/* 
 * PCP Scheduler body
 */
void * pcp_scheduler (void *arg)
{
  /* PCP algoritm variables */
  pthread_key_t pcp_key;
  int num_of_attached_threads = -1;

  /* POSIX SCHED_APP variables */
  posix_appsched_actions_t actions;
  struct pcp_mutex_specific_param mutex_param;
  size_t mutex_param_size;
  struct sched_param param;
  int policy;
  struct posix_appsched_event event;
  posix_appsched_eventset_t event_mask;

  /* Auxiliary variables */
  thread_data_t *t_data;
  mutex_data_t  *m_data;

  // Discard all events but the relevants for the algorithm
  posix_appsched_fillset (&event_mask);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_NEW);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_READY);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_TERMINATE);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_INIT_MUTEX);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_DESTROY_MUTEX);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_LOCK_MUTEX);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_TRY_LOCK_MUTEX);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_UNLOCK_MUTEX);
  posix_appsched_delset (&event_mask, POSIX_APPSCHED_BLOCK_AT_MUTEX);
  CHK( posix_appschedattr_seteventmask (&event_mask) );
  
  // Create a thread-specific data key
  CHK( pthread_key_create (&pcp_key, NULL) );

  // Initialize actions object
  CHK( posix_appsched_actions_init (&actions) );

  // Scheduling events loop
  do {
    show_system_status ();

    // Execute scheduling actions and wait for further event
    CHK( posix_appsched_execute_actions (&actions, NULL, NULL, NULL, &event) );
     
    show_event (event.event_code);

    // Reset actions object
    CHK( posix_appsched_actions_destroy (&actions) );
    CHK( posix_appsched_actions_init (&actions) );

    switch (event.event_code) {

    case POSIX_APPSCHED_NEW:
      // Get thread scheduling parameters
      CHK( pthread_getschedparam (event.thread, &policy, &param) );
      // Assign thread-specific data
      t_data = (thread_data_t *) malloc (sizeof (thread_data_t));
      t_data->prio = param.sched_priority;
      t_data->base_prio = param.sched_priority;
      t_data->mutex_where_blocked = NULL;
      t_data->thread = event.thread;
      CHK( pthread_setspecific_for (pcp_key, event.thread, t_data) );
      // Accept and activate thread
      show_thread_data ("ACT:", t_data);
      CHK( posix_appsched_actions_addaccept (&actions, event.thread) );
      CHK( posix_appsched_actions_addactivate (&actions, event.thread) );
      if (num_of_attached_threads == -1)
	num_of_attached_threads = 1;
      else
	num_of_attached_threads++;
      break;

    case POSIX_APPSCHED_TERMINATE:
      num_of_attached_threads--;
      break;

    case POSIX_APPSCHED_READY:
      CHK( posix_appsched_actions_addactivate (&actions, event.thread) );
      break;
      
    case POSIX_APPSCHED_INIT_MUTEX:
      // Get mutex scheduling parameters
      
      CHK( pthread_mutex_getappschedparam (event.event_info.mutex, 
					   &mutex_param, &mutex_param_size) );
      // Assign mutex-specific data
      m_data = (mutex_data_t *) malloc (sizeof (mutex_data_t));
      m_data->ceiling = mutex_param.ceiling;
      m_data->blocked_threads = NULL;
      m_data->owner = NULL;
      m_data->mutex_id = event.event_info.mutex;
      CHK( posix_appsched_mutex_setspecific (event.event_info.mutex, m_data) );
      // Enqueue mutex
      enqueue_head (m_data, &active_mutexes);
      // Accept Mutex
      CHK( posix_appsched_actions_addacceptmutex (&actions, 
						  event.event_info.mutex) );
      break;

    case POSIX_APPSCHED_DESTROY_MUTEX:
      // Get mutex-specific data
      CHK( posix_appsched_mutex_getspecific (event.event_info.mutex, 
					     (void **)&m_data) );
      // Dequeue mutex from the active mutexes
      dequeue (m_data, &active_mutexes);
      // Free 'm_data'
      free (m_data);
      break;

    case POSIX_APPSCHED_LOCK_MUTEX:
      // Get thread-specific data
      CHK( pthread_getspecific_from (pcp_key, event.thread,
				     (void **)&t_data) );
      // Get mutex-specific data
      CHK( posix_appsched_mutex_getspecific (event.event_info.mutex, 
					     (void **)&m_data) );
      // System ceiling check
      if (system_ceiling_block (t_data)) {
	// Thread blocks at mutex
	enqueue_tail (t_data, &m_data->blocked_threads);
	t_data->mutex_where_blocked = m_data;
	// Raise priorities of all blocking threads.
	raise_blocking_threads_priorities (t_data); 
      } else {
	// Thread locks the mutex
	m_data->owner = t_data;
	show_thread_data ("LOCK:", t_data);
	CHK( posix_appsched_actions_addlockmutex (&actions, event.thread, 
						  event.event_info.mutex) );
      }
      break;

    case POSIX_APPSCHED_TRY_LOCK_MUTEX:
      // Get thread-specific data
      CHK( pthread_getspecific_from (pcp_key, event.thread,
				     (void **)&t_data) );
      // Get mutex-specific data
      CHK( posix_appsched_mutex_getspecific (event.event_info.mutex, 
					     (void **)&m_data) );
      // System ceiling check
      if (system_ceiling_block (t_data)) {
	// Thread cannot lock mutex
	show_thread_data ("TRYLOCK(FAIL):", t_data);
	CHK( posix_appsched_actions_addactivate (&actions, event.thread) );
      } else {
	// Thread locks the mutex
	m_data->owner = t_data;
	show_thread_data ("TRYLOCK(OK):", t_data);
	CHK( posix_appsched_actions_addlockmutex (&actions, event.thread, 
						  event.event_info.mutex) );
      }
      break;

    case POSIX_APPSCHED_UNLOCK_MUTEX:
      // Get thread-specific data
      CHK( pthread_getspecific_from (pcp_key, event.thread,
				     (void **)&t_data) );
      // Get mutex-specific data
      CHK( posix_appsched_mutex_getspecific (event.event_info.mutex,  
					     (void **)&m_data) );
      // Mutex free (maybe just for a while)
      m_data->owner = NULL;
      // Find new thread priority
      update_thread_priority (t_data);
      // Activate thread
      CHK( posix_appsched_actions_addactivate (&actions, event.thread) );
      
      // Look for another thread to lock mutex all over the system
      {
	thread_data_t *ti;
	t_data = NULL;
	m_data = head (active_mutexes);
	while (m_data) {
	  if (!m_data->owner) {
	    ti = head (m_data->blocked_threads);
	    while (ti) {
	      if (!system_ceiling_block (ti) && 
		  (!t_data || ti->prio > t_data->prio)) 
		t_data = ti;
	      ti = next (ti);
	    }
	  }
	  m_data = next (m_data);
	}
      }

      if (t_data) { 
	dequeue (t_data, &t_data->mutex_where_blocked->blocked_threads);
	t_data->mutex_where_blocked->owner = t_data;
	m_data = t_data->mutex_where_blocked; 
	t_data->mutex_where_blocked = NULL;
	// Update the priority of the just locking thread
	update_thread_priority (t_data);
	// Thread locks mutex
	show_thread_data ("LOCK:", t_data);
	CHK( posix_appsched_actions_addlockmutex (&actions, t_data->thread, 
						  m_data->mutex_id) );
      }
      break;


    case POSIX_APPSCHED_BLOCK_AT_MUTEX:

      // Get thread-specific data
      CHK( pthread_getspecific_from (pcp_key, event.thread,
				     (void **)&t_data) );
      // Get mutex-specific data
      CHK( posix_appsched_mutex_getspecific (event.event_info.mutex,  
					     (void **)&m_data) );
      // Thread blocks at mutex
      enqueue_tail (t_data, &m_data->blocked_threads);
      t_data->mutex_where_blocked = m_data;
      // Raise priorities of all blocking threads.
      raise_blocking_threads_priorities (t_data);
      break;

   default :
      printf ("Untreated ");
    }
      
  } while (num_of_attached_threads);
  return NULL;
}
