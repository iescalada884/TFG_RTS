/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                 's c h e d'
 *
 *                                      H
 *
 * File 'sched.h'                                                      by MAR.
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

#ifndef	_MARTE_SCHED_H_
#define _MARTE_SCHED_H_

#include <sys/cpp_macros.h>
#include <sys/marte_general_constants.h>
#include <sys/types.h>
#include <time.h> // Forced by POSIX
#include <signal.h> // for 'siginfo_t'
#include <stdint.h>

CPP_BEGIN_DECLS

/* sched_param */
struct sched_param {
  // Scheduling priority (only sched. param used in FIFO and RR)
  int sched_priority;

  // Sporadic server policy parameters
  int sched_ss_low_priority;
  struct timespec sched_ss_repl_period;
  struct timespec sched_ss_init_budget;
  int sched_ss_max_repl;
};
//  Any modification here should be synchronized with the related type
//  defined in 'kernel-tasks_operations.ads'

/* 13.2.- Scheduling Policies */
#define SCHED_FIFO     _MARTE_SCHED_FIFO     // FIFO scheduling policy
#define SCHED_RR       _MARTE_SCHED_RR       // Round robin scheduling policy
#define SCHED_SPORADIC _MARTE_SCHED_SPORADIC // Sporadic Server sched. policy
#define SCHED_OTHER    _MARTE_SCHED_OTHER    // Another scheduling policy
#define SCHED_APP      _MARTE_SCHED_APP      // Appl.-defined sched. policy
#define SCHED_EDF      _MARTE_SCHED_EDF      // EDF scheduling policy

int        sched_yield (void);
int        sched_get_priority_max (int policy);
int        sched_get_priority_min (int policy);
int        sched_rr_get_interval (pid_t pid, struct timespec * interval);

/*
 * Application-defined scheduling policy
 * (MaRTE expecific)
 */

// == General types ==

typedef uint64_t posix_appsched_urgency_t;

typedef marte_posix_appsched_actions_t posix_appsched_actions_t;

// == Interfaces for application schedulers ==

// Scheduler operations

typedef struct {

  void (*init) (void * sched_data);

  void (*new_thread) (void * sched_data, pthread_t thread,
		      posix_appsched_actions_t * actions);

  void (*thread_ready) (void * sched_data, pthread_t thread,
			posix_appsched_actions_t * actions);

  void (*explicit_call) (void * sched_data, pthread_t thread,
			 int user_event_code,
			 posix_appsched_actions_t * actions);
  
  void (*notification_for_thread) (void * sched_data, pthread_t thread,
				   posix_appsched_actions_t * actions);

  void (*timeout) (void * sched_data, posix_appsched_actions_t * actions);

  void (*signal) (void * sched_data, siginfo_t siginfo,
		  posix_appsched_actions_t * actions);

} posix_appsched_scheduler_ops_t;

// Scheduler creation

typedef void *posix_appsched_scheduler_id_t;

int posix_appsched_scheduler_create(posix_appsched_scheduler_ops_t * sched_ops,
				    void * sched_data,
				    int priority,
				    posix_appsched_scheduler_id_t * sched_id);

// Add scheduling actions

int posix_appsched_actions_addaccept(posix_appsched_actions_t *sched_actions,
				     pthread_t thread);

int posix_appsched_actions_addreject(posix_appsched_actions_t *sched_actions,
				     pthread_t thread);

int posix_appsched_actions_addactivate(posix_appsched_actions_t *sched_actions,
				       pthread_t thread);

int posix_appsched_actions_addactivateurg
   (posix_appsched_actions_t *sched_actions,
    pthread_t thread,
    posix_appsched_urgency_t urg);

int posix_appsched_actions_addsuspend(posix_appsched_actions_t *sched_actions,
				      pthread_t thread);

int posix_appsched_actions_addtimedactivation
   (posix_appsched_actions_t *sched_actions,
    pthread_t thread,
    posix_appsched_urgency_t urg,
    const struct timespec *at_time);

int posix_appsched_actions_addthreadnotification
   (posix_appsched_actions_t *sched_actions,
    pthread_t thread,
    const struct timespec *at_time);

// == Interfaces for application-scheduled threads ==

// Explicit Scheduler Invocation

int posix_appsched_invoke_scheduler (int user_event_code);
int posix_appsched_invoke_withdata (const void * msg, size_t msg_size,
				    void *reply, size_t *reply_size);

/*
 * Application-defined scheduling policy
 * Old version. Deprecated
 *

// Scheduling Events 
union posix_appsched_eventinfo {
  int sched_priority;
  siginfo_t siginfo;
  pthread_mutex_t *mutex;
  void *info;
  int user_event_code;
};

struct posix_appsched_event {
  int event_code;
  pthread_t thread;
  union posix_appsched_eventinfo event_info;
  size_t info_size;
};
// Any change in the union and/or struct implies changes in type
// 'POSIX_Appsched_Event' in package 'Sched' ('kernel/sched.ads').

// The event codes are defined in the following file
#include <sys/marte_sched_events_codes.h>

// Scheduling actions
typedef marte_posix_appsched_actions_t posix_appsched_actions_t;
int posix_appsched_actions_init (posix_appsched_actions_t *sched_actions);
int posix_appsched_actions_destroy (posix_appsched_actions_t *sched_actions);
int posix_appsched_actions_addaccept(posix_appsched_actions_t *sched_actions,
				     pthread_t thread);
int posix_appsched_actions_addreject(posix_appsched_actions_t *sched_actions,
				     pthread_t thread);
int posix_appsched_actions_addactivate(posix_appsched_actions_t *sched_actions,
				       pthread_t thread);
int posix_appsched_actions_addsuspend (posix_appsched_actions_t *sched_actions,
				       pthread_t thread);
int posix_appsched_actions_addacceptmutex(
                           posix_appsched_actions_t *sched_actions,
			   const pthread_mutex_t *mutex);
int posix_appsched_actions_addrejectmutex(
                           posix_appsched_actions_t *sched_actions,
			   const pthread_mutex_t *mutex);
int posix_appsched_actions_addlockmutex (
			   posix_appsched_actions_t *sched_actions,
			   pthread_t thread,
			   const pthread_mutex_t *mutex);

// Explicit Scheduler Invocation
int posix_appsched_invoke_scheduler (int user_event_code);
int posix_appsched_invoke_withdata (const void * msg, size_t msg_size,
				    void *reply, size_t *reply_size);

// Execute Scheduling Actions
int posix_appsched_execute_actions (const
				    posix_appsched_actions_t *sched_actions,
				    const sigset_t *set,
				    const struct timespec *timeout,
				    struct timespec *current_time,
				    struct posix_appsched_event *event);

 *
 * Scheduler attributes
 *

// Scheduler clock 
int posix_appschedattr_setclock (clockid_t clockid);
int posix_appschedattr_getclock (clockid_t *clockid);

// Scheduler flags
#define POSIX_APPSCHED_ABSTIMEOUT           _MARTE_POSIX_APPSCHED_ABSTIMEOUT
int posix_appschedattr_setflags (int flags);
int posix_appschedattr_getflags (int *flags);

// Scheduler events mask

// Scheduling events sets manipulation
typedef marte_posix_appsched_eventset_t posix_appsched_eventset_t;
int posix_appsched_emptyset (posix_appsched_eventset_t *set);
int posix_appsched_fillset (posix_appsched_eventset_t *set);
int posix_appsched_addset (posix_appsched_eventset_t *set, int appsched_event);
int posix_appsched_delset (posix_appsched_eventset_t *set, int appsched_event);
int posix_appsched_ismember (const posix_appsched_eventset_t *set,
			     int appsched_event);

int posix_appschedattr_seteventmask (const posix_appsched_eventset_t *set);
int posix_appschedattr_geteventmask (posix_appsched_eventset_t *set);

// Scheduler reply info
int posix_appschedattr_setreplyinfo (const void *reply, int reply_size);
int posix_appschedattr_getreplyinfo (void *reply, int *reply_size);

End of old version. Deprecated */

CPP_END_DECLS
#endif // _MARTE_SCHED_H_
