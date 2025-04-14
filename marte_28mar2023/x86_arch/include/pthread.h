/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                               'p t h r e a d'
 *
 *                                      H
 *
 * File 'pthread.h'                                                    by MAR.
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
 *--------------------------------------------------------------------------*/

#ifndef	_MARTE_PTHREAD_H_
#define _MARTE_PTHREAD_H_

#include <sys/cpp_macros.h>
#include <sys/marte_types.h>
#include <sys/marte_general_constants.h>
#include <sys/types.h>
#include <time.h>
#include <sched.h>

CPP_BEGIN_DECLS

/*---------------------------------------------------------------------------*/
/*- Mutexes -----------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/* 11.3 Mutexes */

#define PTHREAD_PRIO_NONE          _MARTE_PTHREAD_PRIO_NONE
#define PTHREAD_PRIO_INHERIT       _MARTE_PTHREAD_PRIO_INHERIT
#define PTHREAD_PRIO_PROTECT       _MARTE_PTHREAD_PRIO_PROTECT
#define PTHREAD_APPSCHED_PROTOCOL  _MARTE_PTHREAD_APPSCHED_PROTOCOL
int pthread_mutexattr_init (pthread_mutexattr_t *attr);
int pthread_mutexattr_destroy (pthread_mutexattr_t *attr);

//extern const pthread_mutex_t kernel__mutexes__pthread_mutex_initializer;
//#define PTHREAD_MUTEX_INITIALIZER kernel__mutexes__pthread_mutex_initializer
#define PTHREAD_MUTEX_INITIALIZER \
{32, -62, 41, 8, 0, 0, 0, 0, 77, 73, \
 78, 73, 101, 0, 0, 0, 0, 0, 0, 0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, \
 64, -63, 41, 8,  0,  0,  0,  0,  0,  0, 0, 0}

int pthread_mutex_init (pthread_mutex_t *m,
			const pthread_mutexattr_t *attr);
int pthread_mutex_destroy (pthread_mutex_t *m);
int pthread_mutex_lock (pthread_mutex_t *m);
int pthread_mutex_trylock (pthread_mutex_t *m);
int pthread_mutex_unlock (pthread_mutex_t *m);
int pthread_mutex_timedlock (pthread_mutex_t *m,
			     const struct timespec *abs_timeout);

/* 13.6 Synchronization Scheduling */

int     pthread_mutexattr_setprotocol (pthread_mutexattr_t *attr,
				       int protocol);
int	pthread_mutexattr_getprotocol (const pthread_mutexattr_t *attr,
				       int *protocol);
int	pthread_mutexattr_setprioceiling (pthread_mutexattr_t *attr,
					  int prioceiling);
int	pthread_mutexattr_getprioceiling (const pthread_mutexattr_t *attr,
					  int *prioceiling);
int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type);
int pthread_mutexattr_gettype(const pthread_mutexattr_t *attr, int *type);

int pthread_mutex_getprioceiling(const pthread_mutex_t *mutex,
				 int *prioceiling);
int pthread_mutex_setprioceiling(pthread_mutex_t *mutex,
				 int prioceiling, int *old_ceiling);

/* Application-defined Mutex Protocol */

int pthread_mutexattr_setappscheduler (pthread_mutexattr_t *attr,
				       pthread_t appscheduler);
int pthread_mutexattr_getappscheduler (const pthread_mutexattr_t *attr,
				       pthread_t *appscheduler);

int pthread_mutexattr_setappschedparam (
	 pthread_mutexattr_t *attr,
	 const void *param,
	 size_t param_size);
int pthread_mutexattr_getappschedparam (
	 const pthread_mutexattr_t *attr,
	 void *param,
	 size_t *param_size);
int pthread_mutexattr_setpreemptionlevel (pthread_mutexattr_t *attr,
					  unsigned short int level);
int pthread_mutexattr_getpreemptionlevel (pthread_mutexattr_t *attr,
					  unsigned short int *level);

int pthread_mutex_getappscheduler (const pthread_mutex_t *mutex,
				   pthread_t *scheduler);

int pthread_mutex_setappschedparam (
	 pthread_mutex_t *mutex,
	 const void *param,
	 size_t param_size);
int pthread_mutex_getappschedparam (
         const pthread_mutex_t *mutex,
	 void *param,
	 size_t *param_size);

int posix_appsched_mutex_setspecific (pthread_mutex_t *mutex,
				      const void *data);
int posix_appsched_mutex_getspecific (const pthread_mutex_t *mutex,
				      void **data);


/*---------------------------------------------------------------------------*/
/*- Condition Variables -----------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/* 11.4 Condition Variables */
int		pthread_condattr_init (pthread_condattr_t *attr);
int		pthread_condattr_destroy (pthread_condattr_t *attr);

int		pthread_cond_init (pthread_cond_t *c,
				   const pthread_condattr_t *attr);
int		pthread_cond_destroy (pthread_cond_t *c);
extern const pthread_cond_t PTHREAD_COND_INITIALIZER;

int		pthread_cond_signal (pthread_cond_t *c);
int		pthread_cond_broadcast (pthread_cond_t *c);

int		pthread_cond_wait (pthread_cond_t *c, pthread_mutex_t *m);
int		pthread_cond_timedwait (pthread_cond_t *c, pthread_mutex_t *m,
					const struct timespec *abstime);


/*---------------------------------------------------------------------------*/
/*- Threads ----------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/* 16.- Threads */

#define PTHREAD_CREATE_JOINABLE _MARTE_PTHREAD_CREATE_JOINABLE
#define PTHREAD_CREATE_DETACHED _MARTE_PTHREAD_CREATE_DETACHED

#define PTHREAD_INHERIT_SCHED  _MARTE_PTHREAD_INHERIT_SCHED
#define PTHREAD_EXPLICIT_SCHED _MARTE_PTHREAD_EXPLICIT_SCHED

int		pthread_attr_init (pthread_attr_t *attr);
int		pthread_attr_destroy (pthread_attr_t *attr);
int		pthread_attr_setstacksize(pthread_attr_t *attr,
					  size_t stacksize);
int		pthread_attr_getstacksize(const pthread_attr_t *attr,
					  size_t *stacksize);
int		pthread_attr_setstackaddr(pthread_attr_t *attr,
					  void * stackaddr);
int		pthread_attr_getstackaddr(pthread_attr_t *attr,
					  void ** stackaddr);
int		pthread_attr_setdetachstate(pthread_attr_t *attr,
					    int detachstate);
int		pthread_attr_getdetachstate(const pthread_attr_t *attr,
					    int *detachstate);
int		pthread_create (pthread_t *thread, const pthread_attr_t *attr,
				void *(*start_routine)(void *), void *arg);
void            pthread_exit (void *value_ptr);
pthread_t       pthread_self (void);
int		pthread_join (pthread_t thread, void **value_ptr);
int		pthread_detach (pthread_t thread);
int             pthread_equal (pthread_t t1, pthread_t t2);
int             pthread_once(pthread_once_t *once_control,
                             void (*init_routine)(void));
#define PTHREAD_ONCE_INIT _MARTE_PTHREAD_ONCE_INIT

int pthread_cancel(pthread_t thread);

enum {
        PTHREAD_MUTEX_TIMED_NP,
        PTHREAD_MUTEX_RECURSIVE_NP,
        PTHREAD_MUTEX_ERRORCHECK_NP,
        PTHREAD_MUTEX_ADAPTIVE_NP,
        PTHREAD_MUTEX_NORMAL = PTHREAD_MUTEX_TIMED_NP,
        PTHREAD_MUTEX_RECURSIVE = PTHREAD_MUTEX_RECURSIVE_NP,
        PTHREAD_MUTEX_ERRORCHECK = PTHREAD_MUTEX_ERRORCHECK_NP,
        PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL
};

/* 13.5 Thread Scheduling Functions */

#define PTHREAD_SCOPE_SYSTEM   _MARTE_PTHREAD_SCOPE_SYSTEM
#define PTHREAD_SCOPE_PROCESS  _MARTE_PTHREAD_SCOPE_PROCESS
int             pthread_attr_setinheritsched (pthread_attr_t *attr,
					      int inheritsched);
int             pthread_attr_getinheritsched (const pthread_attr_t *attr,
					      int *inheritsched);
int		pthread_attr_setschedpolicy (pthread_attr_t *attr, int policy);
int		pthread_attr_getschedpolicy (const pthread_attr_t *attr,
					     int * policy);
int		pthread_attr_setschedparam (pthread_attr_t *attr,
					    const struct sched_param *param);
int		pthread_attr_getschedparam (const pthread_attr_t *attr,
					    struct sched_param *param);

int		pthread_setschedparam (pthread_t thread, int policy,
				       const struct sched_param *param);
int		pthread_getschedparam (pthread_t thread, int *policy,
				       struct sched_param *param);
int             pthread_setschedprio (pthread_t thread, int prio);

/*
 * EDF functions
 */
// EDF scheduling is not defined in the POSIX standard.
// MaRTE OS provides a non-standard interface to support EDF and
// the Stack Resource Policy (SRP), an EDF-compatible
// synchronization protocol based on the "Preemption level" concept.
//
// See tests/sched/ for examples of use of the EDF scheduling policy
// in MaRTE.

int pthread_attr_setreldeadline(pthread_attr_t *attr,
				const struct timespec *reldeadline);
int pthread_attr_getreldeadline(const pthread_attr_t *attr,
				struct timespec *reldeadline);
// Sets and gets the relative deadline of a thread.
// The first absolute deadline of the thread will be:
//   first_abs_deadline = activation_time + reldeadline

int pthread_attr_setpreemptionlevel(pthread_attr_t *attr,
				    unsigned short int preemptionlevel);
int pthread_attr_getpreemptionlevel(const pthread_attr_t *attr,
				    unsigned short int *preemptionlevel);
// Sets and gets the preemtion level of a thread.
// Preemption levels values must be inversely proportional to thread
// relative deadlines (the lowest relative deadline tasks must be assigned
// the highest preemption levels).

int pthread_setdeadline(pthread_t thread,
			const struct timespec *deadline,
			clockid_t clock_id,
			int immediate);
int pthread_getdeadline(pthread_t thread,
			clockid_t clock_id,
			struct timespec *deadline);
// Sets and gets the absolute deadline associated with a thread.
// The deadline parameter of pthread_setdeadline and pthread_getdeadline is
// based on the clock specified by clock_id.
// For the pthread_setdeadline, if immediate is 1 the change of absolute
// deadline has an immediate effect. In case immediate is 0 the change will
// be delayed until the next time the thread is reactivated after having
// been suspended or blocked.
// RETURN VALUE
//    If successful, the pthread_setdeadline() and pthread_getdeadline()
//    functions shall return zero; otherwise, an error number shall be returned
//    to indicate the error.
// ERRORS
//  The pthread_setdeadline() function shall fail if:
//    [EINVAL]
//        The value specified by deadline is negative or it will be negative
//        when transformed into monotonic time.
//  The pthread_setdeadline() and pthread_getdeadline() functions shall fail
//  if:
//    [ESRCH]
//        The value specified by thread does not refer to an existing thread.
//    [EPERM]
//        Clock is not CLOCK_REALTIME nor CLOCK_MONOTONIC.

int pthread_setpreemptionlevel(pthread_t thread,
			       short int preemptionlevel);
int pthread_getpreemptionlevel(pthread_t thread,
			       short int *preemptionlevel);
// Sets and gets the preemption level associated with a thread.
// RETURN VALUE
//    If successful, the pthread_preemptionlevel() and
//    pthread_getpreemptionlevel() functions shall return zero; otherwise, an
//    error number shall be returned to indicate the error.
// ERRORS
//    [ESRCH]
//        The value specified by thread does not refer to an existing thread.

/*
 * 17. Thread-Specific Data
 */
int    pthread_key_create (pthread_key_t *key, void (*destructor) (void *));
int    pthread_setspecific (pthread_key_t key, const void *value);
void * pthread_getspecific (pthread_key_t key);
int    pthread_key_delete (pthread_key_t key);
int    pthread_setspecific_for (pthread_key_t key, pthread_t thread,
				const void *value);
int    pthread_getspecific_from (pthread_key_t key, pthread_t thread,
				 void **value);

/* 20 Execution Time Monitoring */
int pthread_getcpuclockid (pthread_t thread_id, clockid_t *clock_id);

/*
 * Application-defined scheduling policy
 */

// == Interfaces for application-scheduled threads ==

// Set/Get application-scheduler of task

int pthread_attr_setappscheduler(pthread_attr_t *attr,
				 posix_appsched_scheduler_id_t scheduler);
int pthread_attr_getappscheduler(const pthread_attr_t *attr,
				 posix_appsched_scheduler_id_t *scheduler);

// Set/Get application-defined scheduling parameters of task attributes

int pthread_attr_setappschedparam(pthread_attr_t *attr,
				  const void *param,
				  size_t paramsize);
int pthread_attr_getappschedparam(const pthread_attr_t *attr,
				  void *param,
				  size_t *paramsize);

// Set/Get application-defined scheduling parameters of task

int pthread_setappschedparam (pthread_t thread,
			      const void *param,
			      size_t paramsize);
int pthread_getappschedparam (pthread_t thread,
			      void *param,
			      size_t *paramsize);

/*
 * Application-defined scheduling policy
 * Old version. Deprecated
 *
#define PTHREAD_REGULAR      _MARTE_PTHREAD_REGULAR
#define PTHREAD_APPSCHEDULER _MARTE_PTHREAD_APPSCHEDULER
int pthread_attr_setappschedulerstate (pthread_attr_t *attr,
				       int appschedstate);
int pthread_attr_getappschedulerstate (const pthread_attr_t *attr,
				       int *appschedstate);

int pthread_attr_setappscheduler (pthread_attr_t *attr,
				  pthread_t scheduler);
int pthread_attr_getappscheduler (const pthread_attr_t *attr,
				  pthread_t *scheduler);

int pthread_attr_setappschedparam  (pthread_attr_t *attr,
				    const void *param,
				    size_t paramsize);
int pthread_attr_getappschedparam  (const pthread_attr_t *attr,
				    void *param,
				    size_t *paramsize);


int pthread_getappschedulerstate (pthread_t thread, int *appschedstate);

int pthread_setappscheduler (pthread_t thread,
			     pthread_t scheduler);
int pthread_getappscheduler (pthread_t thread,
			     pthread_t *scheduler);
End of old version. Deprecated */

int pthread_getsrpsystemceiling (int *prio, unsigned short int *pl);

CPP_END_DECLS
#endif /* _MARTE_PTHREAD_H_ */
