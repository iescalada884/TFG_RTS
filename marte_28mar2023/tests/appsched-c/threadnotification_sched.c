
#include <sched.h>
#include <pthread.h>
#include <stdio.h>
#include <misc/timespec_operations.h>
#include <time.h>
#include <misc/error_checks.h>
#include <stdlib.h>
#include <assert.h>

#include "trace.h"
#include "threadnotification_sched.h"

double expected_thread_execution_time = 0.0;
int thread_notification_invoked = 0;

/*
 * sched_init
 */
void threadnotification_sched_init (void * sched_data)
{ 
  trace_show_current_time("SCHR init");
}



/*
 * sched_new_thread
 */
void threadnotification_sched_new_thread (void * sched_data, pthread_t thread,
					  posix_appsched_actions_t * actions)
{
  struct timespec current_time;
  clock_gettime (CLOCK_MONOTONIC, &current_time);

  trace_show_current_time("SCHR new");	
	
  const struct timespec interval = {2, 0};

  incr_timespec(&current_time, &interval);
	
  CHK (posix_appsched_actions_addaccept (actions, thread));
  CHK (posix_appsched_actions_addthreadnotification (actions,
						     thread,
						     &current_time));

  expected_thread_execution_time = timespec_to_double(&current_time);
	
  trace_show_time("SCHR notification at", &current_time);	

}


/*
 * notification_for_thread
 */
void threadnotification_sched_notification_for_thread
   (void * sched_data, pthread_t thread,
    posix_appsched_actions_t * actions)
{
  struct timespec current_time;
  clock_gettime (CLOCK_MONOTONIC, &current_time);

  // check the activation is at the appropriate time
  assert(abs(expected_thread_execution_time-timespec_to_double(&current_time))
	 < TIME_MARGIN);

  trace_show_current_time("SCHR notification");
	
  CHK (posix_appsched_actions_addactivate(actions, thread) );
  
  thread_notification_invoked = 1;

}

