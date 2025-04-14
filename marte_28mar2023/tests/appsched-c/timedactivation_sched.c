
#include <sched.h>
#include <pthread.h>
#include <stdio.h>
#include <misc/timespec_operations.h>
#include <time.h>
#include <misc/error_checks.h>
#include <stdlib.h>
#include <assert.h>

#include "trace.h"
#include "timedactivation_sched.h"

double expected_thread_activation_time = 0.0;
int new_thread_invoked = 0;

/*
 * sched_init
 */
void timedactivation_sched_init (void * sched_data)
{ 
  trace_show_current_time("SCHR init");
}



/*
 * sched_new_thread
 */
void timedactivation_sched_new_thread (void * sched_data, pthread_t thread,
					  posix_appsched_actions_t * actions)
{
  struct timespec current_time;
  trace_show_current_time("SCHR new");
  clock_gettime (CLOCK_MONOTONIC, &current_time);	
	
  const struct timespec interval = {2, 0};

  incr_timespec(&current_time, &interval);
	
  CHK (posix_appsched_actions_addaccept (actions, thread));

  CHK (posix_appsched_actions_addtimedactivation (actions,
						  thread,
						  10,
						  &current_time));

  expected_thread_activation_time = timespec_to_double(&current_time);
	
  trace_show_time("SCHR timed activation at", &current_time);

  new_thread_invoked = 1;	
}


