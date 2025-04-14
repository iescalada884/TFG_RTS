//  Test for all architectures
/*
 * Test the "timed activation" scheduler operation works OK.
 */
#include <sched.h>
#include <pthread.h>
#include <stdio.h>
#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <assert.h>

#include "timedactivation_sched.h"
#include "trace.h"

#define THREAD_PRIO_LEVEL 10


/*
* Scheduled thread body
*/


void * scheduled_thread (void * arg)
{
  // check the activation is at the appropriate time
  struct timespec current_time;
  clock_gettime (CLOCK_MONOTONIC, &current_time);
  assert(abs(expected_thread_activation_time-timespec_to_double(&current_time))
	 < TIME_MARGIN);

  // check scheduler operation has already been executed
  assert(new_thread_invoked);

  trace_show_current_time("Thread ejecuta");		
	
  return NULL;
}


/*
* main
*/
int main()
{
  pthread_attr_t attr;
	
  struct sched_param param;
  pthread_t th;
  posix_appsched_scheduler_id_t scheduler_id;



  const posix_appsched_scheduler_ops_t scheduler_ops =
    {
      timedactivation_sched_init,
      timedactivation_sched_new_thread,
      NULL, //ready
      NULL, // explicit_call
      NULL, // notification_for_thread,
      NULL, // timeout
      NULL, // signal
    };
  
  printf("[main]\n");
  trace_set_initial_time();


  // Create the application scheduler
  CHK( posix_appsched_scheduler_create
       ((posix_appsched_scheduler_ops_t *)&scheduler_ops,
	NULL,
	THREAD_PRIO_LEVEL+3,
	&scheduler_id) );
  trace_show_current_time("[scheduler creado]");


  // Create a scheduled thread
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_APP) );
  CHK( pthread_attr_setappscheduler (&attr, scheduler_id) );
  param.sched_priority = THREAD_PRIO_LEVEL;
  CHK( pthread_attr_setschedparam (&attr, &param) );
	

  CHK (pthread_create (&th, &attr, scheduled_thread, NULL));
  trace_show_current_time("[thread creado]");

	
  // allow threads to execute
  pthread_join (th, NULL);

  trace_show_current_time("[final del main]");
  printf("Test OK\n");
  return 0;
}

