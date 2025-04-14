/* 
 * Periodic threads using 'clock_nanosleep()'.
 *
 */
#include <stdio.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <malloc.h>
#include <misc/timespec_operations.h>
#include <misc/error_checks.h>

/* Periodic thread */
void * periodic (void *arg)
{
  int activation_count = 0;
  struct timespec my_period, next_activation;

  my_period.tv_sec = * (int*)arg;
  my_period.tv_nsec = 0; // You could also specify a number of nanoseconds
  
  if (clock_gettime(CLOCK_MONOTONIC, &next_activation))
    printf ("Error in clock_realtime\n");

  // Do "useful" work and wait until next period
  while (1) {
    incr_timespec (&next_activation, &my_period);

    printf ("Thread with period %ds activation %d\n",
	    my_period.tv_sec, activation_count++);

    if (clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
			&next_activation, NULL))
      printf("Error in clock_nanosleep");
  }
}

// Main thread. It creates the periodic threads
int main ()
{
  pthread_t th;
  pthread_attr_t attr;
  int *periods;
  int th_number, i;

  // Get number of threads
  printf ("How many periodic threads do you want to create...");
  scanf("%d", &th_number);
  
  // Create period of threads
  periods = (int *) malloc (sizeof(int)*th_number);
  for (i=0; i<th_number; i++)
    periods[i] = i+1;

  // Create threads with default scheduling parameters (SCHED_FIFO)
  CHK( pthread_attr_init (&attr) );

  for (i=0; i<th_number; i++)
     CHK( pthread_create (&th, &attr, periodic, &periods[i]) );
     
  // Allows threads to execute for a while
  sleep (22);
  printf ("Main thread finishing after 22 seconds\n"); 
  return 0;
}
