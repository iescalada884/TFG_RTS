//  Test for all architectures
/*
 * test_load_loop.c
 *
 * Test the load_loop eat() function consumed the right time.
 *
 */
#include <assert.h>
#include <stdlib.h> // for exit in assert
#include <stdio.h>
#include <sched.h>
#include <time.h>
#include <misc/timespec_operations.h>
#include <misc/error_checks.h>
#include <misc/load_loop.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define EAT_TIME_BASE_NS 1e6

#define EATS_NUM 5
const int eat_time_factor[]={1, 2, 10, 100, 1000};

#define MARGIN 10.0 // percentage

struct timespec start_ts, end_ts, total_ts;

/*
 * main
 */
int main() {

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  // adjust eat
  adjust();
  
  printf("\nEAT_TIME_BASE_NS: %f  MARGIN: %f%% \n\n", EAT_TIME_BASE_NS, MARGIN);
  
  int i;
  float eat_time, total_time, dif_time, margin_time;
  for(i=0; i<EATS_NUM; i++) {
    eat_time = EAT_TIME_BASE_NS * eat_time_factor[i] / 1e9;
    printf("Eat time: %f\n", eat_time);
    
    clock_gettime(CLOCK_MONOTONIC, &start_ts);
    eat(eat_time);
    clock_gettime(CLOCK_MONOTONIC, &end_ts);
    
    total_ts = end_ts;
    decr_timespec(&total_ts, &start_ts);
    total_time = timespec_to_double(&total_ts);
    printf("total_ts: %f (start_ts: %f, end_ts: %f\n",
	    total_time,
	    timespec_to_double(&start_ts),
	   timespec_to_double(&end_ts));
    
    if (total_time > eat_time) {
      dif_time = total_time - eat_time;
    } else {
      dif_time = eat_time - total_time;
    }
    margin_time = total_time * MARGIN / 100.0;
      
    printf("Dif.:     %f\n", dif_time);      
    printf("Margin:   %f\n", margin_time);
    printf("\n");
    
    assert(dif_time < margin_time);
  }

  // Check the timer has expired all the times
  //assert(num_accepted_signals==NUM_SIGNAL_LOOPS);

  printf("Test OK\n");
  return 0;
}
