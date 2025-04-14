//  Test for all architectures
/*
 * test_sleep.c
 *
 * Check the actual slept time is close to the desired slept time.
 *
 */

#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define SLEEPS_NUM 7
#define SLEEP_TIME 1
#define GET_OVERHEAD_LOOPS 100
#if MARTE_ARCHITECTURE == ARCH_XTRATUM
#  define ERROR_MARGIN 0.1
#else
#  define ERROR_MARGIN 0.02
#endif


int main ()
{
  struct timespec ts[SLEEPS_NUM+1];
  double diff;
  int i;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  // get clock_gettime time
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts[0]) );
  for(i=0; i<GET_OVERHEAD_LOOPS; i++) {
     CHKE( clock_gettime(CLOCK_MONOTONIC, &ts[1]) );
  }

  const double clock_gettime_overhead =
     (timespec_to_double(&ts[1]) - timespec_to_double(&ts[0]))
     / GET_OVERHEAD_LOOPS;
  printf("clock_gettime overhead (%f-%f): %f\n",
         timespec_to_double(&ts[1]),
         timespec_to_double(&ts[0]),
         clock_gettime_overhead);

  printf("Sleep time:%d sec\n", SLEEP_TIME);

  // get initial time
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts[0]) );

  // Do sleeps
  for (i=1; i<SLEEPS_NUM+1; i++) {
    // sleep
    sleep(SLEEP_TIME);

    printf("  After sleep #%d\n", i);

    // get partial time
    CHKE( clock_gettime(CLOCK_MONOTONIC, &ts[i]) );
  }
  
  // show initial time
  printf("Initial time: %f\n", timespec_to_double(&ts[0]));
  
  // check times
  for(i=1; i<SLEEPS_NUM+1; i++) {
    printf("Partial time %d:     %f\n", i, timespec_to_double(&ts[i]));

    // get difference
    diff = timespec_to_double(&ts[i]) - timespec_to_double(&ts[i-1]);
    printf("  Difference %d:     %f\n", i, diff);
    
    // remove read clock time
    diff -= clock_gettime_overhead;
    printf("  diff - overhead:  %f\n", diff);
    
    // check times
    assert(diff - SLEEP_TIME < ERROR_MARGIN);
  }

  printf("Test OK\n");
  return 0;
}
