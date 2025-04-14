//  Test for all architectures
/*
 * test_nanosleep.c
 *
 * Check the actual slept time is close to the desired slept time.
 * Uses an absolute sleep.
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

#if MARTE_ARCHITECTURE == ARCH_XTRATUM
#  define SLEEPS_NUM 10
#  define ERROR_MARGIN 0.1
#else
#  define SLEEPS_NUM 50
#  define ERROR_MARGIN 0.01
#endif


int main ()
{
  struct timespec ts[SLEEPS_NUM+1];
  struct timespec next_activation;
#if MARTE_ARCHITECTURE == ARCH_XTRATUM
  const struct timespec sleep_time = {0, 500000000};
#else
  const struct timespec sleep_time = {0, 100000000};
#endif
  const double sleep_time_double = timespec_to_double(&sleep_time);
  double diff;
  int i;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  // get initial time
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts[0]) );
  next_activation = ts[0];
  const double initial_time = timespec_to_double(&ts[0]);

  // Do sleeps
  for (i=1; i<SLEEPS_NUM+1; i++) {
    incr_timespec (&next_activation, &sleep_time);
    
    printf("Sleep until:  %f\n", timespec_to_double (&next_activation));
    
    // sleep
    CHK( clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &next_activation, NULL) );

    // get partial time
    CHKE( clock_gettime(CLOCK_MONOTONIC, &ts[i]) );
    printf("  Woken up at:%f\n", timespec_to_double (&ts[i]));
  }
  
  // show initial time
  printf("Initial time: %f\n", timespec_to_double(&ts[0]));
  
  // check times
  for(i=1; i<SLEEPS_NUM+1; i++) {
    printf("Partial time %d:     %f\n", i, timespec_to_double(&ts[i]));

    // get difference with teorical time
    diff = timespec_to_double(&ts[i]) - sleep_time_double * i - initial_time;
    printf("  Difference %d:     %f\n", i, diff);
    
    // check times
    assert(diff < ERROR_MARGIN);
  }

  printf("Test OK\n");
  return 0;
}
