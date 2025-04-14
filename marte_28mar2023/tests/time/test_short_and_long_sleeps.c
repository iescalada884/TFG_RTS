//  Test for all architectures
/*
 * test_short_and_long_sleeps.c
 *
 * Check the actual slept time is close to the desired slept time.
 * One sleep time is short and the other long to be sure the timer
 * is reprogrammed several times (in architectures where the timer
 * has to be reprogrammed).
 * Uses the nanosleep function and tests relative and absolute times.
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
#  define ERROR_MARGIN 0.1
#else
#  define ERROR_MARGIN 0.005
#endif

#if MARTE_ARCHITECTURE == ARCH_XTRATUM
struct timespec short_sleep_time = {0, 500000000};
#else
struct timespec short_sleep_time = {0,  10000000};
#endif
struct timespec long_sleep_time =  {3, 500000000};

void make_measure(int abs_or_rel, struct timespec *sleep_time);

int main ()
{

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif
  
  make_measure(TIMER_ABSTIME, &long_sleep_time);  
  make_measure(0, &long_sleep_time);   
  make_measure(TIMER_ABSTIME, &short_sleep_time); 
  make_measure(0, &short_sleep_time);

  printf("Test OK\n");
  return 0;
}

void make_measure(int abs_or_rel, struct timespec *sleep_time) {
  struct timespec ts_ini, ts_fin, next_activation;
  double ini_time_double, fin_time_double;
  double diff;
  const double sleep_time_double = timespec_to_double(sleep_time);
  
  if (abs_or_rel == TIMER_ABSTIME) {
    printf("ABS ");
  } else {
    printf("REL ");
  }
  printf("nanosleep. Sleep time: %f\n", sleep_time_double);
  
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_ini) );

  if (abs_or_rel == TIMER_ABSTIME) {
    next_activation = ts_ini;
    incr_timespec (&next_activation, sleep_time);
  } else {
    next_activation = *sleep_time;
  }

  CHK( clock_nanosleep(CLOCK_MONOTONIC, abs_or_rel, &next_activation, NULL) );

  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_fin) );

  ini_time_double = timespec_to_double(&ts_ini);
  fin_time_double = timespec_to_double(&ts_fin);
  printf("  Initial time: %f\n", ini_time_double);  
  printf("  Final time  : %f\n", fin_time_double);  
  printf("  Sleep time  : %f\n", fin_time_double - ini_time_double);
  diff = fin_time_double - ini_time_double - sleep_time_double;  
  printf("  Difference  : %f (Margin:%f)\n", diff, ERROR_MARGIN);

  // check difference
  assert(diff >= 0.0);
  assert(diff < ERROR_MARGIN);
}
