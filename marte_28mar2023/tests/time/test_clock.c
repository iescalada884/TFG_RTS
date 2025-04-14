//  Test for all architectures
/*
 * test_clock.c
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

int main ()
{
  struct timespec ts1, ts2;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  // get clock_gettime time
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts1) );
  printf("clock_gettime 1: %f\n", timespec_to_double(&ts1));

  // get clock_gettime time
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts2) );
  printf("clock_gettime 2: %f\n", timespec_to_double(&ts2));

  assert(smaller_timespec (&ts1, &ts2));

  printf("Test OK\n");
  return 0;
}
