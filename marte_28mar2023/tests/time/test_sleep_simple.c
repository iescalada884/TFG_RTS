//  Test for all architectures
/*
 * test_sleep_simple.c
 *
 * Just check that a thread wakes up after a sleep operation.
 *
 */

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define SLEEP_TIME 1

int main ()
{

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  printf("Sleep time:%d sec\n", SLEEP_TIME);

  CHK (sched_yield());
  // when asserts are enabled sched_yield checks the task stack

  sleep(SLEEP_TIME);

  printf("  After sleep\n");

  CHK (sched_yield());

  printf("Test OK\n");
  return 0;
}
