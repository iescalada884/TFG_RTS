//  Test for all architectures
/*
 * Test that two threads at the same priority yields the processor to each
 * other.
 *
 */
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <sched.h>
#include <misc/error_checks.h>
#include <assert.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
// for testing on x86
#include <drivers/console_switcher.h>
#endif

#define PRIO 50

volatile int step_counter = 0;

// Other thread
void *th_other_body (void *arg)
{
  int i, j;

  // (1) Other thread starts
  printf("  Other thread starts\n");
  assert (step_counter == 1);
  step_counter++;

  // Run for a while
  for(i=0; i<10000; i++) {
    for(j=0; j<2000; j++) {
    }
  }

  // (2) Other thread yields
  printf("  Other thread yields CPU\n");
  assert (step_counter == 2);
  step_counter++;
  CHKE (sched_yield());

  // (4) Other thread finishes
  printf("  Other thread finishes\n");
  assert (step_counter == 4);
  step_counter++;

  return NULL;
}

int main()
{
  pthread_t th1;
  pthread_attr_t attr;
  struct sched_param param;

#if MARTE_ARCHITECTURE == ARCH_X86
        // for testing on x86
        SERIAL_CONSOLE_INIT();
#endif

  // (0) set main priority
  assert (step_counter == 0);
  CHK( pthread_setschedprio (pthread_self(), PRIO) );

  // (0) Create the other thread
  printf ("Main thread create the other thread\n");
  assert (step_counter == 0);
  step_counter++;
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_FIFO) );
  param.sched_priority = PRIO;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&th1, &attr, th_other_body, &th1) );

  // Main thread yields CPU ...
  printf ("Main thread yields CPU (1)\n");
  CHKE (sched_yield());

  // (3) Main thread executes after other thread yields
  printf ("Main thread yields CPU (2)\n");
  assert (step_counter == 3);
  step_counter++;
  CHKE (sched_yield());

  // (5) Main thread executes after other thread finishes
  printf ("Main thread finishes\n");
  assert (step_counter == 5);

  printf("Test OK\n");
  return 0;
}
