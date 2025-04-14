//  Test for all architectures
/*
 * Test that a thread created with higher priority preempts the main thread.
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

#define PRIO_HIGH 70
#define PRIO_MED 50

volatile int step_counter = 0;

// thread 1
void *th_high_prio_body (void *arg)
{
  int i, j;

  // (1) thread with high priority starts
  printf("  thread with high priority starts\n");
  assert (step_counter == 1);
  step_counter++;

  // Run for a while
  for(i=0; i<10000; i++) {
    for(j=0; j<2000; j++) {
    }
  }

  // (2) thread with high priority finishes
  printf("  thread with high priority finishes\n");
  assert (step_counter == 2);
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
  CHK( pthread_setschedprio (pthread_self(), PRIO_MED) );

  // (0) Create high prio thread
  printf ("Main thread create high prio thread\n");
  assert (step_counter == 0);
  step_counter++;
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_FIFO) );
  param.sched_priority = PRIO_HIGH;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&th1, &attr, th_high_prio_body, &th1) );

  // Main thread is preempted ...

  // (3) Main thread executes after high prio thread finishes
  printf ("Main thread finishes\n");
  assert (step_counter == 3);

  printf("Test OK\n");
  return 0;
}
