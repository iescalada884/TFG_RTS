//  Test for all architectures
/*
 * Test that termination of a thread has no effect in the dormant main thread.
 *
 * (sched/test_two_threads_simple.c tests that main continues after the
 * termination of a higher priority thread).
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

volatile int step_counter = 0;

// thread 1
void *other_thread_body (void *arg)
{
  int i, j;

  // (2) thread with high priority starts
  printf("  In other thread\n");
  assert (step_counter == 2);
  step_counter++;

  // Run for a while
  for(i=0; i<10000; i++) {
    for(j=0; j<2000; j++) {
    }
  }

  // (3) other thread finishes
  printf("  Other thread finishes\n");
  assert (step_counter == 3);
  step_counter++;

  return NULL;
}

int main()
{
  pthread_t th1;

#if MARTE_ARCHITECTURE == ARCH_X86
        // for testing on x86
        SERIAL_CONSOLE_INIT();
#endif

  // (0) set main priority
  assert (step_counter == 0);

  // (0) Create high prio thread
  printf ("Main thread create other thread\n");
  assert (step_counter == 0);
  step_counter++;
  CHK( pthread_create (&th1, NULL, other_thread_body, NULL) );

  // (1) Main thread sleeps
  printf ("Main thread sleeps\n");
  assert (step_counter == 1);
  step_counter++;
  sleep(1);

  // (4) Main thread executes after suspension
  printf ("Main thread finishes\n");
  assert (step_counter == 4);

  printf("Test OK\n");
  return 0;
}
