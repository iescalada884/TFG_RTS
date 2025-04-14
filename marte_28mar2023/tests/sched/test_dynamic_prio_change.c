//  Test for all architectures
/*
 * Test for pthread_setschedparam and pthread_setschedprio and its different
 * behavior respect the new position (head or tail) of the thread in the ready
 * queue
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
#define PRIO_MED  69
#define PRIO_LOW  68

volatile int step_counter = 0;

// thread 1
void *th1_body (void *arg)
{
  struct sched_param param;
  int policy;
  pthread_t th2 = *((pthread_t *)arg);

  // (1) th1 starts with more priority than th2
  printf("th1 starts\n");
  assert (step_counter == 1);

  // (1->2) th1 calls pthread_setschedparam with t2's priority
  printf ("th1 calls pthread_setschedparam with t2's priority\n");
  CHK( pthread_getschedparam(th2, &policy, &param) );
  assert (policy == SCHED_FIFO);
  assert (param.sched_priority == PRIO_MED);
  step_counter++;
  CHK( pthread_setschedparam (pthread_self (), policy, &param) );

  // (3->4) lowers priority with pthread_setschedprio
  printf ("th1 lowers its priority with pthread_setschedprio\n");
  assert (step_counter == 3);
  step_counter++;
  CHK( pthread_setschedprio (pthread_self (), PRIO_LOW) );

  // (5->6) executes after th2 finishes and before main
  printf ("th1 finishes after th2 finishes and before main\n");
  assert (step_counter == 5);
  CHK( pthread_getschedparam(pthread_self (), &policy, &param) );
  assert (policy == SCHED_FIFO);
  assert (param.sched_priority == PRIO_LOW);
  step_counter++;

  return NULL;
}

// thread 2
void *th2_body (void *arg)
{
  struct sched_param param;
  int policy;
  pthread_t th1 = *((pthread_t *)arg);

  // (2) th2 starts after th1 lowers its priority
  printf("th2 starts after th1 changes its priority\n");
  assert (step_counter == 2);
  CHK( pthread_getschedparam(th1, &policy, &param) );
  assert(policy == SCHED_FIFO);
  assert(param.sched_priority == PRIO_MED);

  // (2) calls pthread_setschedprio with the current prio
  printf("th2 calls pthread_setschedprio with the current prio\n");
  CHK( pthread_getschedparam(pthread_self (), &policy, &param) );
  assert(policy == SCHED_FIFO);
  assert(param.sched_priority == PRIO_MED);
  CHK( pthread_setschedprio(pthread_self (), param.sched_priority) );
  assert (step_counter == 2);

  // (2->3) calls pthread_setschedparam with the current prio
  printf("th2 calls pthread_setschedparam with the current prio\n");
  CHK( pthread_getschedparam(pthread_self (), &policy, &param) );
  assert(policy == SCHED_FIFO);
  assert(param.sched_priority == PRIO_MED);
  step_counter++;
  CHK( pthread_setschedparam(pthread_self (), policy, &param) );

  // (4->5) executes after t1 lowers its priority
  printf("th2 finishes after t1 lowers its priority\n");
  assert (step_counter == 4);
  step_counter++;

  return NULL;
}

int main()
{
  pthread_t th1, th2;
  pthread_attr_t attr;
  struct sched_param param;

#if MARTE_ARCHITECTURE == ARCH_X86
        // for testing on x86
        SERIAL_CONSOLE_INIT();
#endif

  // (0) set main priority
  assert (step_counter == 0);
  CHK( pthread_setschedprio (pthread_self(), PRIO_HIGH) );

  // (0) Create thread 2 (med prio)
  printf ("Main thread create threads\n");
  assert (step_counter == 0);
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_FIFO) );
  param.sched_priority = PRIO_MED;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&th2, &attr, th2_body, &th1) );

  // (0) Create thread 1 (high prio)
  assert (step_counter == 0);
  param.sched_priority = PRIO_HIGH;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&th1, &attr, th1_body, &th2) );

  // (0->1) Main thread lowers prio to the lower priority level
  printf ("Main thread lowers prio to the lower priority level\n");
  assert (step_counter == 0);
  step_counter++;
  param.sched_priority = PRIO_LOW;
  CHK( pthread_setschedparam(pthread_self (), SCHED_FIFO, &param) );

  // (6) Main thread executes when the threads want it
  printf ("Main thread finishes\n");
  assert (step_counter == 6);

  printf("Test OK\n");
  return 0;
}
