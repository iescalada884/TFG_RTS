//  Test for all architectures
/*
 * Test a dynamic priority change on a suspended thread works OK: the thread
 * once it finishes its suspension preempts a lower priority thread
 *
 */
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <sched.h>
#include <misc/error_checks.h>
#include <assert.h>
#include <sys/marte_configuration_parameters.h>
#include <misc/load.h>

#if MARTE_ARCHITECTURE == ARCH_X86
// for testing on x86
# include <drivers/console_switcher.h>
#endif

#define TEST_NAME "Test dinamic priority change on a blocked thread"

#define DELAY_TIME_SEC 1

#define PRIO_HIGH 70
#define PRIO_MED  69
#define PRIO_LOW  68

volatile int step_counter = 0;

// thread 1
void *th1_body (void *arg)
{
  struct sched_param param;
  int policy;

  // (1) th1 starts with low priority
  assert (step_counter == 1);
  printf("th1 starts with low priority\n");
  CHK( pthread_getschedparam(pthread_self (), &policy, &param) );
  assert (policy == SCHED_FIFO);
  assert (param.sched_priority == PRIO_LOW);

  // (1->2) sleeps for a while
  printf ("th1 sleeps for a while\n");
  step_counter++;  // 2
  sleep (DELAY_TIME_SEC * 2);

  // (3->4) wake up with higher priority
  assert (step_counter == 3);

  printf ("th1 wakes up with higher priority and finishes before main\n");

  CHK( pthread_getschedparam(pthread_self (), &policy, &param) );
  assert (policy == SCHED_FIFO);
  assert (param.sched_priority == PRIO_HIGH);
  step_counter++;  // 4

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

  printf ("  ------------ "TEST_NAME" ------------  \n");

  // adjust CPU eat function
  adjust();

  // (0) set main priority (med prio)
  assert (step_counter == 0);
  CHK( pthread_setschedprio (pthread_self(), PRIO_MED) );

  // (0) Create thread 1 (low prio)
  printf ("Main thread create thread 1\n");
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_FIFO) );
  param.sched_priority = PRIO_LOW;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  CHK( pthread_create (&th1, &attr, th1_body, NULL) );

  // (0->1) Allow th1 to execute a little bit
  assert (step_counter == 0);
  printf ("Main allows th1 to execute a little bit\n");
  step_counter++; // 1
  sleep (DELAY_TIME_SEC);

  // (2->3) Main thread raises priority of th1 while it is sleeping
  assert (step_counter == 2);
  printf ("Main thread raises priority of th1 while it is sleeping\n");
  step_counter++; // 3
  CHK( pthread_setschedprio (th1, PRIO_HIGH) );

  // (3) Main thread executes while th1 is sleeping
  assert (step_counter == 3);
  printf ("Main thread executes while th1 is sleeping\n");
  eat (DELAY_TIME_SEC * 3);

  // (4) Main thread finishes after th1
  assert (step_counter == 4);
  printf ("Main thread finishes\n");

  printf("Test OK\n");
  return 0;
}
