//  Test for all architectures
/*
 * The test launch several round robin threads and allow them to execute for a
 * while. At the end it checks it the number of context switches is the
 * expected
 *
 */
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <sched.h>
#include <math.h>
#include <misc/error_checks.h>
#include <assert.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define SLEEP_TIME 1 // seconds
#define RR_THREAD_NUM 5 // create this number of RR threads
#define RR_PRIO 10

volatile int cs = 0; // check to know if a context switch has happend
volatile int cs_count = 0;

// round robin thread
void *rr_thread (void *arg)
{
  int id = (int) arg;

  while (1) {
    if (cs != id) {
      // context switch
      cs = id;
      cs_count++;
    }
  }
}

int main()
{
  pthread_t th;
  pthread_attr_t attr;
  struct sched_param param;
  int i;
  struct timespec rr_interval;
  float rr_interval_sec;
  int expected_context_switch_count;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  // set main priority higher than RR threads priority
  CHK( pthread_setschedprio (pthread_self(), RR_PRIO+1) );

  // Create round-robin threads
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_RR) );
  param.sched_priority = RR_PRIO;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  for (i=0; i<RR_THREAD_NUM; i++) {
    CHK( pthread_create (&th, &attr, rr_thread, (void *) i) );
  }

  printf ("Main thread suspends for %dsec\n", SLEEP_TIME);
  sleep (SLEEP_TIME);
  printf ("Main thread finishes suspension\n");

  // get round robin interval
  CHKE( sched_rr_get_interval(0, &rr_interval) );
  rr_interval_sec = rr_interval.tv_sec + rr_interval.tv_nsec / 1000000000.0;

  expected_context_switch_count = (int)(SLEEP_TIME/rr_interval_sec);
  printf ("Context switch count:%d\n", cs_count);
  printf("Expected context switch count:%d", expected_context_switch_count);
  printf(" (test duration:%d sec, RR slice:%f sec)\n",
	 SLEEP_TIME, rr_interval_sec);

  // check if the number of context switches is the expected
  assert (fabs (expected_context_switch_count - cs_count)<=1);

  printf("Test OK\n");
  return 0;
}
