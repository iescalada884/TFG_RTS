//  Test for all architectures
/*
 * Test timed handlers based on a group clock.
 *
 * Threads in the thread set executes the 'eat_time()' function in
 * order to consume and amount of time. A periodic timed handler is
 * used to control time consumed by threads in group. The number of
 * executions of the handler is counted and the consumed time is
 * compared with the time measured by the group clock.
 */
#include <stdio.h>
#include <signal.h>
#include <time.h>
#include <pthread.h>
#include <thread_sets.h>
#include <timed_handlers.h>
#include <sched.h>
#include <assert.h>
#include <errno.h>
#include <misc/error_checks.h>
#include <misc/load_loop.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define TEST_NAME "Timed handlers based on group clock test"

typedef struct {
  float time_to_eat;
} eat_data_t;

#define TIME_UNIT_SEC 0.1
#define HANDLER_PERIOD  (1.7*TIME_UNIT_SEC)
#define EAT_T1        (1*TIME_UNIT_SEC)
#define EAT_T2        (4*TIME_UNIT_SEC)
#define EAT_MAIN      (1*TIME_UNIT_SEC)
#define LOOPS_IN_MAIN 3

const struct timespec handler_period_ts =
  {(int)HANDLER_PERIOD, (HANDLER_PERIOD - (int)HANDLER_PERIOD) * 1E9};

volatile int handler_count = 0;
volatile float consumed_time = 0;

/* Thread that eats time */
void * eat_time (void *arg)
{
  while (1) {
    printf ("Thread eating %f... ", ((eat_data_t *)arg)->time_to_eat);
    eat (((eat_data_t *)arg)->time_to_eat);
    printf ("done\n");
    consumed_time += ((eat_data_t *)arg)->time_to_eat;
    sched_yield();
  }
  return NULL;
}

// handler
void handler_code (void *area, marte_timed_handler_t *th)
{
  struct timespec ts;

  handler_count++;

  CHKE (clock_gettime(*(clockid_t *)area, &ts));
  printf ("\nhandler: count:%d at %dsec%dns\n",
	  handler_count, ts.tv_sec, ts.tv_nsec);

  /* Set timed handler */
  CHK (marte_timed_handler_set (th, 0, &handler_period_ts));
}



/* main thread */
int main ()
{
  pthread_t t1, t2;
  pthread_attr_t attr;
  struct sched_param param;
  int policy;
  marte_thread_set_t thread_set;
  eat_data_t eat_data1, eat_data2;
  marte_timed_handler_t th;
  clockid_t gclk;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  printf ("---------     "TEST_NAME"    ------------\n");

  // get sched params of main thread
  CHK (pthread_getschedparam (pthread_self(), &policy, &param));

  // set main sched params for the other threads
  CHK (pthread_attr_init (&attr));
  CHK (pthread_attr_setinheritsched (&attr, PTHREAD_INHERIT_SCHED));

  /* Create first thread */
  eat_data1.time_to_eat = EAT_T1;
  CHK (pthread_create (&t1, &attr, eat_time, &eat_data1));

  /* Create second thread */
  eat_data2.time_to_eat = EAT_T2;
  CHK (pthread_create (&t2, &attr, eat_time, &eat_data2));

  /* create thread set */
  CHK (marte_threadset_create (&thread_set));
  CHK (marte_threadset_add (thread_set, t1));
  CHK (marte_threadset_add (thread_set, t2));

  /* get group clock */
  CHK (marte_getgroupcpuclockid(thread_set, &gclk));

  /* Create timed handler */
  CHK (marte_timed_handler_init (&th, gclk,
				 handler_code, &gclk, sizeof(gclk)));

  /* Set timed handler */
  CHK (marte_timed_handler_set (&th, 0, &handler_period_ts));

  adjust();
  /* eat time */
  int i;
  for (i = 0; i < LOOPS_IN_MAIN; i++) {
    printf ("Main eating %f... ", EAT_MAIN);
    eat (EAT_MAIN);
    printf ("done\n");
    sched_yield();
  }

  // destroy the handler
  CHK (marte_timed_handler_destroy (&th));

  // get group clock consumed time
  struct timespec ts;
  CHKE (clock_gettime(gclk, &ts));
  float clock_consumed_time = ts.tv_sec + ts.tv_nsec / 1E9;

  // print results
  printf ("Results: %d handlers (Period:%dsec%dnsec)\n", handler_count,
          handler_period_ts.tv_sec, handler_period_ts.tv_nsec);
  printf ("Time consumed by threads in the group: %f\n", consumed_time);
  float expected_handler_count = consumed_time/HANDLER_PERIOD;
  printf ("Handlers expected (consumed_time/timer period):%f\n",
	  expected_handler_count);
  printf ("Group clock consumed time:%f\n", clock_consumed_time);

  // test OK:  expected signals - signal_count <= 1 and
  //           expected consumed time ~ clock_consumed_time
  assert(expected_handler_count - handler_count <= 1);
  assert((consumed_time - clock_consumed_time) <
	 clock_consumed_time * 0.05);

  printf("Test OK\n");
  return 0;
}
