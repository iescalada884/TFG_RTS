//  Test for all architectures
/*
 * Test thread sets and the use of group clocks in timers.
 *
 * Threads in the thread set executes the 'eat_time()' function in
 * order to consume an amount of time. A periodic timer based on the
 * group clock is armed and a thread is used to wait the signal. The
 * number of generated signals is counted and the consumed time is
 * compared with the time measured by the group clock.
 */
#include <stdio.h>
#include <signal.h>
#include <time.h>
#include <pthread.h>
#include <thread_sets.h>
#include <sched.h>
#include <assert.h>
#include <errno.h>
#include <misc/error_checks.h>
#include <misc/load_loop.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define TEST_NAME "Timer based on Group clock test"

typedef struct {
  float time_to_eat;
} eat_data_t;

#define TIME_UNIT_SEC 0.1
#define TIMER_PERIOD  (1.7*TIME_UNIT_SEC)
#define EAT_T1        (1*TIME_UNIT_SEC)
#define EAT_T2        (4*TIME_UNIT_SEC)
#define EAT_MAIN      (1*TIME_UNIT_SEC)
#define MY_SIGNUM    SIGUSR1
#define LOOPS_IN_MAIN 3

volatile int signal_count = 0;
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

// sig_waiter
void * sig_waiter (void *arg)
{
  int received_sig;
  struct timespec ts;
  sigset_t set;
  sigemptyset (&set);
  sigaddset (&set, MY_SIGNUM);

  // wait signal
  while (1) {
    CHKE (sigwait (&set, &received_sig));
    signal_count++;

    CHKE (clock_gettime(*(clockid_t *)arg, &ts));
    printf ("Received sig:%d (count:%d at %dsec%dns)",
	    received_sig, signal_count, ts.tv_sec, ts.tv_nsec);
  }
  return NULL;
}



/* main thread */
int main ()
{
  pthread_t t1, t2, t3;
  pthread_attr_t attr;
  struct sched_param param;
  int policy;
  marte_thread_set_t thread_set;
  eat_data_t eat_data1, eat_data2;
  sigset_t set;
  struct sigevent event;
  timer_t timer_id;
  struct itimerspec timerdata;
  clockid_t gclk;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  printf ("---------     "TEST_NAME"    ------------\n");

  // Set signal mask for main thread and to be inherited by its children
  sigemptyset (&set);
  sigaddset (&set, MY_SIGNUM);
  CHK (pthread_sigmask (SIG_BLOCK, &set, NULL));

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

  // Create sigwaiter thread (high prio than the rest)
  CHK (pthread_attr_init (&attr));
  CHK (pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED));
  CHK (pthread_attr_setschedpolicy (&attr, policy));
  param.sched_priority = param.sched_priority + 1;
  CHK (pthread_attr_setschedparam (&attr, &param));
  CHK (pthread_create (&t3, &attr, sig_waiter, &gclk));

  /* create thread set */
  CHK (marte_threadset_create (&thread_set));
  CHK (marte_threadset_add (thread_set, t1));
  CHK (marte_threadset_add (thread_set, t2));

  // get group clock
  CHK (marte_getgroupcpuclockid(thread_set, &gclk));

  // Create timer
  event.sigev_notify = SIGEV_SIGNAL;
  event.sigev_signo = MY_SIGNUM;
  CHKE (timer_create (gclk, &event, &timer_id));

  /* Arm periodic timer */
  timerdata.it_interval.tv_sec = (int)TIMER_PERIOD;
  timerdata.it_interval.tv_nsec = (TIMER_PERIOD - (int)TIMER_PERIOD) * 1E9;
  timerdata.it_value.tv_sec = timerdata.it_interval.tv_sec;
  timerdata.it_value.tv_nsec = timerdata.it_interval.tv_nsec;
  CHKE (timer_settime (timer_id, 0, &timerdata, NULL));

  adjust();
  /* eat time */
  int i;
  for (i = 0; i < LOOPS_IN_MAIN; i++) {
    printf ("Main eating %f... ", EAT_MAIN);
    eat (EAT_MAIN);
    printf ("done\n");
    sched_yield();
  }

  // get group clock consumed time
  struct timespec ts;
  CHKE (clock_gettime(gclk, &ts));
  float clock_consumed_time = ts.tv_sec + ts.tv_nsec / 1E9;

  // print results
  printf ("Results: %d signals (Period:%dsec%dnsec)\n", signal_count,
          timerdata.it_interval.tv_sec, timerdata.it_interval.tv_nsec);
  printf ("Time consumed by threads in the group: %f\n", consumed_time);
  float expected_signal_count = consumed_time/TIMER_PERIOD;
  printf ("Signals expected (consumed_time/timer period):%f\n",
	  expected_signal_count);
  printf ("Group clock consumed time:%f\n", clock_consumed_time);

  // test OK:  expected signals - signal_count <= 1 and
  //           expected consumed time ~ clock_consumed_time
  assert(expected_signal_count - signal_count <= 1);
  assert((consumed_time - clock_consumed_time) <
	 clock_consumed_time * 0.05);

  printf("Test OK\n");
  return 0;
}
