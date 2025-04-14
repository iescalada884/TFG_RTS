//  Test for all architectures
/*
 * Tests the tasks executes acording to their deadlines.
 *
 * TEST_NAME and TEST_CLOCK must be defined to compile this code (defined in
 * test_edf_deadlines_clock_realtime and test_edf_deadlines_clock_monotonic)
 */
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <pthread.h>
#include <sched.h>
#include <math.h>
#include <misc/error_checks.h>
#include <assert.h>
#include <sys/marte_configuration_parameters.h>
#include <misc/timespec_operations.h>
#include <misc/load.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

/*
 * Global data
 */
#define NS_PER_SEC 1000000000

#define EDF_THREAD_NUM 4 // create this number of EDF threads
#define EDF_PRIO 10

#define Thread_LOOPS 4
#define EXEC_TIME_NS 0.07 * NS_PER_SEC
#define EAT_PIECES 3

#define TEST_DURATION_SEC 3

double start_time;

/*
 * Thread ids
 */
pthread_t thread_ids[EDF_THREAD_NUM];

/*
 * Thread state
 */
volatile int active[EDF_THREAD_NUM] = {0,0,0,0};

/*
 * Deadlines
 */
struct timespec rel_deadlines[EDF_THREAD_NUM] =
  {{0, EXEC_TIME_NS*1},
   {0, EXEC_TIME_NS*7},
   {0, EXEC_TIME_NS*9},
   {0, EXEC_TIME_NS*13}};
struct timespec abs_deadlines[EDF_THREAD_NUM];

/*
 * Look for the active thread with earliest deadline.
 * Return: index of the thread in range [0..EDF_THREAD_NUM)
 */
static int earliest_deadline()
{
  int earliest_id=-1;
  struct timespec min = {INT32_MAX, 0}; // very long value
  struct timespec ts;
  int i;

  for(i=0; i<EDF_THREAD_NUM; i++) {
    CHK( pthread_getdeadline(thread_ids[i], TEST_CLOCK, &ts) );
    if (active[i] && smaller_timespec(&ts, &min)) {
	earliest_id = i;
	min=ts;
    }
  }
  assert(earliest_id != -1);
  return earliest_id;
}

/*
 * Check if diference between both timespec values is small enough
 */
static int small_diference(const struct timespec *ts1,
			   const struct timespec *ts2)
{
  const struct timespec small = {0, 10E6}; // 10 ms
  struct timespec diff;

  if (smaller_timespec(ts1, ts2)) {
    diff = *ts2;
    decr_timespec(&diff, ts1);
  } else {
    diff = *ts1;
    decr_timespec(&diff, ts2);
  }

  return smaller_timespec(&diff, &small);
}

/*
 * Execution times
 */
const float exec_time[EDF_THREAD_NUM] =
  {0.25 * EXEC_TIME_NS/NS_PER_SEC,  1.5 * EXEC_TIME_NS/NS_PER_SEC,
   2.0 * EXEC_TIME_NS/NS_PER_SEC,   2.0 * EXEC_TIME_NS/NS_PER_SEC};

/*
 * EDF thread
 */
void *edf_thread (void *arg)
{
  int id = (int) arg;
  struct timespec now;
  struct timespec next_activation, actual_deadline;
  int i;

  CHKE( clock_gettime(TEST_CLOCK, &now) );
  next_activation=now;

  // loop
  while (1) {
    active[id]=1;
    printf("Task %d starts activation\n", id);

    //  eats CPU time

    for(i=0; i<EAT_PIECES; i++) {
      assert (id==earliest_deadline());
      eat(exec_time[id]/EAT_PIECES);
      assert (id==earliest_deadline());
    }

    // check expected deadline is equal to actual deadline
    CHK( pthread_getdeadline(pthread_self(), TEST_CLOCK, &actual_deadline) );
    printf("   Task %d expected deadline:%1.3f (actual deadline:%1.3f)\n",
           id,
           timespec_to_double(&abs_deadlines[id]) - start_time,
	   timespec_to_double(&actual_deadline) - start_time);
    assert (small_diference(&abs_deadlines[id], &actual_deadline));

    // set new deadline and wait for next period

    incr_timespec(&next_activation, &rel_deadlines[id]);
    add_timespec(&abs_deadlines[id], &next_activation, &rel_deadlines[id]);
    printf("Task %d finish activation. Sleeps until %1.3f (deadline:%1.3f)\n",
           id,
	   timespec_to_double(&next_activation) - start_time,
	   timespec_to_double(&abs_deadlines[id]) - start_time);
    active[id]=0;
    CHK( pthread_setdeadline(pthread_self(), &abs_deadlines[id],
			     TEST_CLOCK, 0) );

    CHK( clock_nanosleep(TEST_CLOCK, TIMER_ABSTIME,
			 &next_activation, NULL) );
  }
}

int main()
{
  pthread_attr_t attr;
  struct sched_param param;
  struct timespec start_time_ts;
  int i;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  printf ("  --------------- "TEST_NAME" ---------------  \n");

  // adjust CPU eat function
  adjust();

  // get current time
  CHKE( clock_gettime(TEST_CLOCK, &start_time_ts) );
  start_time = timespec_to_double(&start_time_ts);

  // set main priority higher than EDF threads priority
  CHK( pthread_setschedprio (pthread_self(), EDF_PRIO+1) );

  // Create EDF threads
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_EDF) );
  param.sched_priority = EDF_PRIO;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  for (i=0; i<EDF_THREAD_NUM; i++) {
    CHK( pthread_attr_setreldeadline(&attr, &rel_deadlines[i]) );
    add_timespec(&abs_deadlines[i], &start_time_ts, &rel_deadlines[i]);
    CHK( pthread_create (&thread_ids[i], &attr, edf_thread, (void *) i) );
  }

  printf ("Main thread suspends for %dsec\n", TEST_DURATION_SEC);
  sleep (TEST_DURATION_SEC);
  printf ("Main thread finishes suspension\n");


  printf("Test OK\n");
  return 0;
}
