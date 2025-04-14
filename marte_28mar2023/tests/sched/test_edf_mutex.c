//  Test for all architectures
/*
 * Tests the tasks executes acording to their deadlines.
 *
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
#define EDF_PRIO 14

#define Thread_LOOPS 4
#define EXEC_TIME_NS 0.06 * NS_PER_SEC
#define EXEC_TIME_CRITICAL_SECTION_SEC 2 * EXEC_TIME_NS/NS_PER_SEC
#define EAT_PIECES 3

#define TEST_DURATION_SEC 3

double start_time;

static int thread_that_should_be_executing();

/*
 * Thread ids
 */
pthread_t thread_ids[EDF_THREAD_NUM];

/*
 * Thread parameters
 */
volatile int active[EDF_THREAD_NUM] = {0,0,0,0};

const struct timespec rel_deadlines[EDF_THREAD_NUM] =
  {{0, EXEC_TIME_NS*1.5}, {0, EXEC_TIME_NS*7},
   {0, EXEC_TIME_NS*9}, {0, EXEC_TIME_NS*13}};

const float exec_time[EDF_THREAD_NUM] =
  {0.25 * EXEC_TIME_NS/NS_PER_SEC,  2.0 * EXEC_TIME_NS/NS_PER_SEC,
   2.0 * EXEC_TIME_NS/NS_PER_SEC,   2.0 * EXEC_TIME_NS/NS_PER_SEC};

const short int preemtion_levels[EDF_THREAD_NUM] = {5,3,3,3};

#define MUTEX_PREEMTPION_LEVEL 3

/*
 * Shared mutex
 */
pthread_mutex_t mutex;
volatile int thread_in_mutex=-1;

void uses_mutex(int id) {
  int i;
  CHK( pthread_mutex_lock(&mutex) );
  thread_in_mutex=id;
  printf("Task %d enters critical section\n", id);
  for(i=0; i<EAT_PIECES; i++) {
    assert (id==thread_that_should_be_executing());
    eat(EXEC_TIME_CRITICAL_SECTION_SEC/EAT_PIECES);
    assert (id==thread_that_should_be_executing());
  }
  printf("Task %d leaves critical section\n", id);
  thread_in_mutex=-1;
  CHK( pthread_mutex_unlock(&mutex) );
}

/*
 * thread_that_should_be_executing()
 */
static int thread_that_should_be_executing()
{
  int th_id=-1;
  struct timespec min = {INT32_MAX, INT32_MAX}; // very long value
  struct timespec ts;
  int i;

  for(i=0; i<EDF_THREAD_NUM; i++) {
    CHK( pthread_getdeadline(thread_ids[i], CLOCK_MONOTONIC, &ts) );
    if ((i==thread_in_mutex && th_id!=0)
	|| (active[i] && smaller_timespec(&ts, &min))) {
      // if the thread is in the mutex it should be executing before
      // any other thread appart from the thread with id 0
      // if it is not in the mutex, then threads compite by deadline
      th_id = i;
      min=ts;
    }
  }
  assert(th_id != -1);
  return th_id;
}

/*
 * EDF threads
 */
void *edf_thread (void *arg)
{
  int id = (int) arg;
  struct timespec now;
  struct timespec next_activation, abs_deadline;

  CHKE( clock_gettime(CLOCK_MONOTONIC, &now) );
  next_activation=now;

  // loop
  while (1) {
    active[id]=1;
    printf("Task %d starts activation\n", id);

    if (id==1 || id==2) {
      //  use the mutex

      assert (id==thread_that_should_be_executing());
      uses_mutex(id);
      assert (id==thread_that_should_be_executing());

    } else {
      // don use the mutex: eats CPU time

      int i;
      for(i=0; i<EAT_PIECES; i++) {
	assert (id==thread_that_should_be_executing());
	eat(exec_time[id]/EAT_PIECES);
	assert (id==thread_that_should_be_executing());
      }
    }

    // set new deadline and wait for next period

    incr_timespec(&next_activation, &rel_deadlines[id]);
    add_timespec(&abs_deadline, &next_activation, &rel_deadlines[id]);
    printf("Task %d finish activation. Sleeps until %1.3f (deadline:%1.3f)\n",
           id,
	   timespec_to_double(&next_activation) - start_time,
	   timespec_to_double(&abs_deadline) - start_time);
    active[id]=0;
    CHK( pthread_setdeadline(pthread_self(), &abs_deadline, 
			     CLOCK_MONOTONIC, 0) );

    CHK( clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME,
			 &next_activation, NULL) );
  }
}

int main()
{
  pthread_attr_t attr;
  struct sched_param param;
  struct timespec ts;
  pthread_mutexattr_t mutexattr;
  int i;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  // adjust CPU eat function
  adjust();

  // get current time
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts) );
  start_time = timespec_to_double(&ts);

  // set main priority higher than EDF threads priority
  CHK( pthread_setschedprio (pthread_self(), EDF_PRIO+1) );

  // create mutex
  CHK( pthread_mutexattr_init(&mutexattr) );
  CHK( pthread_mutexattr_setprotocol(&mutexattr, PTHREAD_PRIO_PROTECT) );
  CHK( pthread_mutexattr_setprioceiling(&mutexattr, EDF_PRIO) );
  CHK( pthread_mutexattr_setpreemptionlevel(&mutexattr,
					    MUTEX_PREEMTPION_LEVEL) );
  CHK( pthread_mutex_init(&mutex, &mutexattr) );

  // Create EDF threads
  CHK( pthread_attr_init (&attr) );
  CHK( pthread_attr_setschedpolicy (&attr, SCHED_EDF) );
  param.sched_priority = EDF_PRIO;
  CHK( pthread_attr_setschedparam (&attr, &param) );
  for (i=0; i<EDF_THREAD_NUM; i++) {
    CHK( pthread_attr_setreldeadline(&attr, &rel_deadlines[i]) );
    CHK( pthread_attr_setpreemptionlevel(&attr, preemtion_levels[i]) );
    CHK( pthread_create (&thread_ids[i], &attr, edf_thread, (void *) i) );
  }

  printf ("Main thread suspends for %dsec\n", TEST_DURATION_SEC);
  sleep (TEST_DURATION_SEC);
  printf ("Main thread finishes suspension\n");


  printf("Test OK\n");
  return 0;
}
