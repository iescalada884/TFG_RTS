//  Test for all architectures
/*
 * test_interrupt_clock.c
 *
 * The sum of the time executed by the two threads plus the time executed by
 * the interrupt handlers must be equal to the elapsed time.
 *
 */

#include <stdio.h>
#include <time.h>
#include <sched.h>
#include <pthread.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define LOOPS_MAIN 5
const struct timespec ONE_DSEC = {0,100000000};
const struct timespec TEN_MSEC = {0, 10000000};
const struct timespec TEN_USEC = {0,    10000};

// flag thread has executed
volatile int thread_has_executed = 0;

// flag to make main thread execute once after the second thread
volatile int after_second_thread = 0;


static void show_times(char *msj,
		       const struct timespec * ts_mono,
		       const struct timespec * ts_rt,
		       const struct timespec * ts_cpu,
		       const struct timespec * ts_cpu_th,
		       const struct timespec * ts_cpu_sum,
		       const struct timespec * ts_cpu_int,
		       const struct timespec * ts_cpu_int_sum)
{
  printf("%s\n", msj);
  printf("   Monotonic        :%f\n", timespec_to_double(ts_mono));
  printf("   Realtime         :%f\n", timespec_to_double(ts_rt));
  printf("   Main CPU time    :%f\n", timespec_to_double(ts_cpu));
  printf("   Thread CPU time  :%f\n", timespec_to_double(ts_cpu_th));
  printf("   CPU times sum    :%f\n", timespec_to_double(ts_cpu_sum));
  printf("   Inter. CPU time  :%f\n", timespec_to_double(ts_cpu_int));
  printf("   All CPU times sum:%f\n", timespec_to_double(ts_cpu_int_sum));
}

void * thread_body(void *arg)
{
  const struct timespec sleep_time = {0, 5000000}; // 5 ms
  struct timespec ts;
  //int j;
  CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts) );

  // initial CPU time should be close to 0
  printf(" Initial time of second thread:%f\n", timespec_to_double(&ts));
  assert(smaller_timespec(&ts, &TEN_MSEC));

  thread_has_executed = 1;

  while (1) {
    // eat some CPU time
    //for (j=0; j<1000000; j++);

    // sleeps and yields CPU to main
    after_second_thread = 1;
    nanosleep(&sleep_time, NULL);
  }

  return NULL;
}


int main ()
{
  struct timespec ts_mono_1, ts_mono_2;
  struct timespec ts_rt_1, ts_rt_2;
  struct timespec ts_cpu_1, ts_cpu_2;
  struct timespec ts_cpu_th_1, ts_cpu_th_2;
  struct timespec ts_cpu_sum;
  struct timespec ts_cpu_int;
  struct timespec ts_cpu_int_sum;
  int i;
  //int j;
  pthread_t th;
  pthread_attr_t attr;
  struct sched_param params;
  clock_t th_cpu_clock;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  // get initial times
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_mono_1) );
  CHKE( clock_gettime(CLOCK_REALTIME, &ts_rt_1) );
  CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts_cpu_1) );
  if (clock_gettime(CLOCK_INTERRUPTS_CPUTIME, &ts_cpu_int) != 0) {
    // CLOCK_INTERRUPTS_CPUTIME not supported
    printf("CLOCK_INTERRUPTS_CPUTIME not supported\n");
    printf("   Test skipped\n");
    printf("Test OK");
  }

  // initial CPU time should be close to 0
  printf(" Initial time of main:%f\n", timespec_to_double(&ts_cpu_1));
  assert(smaller_timespec(&ts_cpu_1, &ONE_DSEC));

  // create the second thread
  CHK( pthread_attr_init(&attr) );
  CHK( pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED) );
  params.sched_priority = sched_get_priority_max(SCHED_FIFO);
  CHK( pthread_attr_setschedparam(&attr, &params) );
  CHK( pthread_create(&th, &attr, thread_body, NULL) );
  CHK( pthread_getcpuclockid(th, &th_cpu_clock) );

  // initial CPU time of thread should be 0
  CHKE( clock_gettime(th_cpu_clock, &ts_cpu_th_1) );
  add_timespec (&ts_cpu_sum, &ts_cpu_1, &ts_cpu_th_1);
  add_timespec (&ts_cpu_int_sum, &ts_cpu_int, &ts_cpu_sum);
  show_times("Initial times", &ts_mono_1, &ts_rt_1, &ts_cpu_1, &ts_cpu_th_1,
	     &ts_cpu_sum, &ts_cpu_int, &ts_cpu_int_sum);
  assert( smaller_timespec(&ts_cpu_th_1, &TEN_MSEC) );

  for (i=0; i<LOOPS_MAIN; i++) {
    // wait for the second thread
    while(!after_second_thread);
    after_second_thread=0;

    // eat some CPU time
    //for (j=0; j<1000000; j++);

    // get current times
    CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_mono_2) );
    CHKE( clock_gettime(CLOCK_REALTIME, &ts_rt_2) );
    CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts_cpu_2) );
    CHKE( clock_gettime(th_cpu_clock, &ts_cpu_th_2) );
    CHKE( clock_gettime(CLOCK_INTERRUPTS_CPUTIME, &ts_cpu_int) );
    add_timespec (&ts_cpu_sum, &ts_cpu_2, &ts_cpu_th_2);
    add_timespec (&ts_cpu_int_sum, &ts_cpu_int, &ts_cpu_sum);
    show_times("Partial times", &ts_mono_2, &ts_rt_2, &ts_cpu_2, &ts_cpu_th_2,
	       &ts_cpu_sum, &ts_cpu_int, &ts_cpu_int_sum);

    // get differences
    decr_timespec(&ts_mono_2, &ts_mono_1);
    decr_timespec(&ts_rt_2, &ts_rt_1);
    decr_timespec(&ts_cpu_2, &ts_cpu_1);
    decr_timespec(&ts_cpu_th_2, &ts_cpu_th_1);
    add_timespec (&ts_cpu_sum, &ts_cpu_2, &ts_cpu_th_2);
    add_timespec (&ts_cpu_int_sum, &ts_cpu_int, &ts_cpu_sum);
    show_times("Differences", &ts_mono_2, &ts_rt_2, &ts_cpu_2, &ts_cpu_th_2,
	       &ts_cpu_sum, &ts_cpu_int, &ts_cpu_int_sum);

    // check differences
    decr_timespec(&ts_rt_2, &ts_mono_2);
    decr_timespec(&ts_cpu_int_sum, &ts_mono_2);
    assert(smaller_timespec(&ts_rt_2,  &TEN_USEC));
    assert(smaller_timespec(&ts_cpu_int_sum, &TEN_USEC));

  }


  // check second thread has executed
  assert(thread_has_executed);

  printf("Test OK\n");
  return 0;
}
