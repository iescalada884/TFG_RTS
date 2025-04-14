//  Test for all architectures
/*
 * test_cpu_clocks.c
 *
 * The sum of the time executed by two threads must be equal to elapsed
 * time.
 *
 */

#include <stdio.h>
#include <time.h>
#include <sched.h>
#include <pthread.h>
#include <math.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define LOOPS_MAIN 100
const double ERROR_MARGIN = 0.001;
// const struct timespec ONE_DSEC = {0,100000000};
// const struct timespec TEN_MSEC = {0, 10000000};
// const struct timespec TEN_USEC = {0,    10000};

// flag thread has executed
volatile int thread_has_executed = 0;


static void show_times(char *msj,
		       const struct timespec * ts_mono,
		       const struct timespec * ts_rt,
		       const struct timespec * ts_cpu,
		       const struct timespec * ts_cpu_th,
		       const struct timespec * ts_cpu_sum)
{
  printf("%s\n", msj);
  printf("   Monotonic       :%f\n", timespec_to_double(ts_mono));
  printf("   Realtime        :%f\n", timespec_to_double(ts_rt));
  printf("   Main CPU time   :%f\n", timespec_to_double(ts_cpu));
  printf("   Thread CPU time :%f\n", timespec_to_double(ts_cpu_th));
  printf("   CPU times sum   :%f\n", timespec_to_double(ts_cpu_sum));
}

void * thread_body(void *arg)
{
  struct timespec ts;
  int j;
  CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts) );

  // initial CPU time should be close to 0
  printf(" Initial time of second thread:%f\n", timespec_to_double(&ts));
  assert(fabs(timespec_to_double(&ts)) < 0.01);

  thread_has_executed = 1;

  while (1) {
    // eat some CPU time
    for (j=0; j<1000000; j++);

    // yields CPU to main
    sched_yield();
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
  int i, j;
  pthread_t th;
  pthread_attr_t attr;
  clock_t th_cpu_clock;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  // get initial times
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_mono_1) );
  CHKE( clock_gettime(CLOCK_REALTIME, &ts_rt_1) );
  CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts_cpu_1) );

  // initial CPU time should be close to 0
  printf(" Initial time of main:%f\n", timespec_to_double(&ts_cpu_1));
  assert(fabs(timespec_to_double(&ts_cpu_1)) < 0.1);

  // create the second thread
  CHK( pthread_attr_init(&attr) );
  CHK( pthread_attr_setinheritsched(&attr, PTHREAD_INHERIT_SCHED) ); 
  CHK( pthread_create(&th, &attr, thread_body, NULL) );
  CHK( pthread_getcpuclockid(th, &th_cpu_clock) );

  // initial CPU time of thread should be 0
  CHKE( clock_gettime(th_cpu_clock, &ts_cpu_th_1) );
  add_timespec (&ts_cpu_sum, &ts_cpu_1, &ts_cpu_th_1);
  show_times("Initial times", &ts_mono_1, &ts_rt_1, &ts_cpu_1, &ts_cpu_th_1,
	     &ts_cpu_sum);
  assert((ts_cpu_th_1.tv_sec==0) && (ts_cpu_th_1.tv_nsec==0));

  for (i=0; i<LOOPS_MAIN; i++) {
    // eat some CPU time
    for (j=0; j<1000000; j++);

    // yields CPU to the second thread
    sched_yield();

    // get current times
    CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_mono_2) );
    CHKE( clock_gettime(CLOCK_REALTIME, &ts_rt_2) );
    CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts_cpu_2) );
    CHKE( clock_gettime(th_cpu_clock, &ts_cpu_th_2) );
    add_timespec (&ts_cpu_sum, &ts_cpu_2, &ts_cpu_th_2);
    show_times("Partial times", &ts_mono_2, &ts_rt_2, &ts_cpu_2, &ts_cpu_th_2,
	       &ts_cpu_sum);

    // get differences
    decr_timespec(&ts_mono_2, &ts_mono_1);
    decr_timespec(&ts_rt_2, &ts_rt_1);
    decr_timespec(&ts_cpu_2, &ts_cpu_1);
    decr_timespec(&ts_cpu_th_2, &ts_cpu_th_1);
    add_timespec (&ts_cpu_sum, &ts_cpu_2, &ts_cpu_th_2);
    show_times("Differences", &ts_mono_2, &ts_rt_2, &ts_cpu_2, &ts_cpu_th_2,
	       &ts_cpu_sum);

    // check differences
    decr_timespec(&ts_rt_2, &ts_mono_2);
    decr_timespec(&ts_cpu_sum, &ts_mono_2);
    assert(fabs(timespec_to_double(&ts_rt_2)) < ERROR_MARGIN);
    assert(fabs(timespec_to_double(&ts_cpu_sum)) < ERROR_MARGIN);
        
  }


  // check second thread has executed
  assert(thread_has_executed);

  printf("Test OK\n");
  return 0;
}
