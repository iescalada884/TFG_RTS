//  Test for all architectures
/*
 * test_cpu_clock_for_another_threads.c
 *
 * Form the main thread read the clock of another thread.
 *
 */

#include <stdio.h>
#include <time.h>
#include <sched.h>
#include <pthread.h>
#include <math.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <misc/load_loop.h>
#include <misc/timespec_operations.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define LOOPS_MAIN 5
#define TH_EAT_TIME 1.0
#define ERROR_MARGIN_S 0.015


void * thread_body(void *arg)
{
  struct timespec ts;  
  CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts) );
  printf("Thread initial CPU time :%f\n", timespec_to_double(&ts));
  assert(timespec_to_double(&ts) < 0.01);

  while (1) {
    // eat some CPU time
    eat(TH_EAT_TIME);

    // yields CPU to main
    sched_yield();
  }
    
  return NULL;
}


int main ()
{
  struct timespec ts_cpu_th_1, ts_cpu_th_2, ts_cpu_th_dif;
  pthread_t th;
  pthread_attr_t attr;
  clock_t th_cpu_clock;
  int i;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  adjust();

  // create the thread
  CHK( pthread_attr_init(&attr) );
  CHK( pthread_attr_setinheritsched(&attr, PTHREAD_INHERIT_SCHED) ); 
  CHK( pthread_create(&th, &attr, thread_body, NULL) );
  CHK( pthread_getcpuclockid(th, &th_cpu_clock) );

  // initial CPU time of thread should be 0
  CHKE( clock_gettime(th_cpu_clock, &ts_cpu_th_1) );
  printf("   Initial Thread CPU time :%f\n", timespec_to_double(&ts_cpu_th_1));
  assert((ts_cpu_th_1.tv_sec==0) && (ts_cpu_th_1.tv_nsec==0));

  for (i=0; i<LOOPS_MAIN; i++) {

    // yields CPU to the second thread
    sched_yield();

    // get current time
    CHKE( clock_gettime(th_cpu_clock, &ts_cpu_th_2) );

    // show current time and differences
    printf("Thread CPU time :%f\n", timespec_to_double(&ts_cpu_th_2));
    ts_cpu_th_dif = ts_cpu_th_2;
    decr_timespec(&ts_cpu_th_dif, &ts_cpu_th_1);
    printf("   Diff :%f\n", timespec_to_double(&ts_cpu_th_dif));

    assert(fabs(timespec_to_double(&ts_cpu_th_dif) - TH_EAT_TIME) < 
	   ERROR_MARGIN_S);

    // update time
    ts_cpu_th_1 = ts_cpu_th_2;
        
  }

  printf("Test OK\n");
  return 0;
}
