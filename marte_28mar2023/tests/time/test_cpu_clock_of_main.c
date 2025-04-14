//  Test for all architectures
/*
 * test_cpu_clock_of_main.c
 *
 * Simplest test for CPU clocks:
 *   - initial value of CPU clock of main must be close to zero.
 *   - time executed by main must be equal to elapsed time.
 *   - main task never leaves CPU
 *
 */

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <math.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define LOOPS_MAIN 100
#if MARTE_ARCHITECTURE == ARCH_XTRATUM
//                                             ms ns
const struct timespec ERROR_MARGIN_INI = {0, 60000000};
const double ERROR_MARGIN = 0.003;
#elsif MARTE_ARCHITECTURE == ARCH_RPI
//                                             ms ns
const struct timespec ERROR_MARGIN_INI = {0, 80000000};
const double ERROR_MARGIN = 0.002;
#else
//                                             ms ns
const struct timespec ERROR_MARGIN_INI = {0, 80000000};
const double ERROR_MARGIN = 0.002;
#endif


static void show_times(char *msj,
		       const struct timespec * ts_mono,
		       const struct timespec * ts_rt,
		       const struct timespec * ts_cpu)
{
  printf("%s\n", msj);
  printf("   Monotonic:%f\n", timespec_to_double(ts_mono));
  printf("   Realtime :%f\n", timespec_to_double(ts_rt));
  printf("   CPU time :%f\n", timespec_to_double(ts_cpu));
}


int main ()
{
  struct timespec ts_mono_1, ts_mono_2;
  struct timespec ts_rt_1, ts_rt_2;
  struct timespec ts_cpu_1, ts_cpu_2;
  int i, j;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif
  printf("Initial margin:%f\n", timespec_to_double(&ERROR_MARGIN_INI));
  printf("Error margin:%f\n", ERROR_MARGIN);

  // get initial times
  CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts_cpu_1) );
  CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_mono_1) );
  CHKE( clock_gettime(CLOCK_REALTIME, &ts_rt_1) );
  show_times("Initial times", &ts_mono_1, &ts_rt_1, &ts_cpu_1);

  // initial CPU time should be close to 0
  assert(smaller_timespec(&ts_cpu_1, &ERROR_MARGIN_INI));
  

  for (i=0; i<LOOPS_MAIN; i++) {
    // eat some CPU time
    for (j=0; j<1000000; j++);

    // get current times
    CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts_cpu_2) );
    CHKE( clock_gettime(CLOCK_MONOTONIC, &ts_mono_2) );
    CHKE( clock_gettime(CLOCK_REALTIME, &ts_rt_2) );
    show_times("Partial times", &ts_mono_2, &ts_rt_2, &ts_cpu_2);

    // get differences
    decr_timespec(&ts_mono_2, &ts_mono_1);
    decr_timespec(&ts_rt_2, &ts_rt_1);
    decr_timespec(&ts_cpu_2, &ts_cpu_1);
    show_times("Differences", &ts_mono_2, &ts_rt_2, &ts_cpu_2);

    // check differences
    decr_timespec(&ts_rt_2, &ts_mono_2);
    decr_timespec(&ts_cpu_2, &ts_mono_2);
    assert(fabs(timespec_to_double(&ts_rt_2))  < ERROR_MARGIN);
    assert(fabs(timespec_to_double(&ts_cpu_2)) < ERROR_MARGIN);
        
  }


  printf("Test OK\n");
  return 0;
}
