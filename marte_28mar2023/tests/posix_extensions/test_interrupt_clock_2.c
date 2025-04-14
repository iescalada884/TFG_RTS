//  Test for all architectures
/*
 * test_interrupt_clock_2.c
 *
 * Test effect of long interrupt handlers on thread CPU-time clock. Uses a
 * periodic timed handler.
 *
 * The sum of the time executed by the thread plus the time executed by the
 * interrupt handlers must be equal to the elapsed time.
 *
 */
#include <stdio.h>
#include <signal.h>
#include <time.h>
#include <pthread.h>
#include <timed_handlers.h>
#include <assert.h>
#include <errno.h>
#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <misc/load_loop.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define TEST_NAME "Test effect of long interrupt handlers on CPU time"

typedef struct {
  float time_to_eat;
} eat_data_t;

#define TIME_UNIT_SEC 0.01
#define HANDLER_PERIOD  (2*TIME_UNIT_SEC)
#define HANDLER_EAT     (0.5*TIME_UNIT_SEC)
#define EAT_MAIN        (20*TIME_UNIT_SEC)
#define LOOPS_IN_MAIN 3
const struct timespec TEN_USEC = {0,    10000};

const struct timespec handler_period_ts =
  {(int)HANDLER_PERIOD, (HANDLER_PERIOD - (int)HANDLER_PERIOD) * 1E9};

volatile int handler_count = 0;

// handler
void handler_code (void *area, marte_timed_handler_t *th)
{
  handler_count++;

  //printf("\nhandler: count:%d\n", handler_count);
  eat(HANDLER_EAT);
}

static void show_times(char *msj,
		       const struct timespec * ts_mono,
		       const struct timespec * ts_cpu,
		       const struct timespec * ts_cpu_int)
{
  struct timespec ts_cpu_int_sum;
  add_timespec (&ts_cpu_int_sum, ts_cpu_int, ts_cpu);
  printf("%s\n", msj);
  printf("   Monotonic        :%f\n", timespec_to_double(ts_mono));
  printf("   Main CPU time    :%f\n", timespec_to_double(ts_cpu));
  printf("   Inter. CPU time  :%f\n", timespec_to_double(ts_cpu_int));
  printf("   All CPU times sum:%f\n", timespec_to_double(&ts_cpu_int_sum));
}

static void get_times(struct timespec * ts_mono,
		      struct timespec * ts_cpu,
		      struct timespec * ts_cpu_int)
{
  CHKE( clock_gettime(CLOCK_MONOTONIC, ts_mono) );
  CHKE( clock_gettime(CLOCK_THREAD_CPUTIME_ID, ts_cpu) );
  if (clock_gettime(CLOCK_INTERRUPTS_CPUTIME, ts_cpu_int) != 0) {
    // CLOCK_INTERRUPTS_CPUTIME not supported
    printf("CLOCK_INTERRUPTS_CPUTIME not supported\n");
    printf("   Test skipped\n");
    printf("Test OK");
  }
}


/* main thread */
int main ()
{
  struct timespec ts_mono_1, ts_mono_2;
  struct timespec ts_cpu_1, ts_cpu_2;
  struct timespec ts_cpu_int_1, ts_cpu_int_2;
  marte_timed_handler_t th;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  printf ("---------     "TEST_NAME"    ------------\n");

  adjust();

  /* Create periodic timed handler */
  CHK( marte_timed_handler_init(&th, CLOCK_MONOTONIC, handler_code, NULL, 0) );

  // Initial time
  get_times(&ts_mono_1, &ts_cpu_1, &ts_cpu_int_1);
  show_times("Initial times", &ts_mono_1, &ts_cpu_1, &ts_cpu_int_1);

  /* Set timed handler */
  CHK (marte_timed_handler_set (&th, PERIODIC_HANDLER, &handler_period_ts));

  /* eat time */
  int i;
  for (i = 0; i < LOOPS_IN_MAIN; i++) {
    //printf ("Main eating %f... ", EAT_MAIN);
    eat (EAT_MAIN);
    //printf ("done\n");
  }

  // Final time
  get_times(&ts_mono_2, &ts_cpu_2, &ts_cpu_int_2);
  show_times("Final times", &ts_mono_2, &ts_cpu_2, &ts_cpu_int_2);

  // get differences
  decr_timespec(&ts_mono_2, &ts_mono_1);
  decr_timespec(&ts_cpu_2, &ts_cpu_1);
  decr_timespec(&ts_cpu_int_2, &ts_cpu_int_1);
  show_times("Differences", &ts_mono_2, &ts_cpu_2, &ts_cpu_int_2);

  // check differences
  struct timespec ts_cpu_int_sum;
  add_timespec (&ts_cpu_int_sum, &ts_cpu_int_2, &ts_cpu_2);
  decr_timespec(&ts_cpu_int_sum, &ts_mono_2);
  assert(smaller_timespec(&ts_cpu_int_sum, &TEN_USEC));

  // handler count
  // Obtained from this equation system:
  //   total_t = main_t + expected_handler_count * HANDLER_EAT
  //   total_t = HANDLER_PERIOD * expected_handler_count
  //
  // where main_t = LOOPS_IN_MAIN * EAT_MAIN
  int expected_handler_count = ((double)LOOPS_IN_MAIN * EAT_MAIN)
                               / (HANDLER_PERIOD - HANDLER_EAT);
  printf("Handler count:%d (expected:%d)\n", handler_count,
	 expected_handler_count);
  printf("Expected value of Interrupt clock:%f sec\n",
	 expected_handler_count * HANDLER_EAT);
  assert(abs(expected_handler_count-handler_count) <= 1);

  // destroy the handler
  CHK (marte_timed_handler_destroy (&th));

  printf("Test OK\n");
  return 0;
}
