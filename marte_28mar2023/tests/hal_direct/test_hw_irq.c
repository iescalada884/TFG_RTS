//  Test for all architectures
/*
 * test_hw_irq.c
 *
 * Program a timer interrupt and check that the handler is executed.
 *
 */

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#include "marte_hal_for_tests.h"

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

volatile int handler_has_been_executed = 0;

void handler(void *state) {
  printf("In handler\n");
  handler_has_been_executed = 1;
}

int main ()
{
  int i, j;
  hw_time_t interval = 100000;
  hw_time_t next_activation;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  printf("Install timer handler for IRQ:%d\n", marte__hal__timer_interrupt());
  marte__hal__install_hw_interrupt_handler(marte__hal__timer_interrupt(),
					   handler);

  printf("Program timer\n");

  marte__hal__disable_interrupts();
  marte__hal__program_timer(0, interval, &next_activation);
  marte__hal__enable_interrupts();

  CHK (sched_yield());
  // when asserts are enabled sched_yield checks the task stack

  for(i=0; i<100; i++) {
    printf("%d ", i);
    CHK (sched_yield());
    for(j=0; j<100000; j++) {
    }
  }

  CHK (sched_yield());

  marte__hal__disable_interrupts();
  printf("  After loop\n");

  assert(handler_has_been_executed);

  printf("Test OK\n");
  return 0;
}
