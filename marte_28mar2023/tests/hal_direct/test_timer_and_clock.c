//  Test for all architectures
/*
 * test_timer_and_clock.c
 *
 * Test the timer is fired at the right time.
 *
 */

#include <stdio.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#include "marte_hal_for_tests.h"

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

volatile int handler_has_been_executed = 0;
volatile hw_time_t handler_time;

void handler(void *state) {
  handler_time = marte__hal__get_hwtime();
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

  for(i=0; i<100; i++) {
    printf("%d ", i);
    for(j=0; j<100000; j++) {
    }
  }

  printf("  After sleep\n");

  marte__hal__disable_interrupts();
  
  assert(handler_has_been_executed);

  printf("next_activation           :%qd\n", next_activation);
  printf("handler_time              :%qd\n", handler_time);
  printf("dif(handler_time-next_activation):%qd\n", 
	 (handler_time-next_activation));

  printf("Test OK\n");
  return 0;
}
