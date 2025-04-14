//  Test for all architectures
/*
 * test_enable_interrupts.c
 *
 * Test that the interrupts are enabled and disabled properly.
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

volatile int handler_counter = 0;

void handler(void *state) {
  // printf("In handler\n");
  handler_counter++;
}

int main ()
{
  int i, j;
  hw_time_t interval = 100000;
  hw_time_t next_activation;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif
    
  // Test simple enable/disable
  
  assert (handler_counter == 0);
  
  assert (marte__hal__are_interrupts_enabled());
  
  marte__hal__disable_interrupts();
  
  assert (!marte__hal__are_interrupts_enabled());
  
  marte__hal__enable_interrupts(); 
  
  assert (marte__hal__are_interrupts_enabled());

  // program timer
  
  marte__hal__disable_interrupts();
  
  printf("Install timer handler for IRQ:%d\n", marte__hal__timer_interrupt());
  marte__hal__install_hw_interrupt_handler(marte__hal__timer_interrupt(),
					   handler);
  
  assert (handler_counter == 0);

  printf("Program timer\n");
  marte__hal__program_timer(0, interval, &next_activation);
    
  assert (handler_counter == 0);
  
  marte__hal__enable_interrupts();

  printf("  Run for a while with interrupts enabled\n");
  for(i=0; i<100; i++) {
    printf("%d ", i);
    for(j=0; j<100000; j++) {
    }
  }
  printf("\n    End for\n");
    
  assert (handler_counter > 0);

  marte__hal__disable_interrupts();
  
  {
    int handler_counter0 = handler_counter;

    printf("  Run for a while with interrupts disabled\n");
    for(i=0; i<100; i++) {
      printf("%d ", i);
      for(j=0; j<100000; j++) {
      }
    }
    printf("\n    End for\n");
    
    assert (handler_counter == handler_counter0);
  }

  printf("Test OK\n");
  return 0;
}
