//  Test for:
/*
 * test_context_switch.c
 *
 * Low level test of the HAL.Context_Switch.
 * A stack is created and set to execute a procedure
 * (other_thread_body). A context switch is performed to that stack and
 * then back to the main task.
 *
 */

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <pthread.h>

#include <assert.h>

#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#include "marte_stacks_for_tests.h"
#include "marte_hal_for_tests.h"

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define OTHER_STACK_SIZE_B 8*1024
#define PARAM_OF_OTHER_BODY 11

void * other_stack_top;
void * main_stack_top;
stack_id_t other_stack_id;
stack_id_t main_stack_id;

volatile int other_has_been_executed = 0;
volatile int handler_has_been_executed = 0;

void handler(void *state) {
  handler_has_been_executed = 1;

  //  swicth to the other task
  printf("**Handler:About to switch context to the other task...\n");
  marte__hal__context_switch(&main_stack_top, &other_stack_top);
}

void other_thread_body(int param) {
  printf("  Other_thread_body. Param:%d\n", param);
  assert(handler_has_been_executed);

  assert(param == PARAM_OF_OTHER_BODY);
  assert(marte__stacks_management__stack_ok(main_stack_top, main_stack_id));
  assert(marte__stacks_management__stack_ok(other_stack_top, other_stack_id));

  other_has_been_executed = 1;

  //  go back to the main task

  marte__hal__context_switch(&other_stack_top, &main_stack_top);

  // This should never be reached
  assert(0);
}

int main ()
{
  const hw_time_t interval = 200000;
  hw_time_t next_activation;

#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

  main_stack_id = marte__stacks_management__get_main_task_stack();

  printf("Main request stack for other\n");
  other_stack_id = marte__stacks_management__request_stack(OTHER_STACK_SIZE_B);
  assert(other_stack_id != 0);

  //  Configure the task stack for the first activation
  marte__stacks_management__write_in_stack((int)&other_thread_body,
					   15,
					   other_stack_id);
  //                       31   27   23   19   15   11   7    4
  //                       |    |  J |    _GE_ |    | EA IFT_mode_            
  //                  cprs=0000 0000 0000 0000 0000 0011 0001 0011
  //                                                         SVC (supervisor) 
  marte__stacks_management__write_in_stack(0x00000313,
					   14,
					   other_stack_id);
  marte__stacks_management__write_in_stack(PARAM_OF_OTHER_BODY,
					   13,
					   other_stack_id);
  other_stack_top = 
    marte__stacks_management__dword_in_stack_address(15, other_stack_id);

  // install handler and program timer
  printf("Install timer handler for IRQ:%d\n", marte__hal__timer_interrupt());
  marte__hal__install_hw_interrupt_handler(marte__hal__timer_interrupt(),
					   handler); 

  printf("Program timer\n");
  marte__hal__program_timer(0, interval, &next_activation);

  // wait for the other task to execute
  printf("Main: wait for the other task to execute");
  while (!other_has_been_executed) {
  }

  //  When here is because the other task has executed
  printf("Back in main thread\n");
  assert(marte__stacks_management__stack_ok(main_stack_top, main_stack_id));
  assert(marte__stacks_management__stack_ok(other_stack_top, other_stack_id));
  assert(other_has_been_executed);
  assert(handler_has_been_executed);

  printf("Test OK");
  return 0;
}
