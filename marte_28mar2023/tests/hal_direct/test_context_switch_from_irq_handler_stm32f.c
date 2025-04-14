//  Test for: stm32f
/*
 * test_context_switch_from_irq_handler_stm32f.c
 *
 * Low level test of the HAL.Context_Switch.
 * A stack is created and set to execute a procedure
 * (other_thread_body). A context switch is performed to that stack and
 * then back to the main task.
 * The context switch is fired from the timer handler.
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

#if MARTE_ARCHITECTURE != ARCH_STM32F
#error "Tests for stm32f architecture"
#endif

extern void marte__kernel__tasks_operations__internals__task_wrapper
  (void *task_body);

extern int __stack_end;

#define OTHER_STACK_SIZE_B 8*1024

void * other_stack_top;
void * main_stack_top;
stack_id_t other_stack_id;
stack_id_t main_stack_id;
arch_specific_context_t other_context;
arch_specific_context_t main_context;

volatile int other_has_been_executed = 0;
volatile unsigned handler_counter = 0;

void handler(void *state) {
  //  swicth to the other task
  //printf("**Handler\n");
  if (handler_counter == 0) {
    printf("**Handler:About to switch context to the other task...\n");
    marte__hal__context_switch(&main_context, &other_context);
  }
  
  handler_counter++;
}

void other_thread_body(int param) {
  printf("  Other_thread_body\n");
  assert(handler_counter > 0);

  assert(marte__stacks_management__stack_ok(other_stack_top, other_stack_id));

  other_has_been_executed = 1;

  //  go back to the main task

  marte__hal__disable_interrupts();
  marte__hal__context_switch(&other_context, &main_context);
  marte__hal__enable_interrupts();

  // This should never be reached
  assert(0);
}

int main ()
{
  assert(sizeof(arch_specific_context_t) == 
	 CONTEXT_BUFFER_CAPACITY * sizeof(uint32_t));
  assert(sizeof(uint32_t) == sizeof(void *));
  
  printf("This tests fails with asserts enabled\n");

  main_stack_id = marte__stacks_management__get_main_task_stack();

  printf("Main request stack for other\n");
  other_stack_id = marte__stacks_management__request_stack(OTHER_STACK_SIZE_B);
  assert(other_stack_id != 0);
  
  printf("Main stack botton: %p\n", &__stack_end);
  printf("Other stack botton:%p\n",
	 marte__stacks_management__dword_in_stack_address(1, other_stack_id));
  printf("Main context: %p\n", &main_context);
  printf("Other context:%p\n", &other_context);

  // Configure the otehr task context for its first activation
  // Equivalent to code in K.Tasks_Operations.Initialize_TCBs.Initialize
  /* HAL.Init_Task_Context
           (Context_Ac           => T.Arch_Specific_Context'Access,
            Task_Wrapper_Address => Task_Wrapper,
            Task_Body_Address    =>
               Task_Body_Function_To_Address (Task_Body),
            Stack_Pointer        =>
	       MaRTE.Stacks_Management.Get_Stack_Base_Address (T.Stack)); */
  marte__hal__init_task_context
    (&other_context,
    marte__kernel__tasks_operations__internals__task_wrapper,
    other_thread_body,
    marte__stacks_management__dword_in_stack_address(1, other_stack_id));
				  
  other_stack_top = 
    marte__stacks_management__dword_in_stack_address(1, other_stack_id);

  // install handler and program timer
  marte__hal__disable_interrupts();
  printf("Install timer handler for IRQ:%d\n", marte__hal__timer_interrupt());
  marte__hal__install_hw_interrupt_handler(marte__hal__timer_interrupt(),
					   handler);
  assert(handler_counter == 0);
  
  // wait for the other task to execute
  printf("Main: wait for the other task to execute1\n");
  while (!other_has_been_executed) {
    printf("Handler counter: %u\n", handler_counter);
  marte__hal__enable_interrupts();
  marte__hal__disable_interrupts();
  }

  //  When here is because the other task has executed
  marte__hal__disable_interrupts();
  printf("Back in main thread\n");
  assert(marte__stacks_management__stack_ok(returnSP(), main_stack_id));
  assert(marte__stacks_management__stack_ok(other_stack_top, other_stack_id));
  assert(other_has_been_executed);
  assert(handler_counter > 0);

  printf("Test OK\n");
  return 0;
}
