// marte-stacks_management.adb
#ifndef _MARTE_STACKS_FOR_TESTS_H_
#define _MARTE_STACKS_FOR_TESTS_H_

#include <stdint.h>


typedef void * stack_id_t;
typedef uint32_t stack_size_t;

extern stack_id_t marte__stacks_management__request_stack(stack_size_t size);

extern stack_id_t marte__stacks_management__get_main_task_stack();

extern int * marte__stacks_management__get_stack_base_address(stack_id_t stack_id);

extern int * marte__stacks_management__get_stack_top_address(stack_id_t stack_id);

extern void marte__stacks_management__write_in_stack(int val,
						     uint32_t pos,
						     stack_id_t stack_id);
extern void * marte__stacks_management__dword_in_stack_address (uint32_t pos,
								stack_id_t stack_id);

extern int marte__stacks_management__stack_ok(void * task_stack_top,
					      stack_id_t stack_id);

#endif // _MARTE_STACKS_FOR_TESTS_H_
