// marte-hal.adb
#ifndef _MARTE_HAL_FOR_TESTS_H_
#define _MARTE_HAL_FOR_TESTS_H_

#include <stdint.h>

// Interrupts

typedef int hw_interrupt_t;

extern void marte__hal__install_hw_interrupt_handler(hw_interrupt_t int_num,
						     void (*handler)(void *));

extern hw_interrupt_t marte__hal__timer_interrupt();

extern void marte__hal__disable_interrupts();

extern void marte__hal__enable_interrupts();

extern uint8_t marte__hal__are_interrupts_enabled();

// Stack Pointer Register

#if MARTE_ARCHITECTURE == ARCH_STM32F
extern void *returnSP();
#endif

// Time and timers

typedef uint64_t hw_time_t;
typedef int64_t duration_t;

extern hw_time_t marte__hal__get_hwtime();

extern duration_t marte__hal__hwtime_to_duration(hw_time_t th);

extern hw_time_t marte__hal__duration_to_hwtime(duration_t d);

extern hw_time_t marte__hal__get_hwclock_frequency();  //  Ticks per second

extern hw_time_t marte__hal__cpu_frequency();  //  Ticks per second

extern void marte__hal__program_timer(int timer_not_used,
				      hw_time_t interval,
				      hw_time_t* next_activation);

// Context switch

#if MARTE_ARCHITECTURE == ARCH_STM32F
#define CONTEXT_BUFFER_CAPACITY 10
// Same value than System.BB.Parameters.Context_Buffer_Capacity
typedef struct {
  uint32_t bytes[CONTEXT_BUFFER_CAPACITY];
} arch_specific_context_t;

extern void marte__hal__init_task_context(void *context_ac,
					  void *task_wrapper_address,
					  void *task_body_address,
					  void *stack_pointer);
extern void marte__hal__context_switch(arch_specific_context_t *old,
				       arch_specific_context_t *new);
#else

extern void marte__hal__context_switch(void ** old, void ** new);
#endif

#endif // _MARTE_HAL_FOR_TESTS_H_
