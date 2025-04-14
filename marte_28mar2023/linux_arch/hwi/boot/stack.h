/*
 * stack.h
 * Version 0.1
 *
 * Written by Miguel Masmano Tello <mmasmano@disca.upv.es>
 * Copyright (C) Jul, 2003
 * Release under the terms of the GNU General Public License Version 2
 *
 */

#ifndef _STACK_DEF_
#define _STACK_DEF_

extern unsigned int linux_stack;

#define GET_CONTEXT(current_stack) \
__asm__ __volatile__ ("pushl %%eax\n\t" \
                      "pushl %%ebp\n\t" \
                      "pushl %%edi\n\t" \
                      "pushl %%esi\n\t" \
                      "pushl %%edx\n\t" \
                      "pushl %%ecx\n\t" \
                      "pushl %%ebx\n\t" \
    	              "pushl $ret_addr\n\t" \
                      "mov %%esp, %0\n\t": "=m" (current_stack) \
                      : /* No Input */); 

#define PUT_RETURN_ADDRESS \
__asm__ __volatile__ ("ret_addr:\n\t" \
		      "popl %%ebx\n\t" \
                      "popl %%ecx\n\t" \
                      "popl %%edx\n\t" \
                      "popl %%esi\n\t" \
                      "popl %%edi\n\t" \
                      "popl %%ebp\n\t" \
                      "popl %%eax\n\t": /* No Output */ : /* No Input */);

#define SET_CONTEXT(new_stack) { \
__asm__ __volatile__ ("mov %0, %%esp\n\t" \
                      "ret\n\t": /* No Output */ : "m" (new_stack)); \
}


#define SET_NEW_STACK(new_stack) { \
__asm__ __volatile__ ("mov %0, %%esp\n\t" \
                      : /* No Output */ : "m" (new_stack)); \
}

#endif
