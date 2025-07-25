## -*- asm -*- ##############################################################
#                                                                           #
#                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  #
#                                                                           #
#                                 S T A R T                                 #
#                                                                           #
#                               Assembly File                               #
#                                                                           #
#                      Copyright (C) 2012-2016 AdaCore                      #
#                                                                           #
#  GNAT is free software;  you can  redistribute it  and/or modify it under #
#  terms of the  GNU General Public License as published  by the Free Soft- #
#  ware  Foundation;  either version 3,  or (at your option) any later ver- #
#  sion.  GNAT is distributed in the hope that it will be useful, but WITH- #
#  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY #
#  or FITNESS FOR A PARTICULAR PURPOSE.                                     #
#                                                                           #
#  As a special exception under Section 7 of GPL version 3, you are granted #
#  additional permissions described in the GCC Runtime Library Exception,   #
#  version 3.1, as published by the Free Software Foundation.               #
#                                                                           #
#  You should have received a copy of the GNU General Public License and    #
#  a copy of the GCC Runtime Library Exception along with this program;     #
#  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    #
#  <http://www.gnu.org/licenses/>.                                          #
#                                                                           #
#############################################################################

    .syntax unified
    .cpu cortex-m4
    .thumb

    /* This is the initial vector table and also the one that is used by
       the light runtime. The tasking runtimes install their own table (using
       VTOR register) defined in handler.S.  */
    .section .vectors,"a"
__vectors0:
    .word   __stack_end /* stack top address */
    .word   _start_rom  /* 1 Reset */
    .word   hang        /* 2 NMI */
    .word   hang        /* 3 HardFault */
    .word   hang        /* 4 MemManage */

    .text

    .thumb_func
    .globl _start_rom
_start_rom:
        /* Enable FPU and cache when appropriate */
        bl _stm32_start_common

    /* Copy .data */
    movw    r0,#:lower16:__data_start
    movt    r0,#:upper16:__data_start
    movw    r1,#:lower16:__data_words
    movw    r2,#:lower16:__data_load
    movt    r2,#:upper16:__data_load
    cbz r1,1f
0:  ldr r4,[r2],#4
    str r4,[r0],#4
    subs    r1,r1,#1
    bne 0b
1:
    /* Copy .ccmdata */
    movw    r0,#:lower16:__ccmdata_start
    movt    r0,#:upper16:__ccmdata_start
    movw    r1,#:lower16:__ccmdata_words
    movw    r2,#:lower16:__ccmdata_load
    movt    r2,#:upper16:__ccmdata_load
    cbz r1,1f
0:  ldr r4,[r2],#4
    str r4,[r0],#4
    subs    r1,r1,#1
    bne 0b
1:
    /* Clear .bss */
    movw    r0,#:lower16:__bss_start
    movt    r0,#:upper16:__bss_start
    movw    r1,#:lower16:__bss_words
    mov r2,#0
    cbz r1,1f
0:  str r2,[r0],#4
    subs    r1,r1,#1
    bne 0b

1:
    bl  wrapper_main
   
    bl  _exit /* Analize if in a future we need runtime exit too */

.globl _exit
_exit:
    mov r0, #0x18       /* SYS_EXIT semihosting operation */
    mov r1, #0          /* Exit status (0 for success) */
    bkpt #0xAB          /* Trigger semihosting call */
    b .                 /* Infinite loop as a fallback */

hang:   b .
