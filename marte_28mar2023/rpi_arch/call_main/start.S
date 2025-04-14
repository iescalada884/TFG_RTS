.section .init
.globl _start 
#_start:
    ldr pc,reset_handler
    ldr pc,undefined_handler
    ldr pc,swi_handler
    ldr pc,prefetch_handler
    ldr pc,data_handler
    ldr pc,unused_handler
    ldr pc,irq_handler
    ldr pc,fiq_handler

reset_handler: 		.word reset
undefined_handler: 	.word undefined
swi_handler: 		.word swi
prefetch_handler: 	.word prefetch
data_handler: 		.word data
unused_handler: 	.word unused
irq_handler: 		.word irq
fiq_handler: 		.word fiq
	
_start:
#Copy the interrupts vector to the first memory positions
reset:
	#The stack address must be the same that the one used in the linker script
    mov r0,$0x8000
    mov r1,$0x0000
    ldmia r0!,{r2,r3,r4,r5,r6,r7,r8,r9}
    stmia r1!,{r2,r3,r4,r5,r6,r7,r8,r9}
    ldmia r0!,{r2,r3,r4,r5,r6,r7,r8,r9}
    stmia r1!,{r2,r3,r4,r5,r6,r7,r8,r9}

	# Set the stack. 
	#The stack address must be the same that the one used in the linker script
	mov sp, $0x8000

#set stack in irq mode
        mrs r0, cpsr
        bic r0, $0b11111111
        orr r0, $0b11010010
        msr cpsr, r0
#        mov sp, $0x300  [Orig]
        mov sp, $0x600
 
#come back to svc mode  [Orig]
#        mrs r0, cpsr
#        bic r0, $0b11111111
#        orr r0, $0b11010011
#        msr cpsr, r0

# swich to system mode (MaRTE only uses IRQ and system modes)  [NEW]
        msr     cpsr_c,#0xDF 
	#The stack address must be the same that the one used in the linker script
	mov sp, $0x8000

@ Set up initial frame pointers etc [NEW]
	mov     a2, #0		@ Second arg: fill value
	mov	fp, a2		@ Null frame pointer
	mov	r7, a2		@ Null frame pointer for Thumb
	
#fill .bss section with zeros
	ldr r0, =_bss_start
	ldr r1, =_bss_end
	mov r2, $0
	
bssloop:
        cmp r0, r1
        strlo r2, [r0], $4
        blo bssloop
	

#Led ON
#	bl ledOn

#Go to the main
	bl wrapper_main

# infinite loop
loop:
	b loop

/*
#Irq interrupt
irq:
        #mov r1, #1  [NEW]
	ldr sp, =_irq_stack
        bl irq_dbg0
	push {r11, r12, lr} 
# nuevo Save spsr  [NEW]
mrs     r11,spsr
	mov r12, sp
	
#	# switch to SVM
##	mrs r11, cpsr         [OLD]
##	orr r11, r11, #0x1F         [OLD]
##	msr cpsr, r11         [OLD]
	#mrs r11, cpsr         [Orig]
        #bic r11, $0b11111111
        #orr r11, $0b11010011
        #msr cpsr, r11
        ##msr     cpsr_c,#0xD3   [NEW]

        # swich to system mode   [NEW]
        msr     cpsr_c,#0xDF
	
	#save system state
	push {r0-r12, lr}

	
	#save interrupt state on system sp
	#ldmdb r12, {r0, r1, r2}
	#stmfd sp!, {r0, r1, r2}
	ldmfd r12!, {r0-r2}
	stmfd sp!, {r0-r2}

	#call the irq interrupt handler
	bl irq_interrupt

	#restore interrupt state from system sp
	ldr r12, =_irq_stack
	#ldmfd sp!, {r0, r1, r2}
	#stmdb r12, {r0, r1, r2}
	ldmfd sp!, {r0-r2}
	stmfd r12!, {r0-r2}

	#restore system state
	pop {r0-r12, lr}

	#switch to interrupt mode
##	mrs r11, cpsr         [OLD]
##	orr r11, r11, #0x1F   [OLD]
##	and r11, r11, #0x12   [OLD]
##	msr cpsr, r11         [OLD]
	#mrs r11, cpsr        [Orig]
        #bic r11, $0b11111111
        #orr r11, $0b11010010
        #msr cpsr, r11
        #switch to interrupt mode  [NEW]
        msr     cpsr_c,#0xD2
        bl irq_dbg1

# nuevo Restore spsr  [NEW]
mrs     r11,spsr

	#restore interrupt status
	pop {r11, r12, lr}

	#return from interrupt
	subs pc, lr, $4
*/
	
#Unexpected interrupt
#unexpected:
#	push {r0-r12, lr}
#	bl unexpected_interrupt
#	pop {r0-r12, lr}
#	subs pc, lr, $4

undefined:
	ldr sp, =_irq_stack
        push {r0-r12, lr}
        bl undefined_interrupt
        pop {r0-r12, lr}
        subs pc, lr, $4
swi:
	ldr sp, =_irq_stack
        push {r0-r12, lr}
        bl swi_interrupt
        pop {r0-r12, lr}
        subs pc, lr, $4
prefetch:
	ldr sp, =_irq_stack
        push {r0-r12, lr}
        bl prefetch_interrupt
        pop {r0-r12, lr}
        subs pc, lr, $4
data:
	ldr sp, =_irq_stack
        push {r0-r12, lr}
        bl data_interrupt
        pop {r0-r12, lr}
        subs pc, lr, $4
unused:
	ldr sp, =_irq_stack
        push {r0-r12, lr}
        bl unused_interrupt
        pop {r0-r12, lr}
        subs pc, lr, $4
fiq:
	ldr sp, =_irq_stack
        push {r0-r12, lr}
        bl fiq_interrupt
        pop {r0-r12, lr}
        subs pc, lr, $4

@  IRQ wrappers.
        
        .text
        .code 32
        .align   2

        .global irq
irq:

@ Save registers on stack
        sub     r14,r14,#4 @ fix up for return
        #ldr     sp, =__irq_stack__
        ldr sp, =_irq_stack
        stmdb   sp,{r11,r12,r14}
        mrs     r11,spsr
        mov     r12,sp

@ swich to system mode.
        msr     cpsr_c,#0xDF

@ Save registers.
        stmfd   sp!,{r0-r3,r9,r11,lr}

        ldmdb   r12,{r0,r1,r2}	@ ie r11,r12,r14
        stmfd   sp!,{r0,r1,r2}

@ Call the function
        #bl      irq_handler_ada
        bl irq_interrupt

@ pop stack
        #ldr     r12, =__irq_stack__
        ldr     r12, =_irq_stack
        ldmfd   sp!,{r0,r1,r2}
        stmdb   r12,{r0,r1,r2} @ ie r11,r12,lr
        ldmfd   sp!,{r0-r3,r9,r11,lr}

@ swich to interrupt mode and disable IRQs and FIQs
        msr     cpsr_c,#0xD2

@ Restore spsr
        msr     spsr_all,r11

@ Return from interrupt (unstacking the modified r14)
        ldmdb   sp,{r11,r12,pc}^


