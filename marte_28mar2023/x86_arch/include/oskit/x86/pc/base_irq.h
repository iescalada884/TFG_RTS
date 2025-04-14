/*
 * Copyright (c) 1995-1998 University of Utah and the Flux Group.
 * All rights reserved.
 * 
 * This file is part of the Flux OSKit.  The OSKit is free software, also known
 * as "open source;" you can redistribute it and/or modify it under the terms
 * of the GNU General Public License (GPL), version 2, as published by the Free
 * Software Foundation (FSF).  To explore alternate licensing terms, contact
 * the University of Utah at csl-dist@cs.utah.edu or +1-801-585-3271.
 * 
 * The OSKit is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GPL for more details.  You should have
 * received a copy of the GPL along with the OSKit; see the file COPYING.  If
 * not, write to the FSF, 59 Temple Place #330, Boston, MA 02111-1307, USA.
 */
/*
 * This header defines the OSKIT's base IRQ handling facilities.
 * The default environment provides a high-level, easy-to-use,
 * and not-too-horrendously-inefficient mechanism
 * for hooking into hardware interrupts and installing interrupt handlers.
 * It also provides a basic software interrupt facility.
 */
#ifndef _OSKIT_X86_PC_BASE_IRQ_H_
#define _OSKIT_X86_PC_BASE_IRQ_H_

struct trap_state;


/* On normal PCs, there are always 16 IRQ lines.  */
/* MAR. I add one more to the 'base_irq_handlers' table (the Local
   Apic Timer interrupt) */
#define BASE_IRQ_COUNT		0x10 + 1

/* This is the default location in the IDT at which we program the PICs. */
#define BASE_IRQ_MASTER_BASE	0x20
#define BASE_IRQ_SLAVE_BASE	0x28
/* (MAR) Any change in these values should be reflected in
    'PIC_Master_Base' and 'PIC_Slave_Base' (file
    'x86/hardware_interface.gpb') and in 'x86/boot/pc_asm.h'. */



/* Variables storing the current master and slave PIC interrupt vector base */
extern int irq_master_base, irq_slave_base;


/*
 * In the base oskit environment,
 * each hardware interrupt vector points to a small assembly language stub
 * that saves the standard trap frame (see x86/base_trap.h),
 * disables and acknowledges the hardware interrupt,
 * and calls a designated high-level handler routine through this table.
 * Initially, all the entries in this table point to base_irq_default_handler();
 * to install a custom handler, simply change the appropriate table entry.
 * Custom interrupt handlers can freely examine and modify
 * the processor state of the interrupted activity,
 * e.g., to implement threads and preemption.
 * On entry, the processor's IDT interrupt vector number is in ts->trapno
 * and the hardware IRQ number that caused the interrupt is in ts->err.
 */
extern void (*base_irq_handlers[])(struct trap_state *ts);


/* Fill an IRQ gate in the base IDT.
   Always uses an interrupt gate; just set `access' to the privilege level.  */
#define fill_irq_gate(irq_num, entry, selector, access)			\
	fill_gate(&base_idt[(irq_num) < 8				\
			    ? irq_master_base+(irq_num)			\
			    : irq_slave_base+(irq_num)-8],		\
		  entry, selector, ACC_INTR_GATE | (access), 0)


/*
 * Program the PICs to the standard vector base addresses above,
 * and initialize the corresponding entries in the base IDT.
 * Assumes the processor's interrupt flag is disabled throughout.
 */
void base_irq_init(void);

/*
 * This gate initialization table is used by base_irq_init
 * to initialize the hardware interrupt vectors in the base IDT.
 */
#include <oskit/x86/gate_init.h> // <- MaRTE OS (28-08-07)
extern struct gate_init_entry base_irq_inittab[];

/*
 * Default IRQ handler for unexpected hardware interrupts
 * simply displays a warning message and returns.
 */
void base_irq_default_handler(struct trap_state *ts);


/*** Software Interrupt Handling ***/

/*
 * Hardware interrupt nesting counter,
 * used to ensure that the software interrupt handler isn't called
 * until all outstanding hardware interrupts have been processed.
 * In addition, this word also acts as the software interrupt pending flag:
 * if the high bit is set, no software interrupt is pending;
 * if the high bit is clear, a software interrupt is pending.
 * This design allows the interrupt path to decide
 * whether to call the software interrupt handler
 * simply by testing the word for zero.
 */
extern unsigned char base_irq_nest;

/*
 * Request a software interrupt (usually from a hardware interrupt handler).
 * The base_irq_softint_handler() function will be called later
 * after all hardware interrupt handlers have completed processing.
 */
#define base_irq_softint_request()	\
	base_irq_nest &= ~0x80;


#endif /* _OSKIT_X86_PC_BASE_IRQ_H_ */
