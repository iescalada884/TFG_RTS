/*
 * Copyright (c) 1996-1998 University of Utah and the Flux Group.
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
/*
 * Interrupt handler initialization table and stubs.
 */
/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 * Just slight modifications to the original code:
 *
 *  - The "software handler" is 'do_scheduling'.
 *  - Removed the annotations related stuff.
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/



#include <oskit/x86/asm.h>
#include <oskit/x86/gate_init.h>
#include <oskit/x86/seg.h>
#include <oskit/x86/base_trap.h>

#include "pc_asm.h" /* PICs and APIC definitions */

/*
 * XXX these should either be generated (ala genassym) or defined in
 * base_trap.h.
 */
#define TS_EIP	56
#define TS_CS	60

	.text

/*
 * Gives gprof a symbol to assign time to for functions registered
 * as interrupt handlers
 */
NON_GPROF_ENTRY(oskit_base_irq_handler);


#define MASTER(irq, num)						\
	GATE_ENTRY(BASE_IRQ_MASTER_BASE + (num), 0f, ACC_PL_K|ACC_INTR_GATE) ;\
	P2ALIGN(TEXT_ALIGN)						;\
0:									;\
	pushl	$(irq)			/* error code = irq vector */	;\
	pushl	$BASE_IRQ_MASTER_BASE + (num)	/* trap number */	;\
	pusha				/* save general registers */	;\
	movl	$(irq),%ecx		/* irq vector number */		;\
	movb	$1 << num,%dl		/* pic mask for this irq */	;\
	movl	EXT(base_irq_handlers + irq*4),%esi			;\
	call EXT(scheduler__start_interrupt)				;\
	jmp	master_ints

#define SLAVE(irq, num)						\
	GATE_ENTRY(BASE_IRQ_SLAVE_BASE + (num), 0f, ACC_PL_K|ACC_INTR_GATE) ;\
	P2ALIGN(TEXT_ALIGN)						;\
0:									;\
	pushl	$(irq)			/* error code = irq vector */	;\
	pushl	$BASE_IRQ_SLAVE_BASE + (num)	/* trap number */	;\
	pusha				/* save general registers */	;\
	movl	$(irq),%ecx		/* irq vector number */		;\
	movb	$1 << num,%dl		/* pic mask for this irq */	;\
	movl	EXT(base_irq_handlers + irq*4),%esi			;\
	call EXT(scheduler__start_interrupt)				;\
	jmp	slave_ints

#define APIC_TIMER(irq, num)						\
	GATE_ENTRY(BASE_IRQ_SLAVE_BASE + (num), 0f, ACC_PL_K|ACC_INTR_GATE) ;\
	P2ALIGN(TEXT_ALIGN)						;\
0:									;\
	pushl	$(irq)			/* error code = irq vector */	;\
	pushl	$BASE_IRQ_SLAVE_BASE + (num)	/* trap number */	;\
	pusha				/* save general registers */	;\
	movl	$(irq),%ecx		/* irq vector number */		;\
	movl	EXT(base_irq_handlers + irq*4),%esi			;\
	call EXT(scheduler__start_interrupt)				;\
	jmp	apic_timer_int


GATE_INITTAB_BEGIN(base_irq_inittab)

MASTER(0, 0)
MASTER(1, 1)
MASTER(2, 2)
MASTER(3, 3)
MASTER(4, 4)
MASTER(5, 5)
MASTER(6, 6)
MASTER(7, 7)
SLAVE( 8, 0)
SLAVE( 9, 1)
SLAVE(10, 2)
SLAVE(11, 3)
SLAVE(12, 4)
SLAVE(13, 5)
SLAVE(14, 6)
SLAVE(15, 7)
APIC_TIMER(16, 8)

GATE_INITTAB_END

/* master ints */
	P2ALIGN(TEXT_ALIGN)
master_ints:

	/* Save the current master PIC mask (bl) */
	inb	$0x21,%al
	movb	%al,%bl

	/* Mask the interrupting IRQ */
	orb	%dl,%al
	outb	%al,$0x21

	/* Acknowledge the interrupt */
	movb	$0x20,%al
	outb	%al,$0x20

	/* Increment the hardware interrupt nesting counter */
	incb	EXT(base_irq_nest)

	/* XXX re-enable the processor's I flag? */

	/* Save the rest of the standard trap frame (oskit/x86/base_trap.h). */
	pushl	%ds
	pushl	%es
	pushl	%fs
	pushl	%gs

	/* Load the kernel's segment registers.  */
	movl	%ss,%dx
	movl	%dx,%ds
	movl	%dx,%es

	/* GCC likes the direction flag cleared.  */
	cld

	/* Call the interrupt handler with the trap frame as a parameter */
	pushl	%esp
	call	*%esi
	popl	%eax

	/* Unmask the IRQ */
	movb	%bl,%al
	outb	%al,$0x21

        jmp end_interrupt_handler


/* slave ints */
	P2ALIGN(TEXT_ALIGN)
slave_ints:

	/* Save the current slave PIC mask (bl) */
	inb	$0xa1,%al
	movb	%al,%bl

	/* Mask the interrupting IRQ */
	orb	%dl,%al
	outb	%al,$0xa1

	/* Acknowledge the interrupt at both the master and the slave */
	movb	$0x20,%al
	outb	%al,$0x20
	outb	%al,$0xa0

	/* Increment the hardware interrupt nesting counter */
	incb	EXT(base_irq_nest)

	/* XXX re-enable the processor's I flag? */

	/* Save the rest of the standard trap frame (oskit/x86/base_trap.h). */
	pushl	%ds
	pushl	%es
	pushl	%fs
	pushl	%gs

	/* Load the kernel's segment registers.  */
	movl	%ss,%dx
	movl	%dx,%ds
	movl	%dx,%es

	/* GCC likes the direction flag cleared.  */
	cld

	/* Call the interrupt handler with the trap frame as a parameter */
	pushl	%esp
	call	*%esi
	popl	%eax

	/* Unmask the IRQ */
	movb	%bl,%al
	outb	%al,$0xa1

        jmp end_interrupt_handler


/* apic_timer_int */
	P2ALIGN(TEXT_ALIGN)
apic_timer_int:

	/* Increment the hardware interrupt nesting counter */
	incb	EXT(base_irq_nest)

	/* XXX re-enable the processor's I flag? */

	/* Save the rest of the standard trap frame (oskit/x86/base_trap.h). */
	pushl	%ds
	pushl	%es
	pushl	%fs
	pushl	%gs

	/* Load the kernel's segment registers.  */
	movl	%ss,%dx
	movl	%dx,%ds
	movl	%dx,%es

	/* GCC likes the direction flag cleared.  */
	cld

	/* Call the interrupt handler with the trap frame as a parameter */
	pushl	%esp
	call	*%esi
	popl	%eax

	/* End of Interrupt (the local APIC it can issue the next interrupt) */
	movl	$0x0,APIC_BASE+EOI_OFFSET

        jmp end_interrupt_handler


/* final part of any interrupt handler either from master PIC, slave PIC
   or APIC */
end_interrupt_handler:
	/* Decrement the nesting counter and check for software interrupts */
	decb	EXT(base_irq_nest)
	jnz	no_do_scheduling
	/*movb	$0x80,EXT(base_irq_nest)*/
	pushl	%esp
	call	EXT(do_scheduling)
	popl	%eax
	jmp	fin
no_do_scheduling:
	call    EXT(scheduler__end_interrupt)
fin:

	/* Return from the interrupt */
	popl	%gs
	popl	%fs
	popl	%es
	popl	%ds
	popa
	addl	$4*2,%esp	/* Pop trap number and error code */
	iret
