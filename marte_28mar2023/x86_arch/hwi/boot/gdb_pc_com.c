/*
 * Copyright (c) 1994-1996 Sleepless Software
 * Copyright (c) 1997-1998 University of Utah and the Flux Group.
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
 * Remote PC serial-line debugging for the Flux OS Toolkit
 * Remote GDB debugging through COM port on a PC.
 */

#include <oskit/gdb.h>
#include <oskit/gdb_serial.h>
#include <oskit/tty.h>
#include <oskit/x86/pio.h>
#include <oskit/x86/seg.h>
#include <oskit/x86/base_idt.h>
#include <oskit/x86/base_gdt.h>
#include <oskit/x86/base_trap.h>
#include <oskit/x86/pc/pic.h>
#include <oskit/x86/pc/base_irq.h>
#include <oskit/x86/pc/com_cons.h>
#include <oskit/x86/debug_reg.h>
#include <oskit/config.h>

#define NULL 0

extern void gdb_pc_com_intr();

static int gdb_com_port = 1;

int debugger_active = 0; /* Set by gdb_pc_com_init (MAR) */

int gdb_cons_getchar()
{
        return com_cons_getchar(gdb_com_port);
}

void gdb_cons_putchar(int ch)
{
        com_cons_putchar(gdb_com_port, ch);
}


void gdb_pc_com_init(int com_port, struct termios *com_params)
{
	int com_irq = com_port & 1 ? 4 : 3;
	int i;

	printc ("Initialize remote debugger using COM%1d\n", com_port);

	debugger_active = 1; // MAR

	/* Tell the generic serial GDB code
	   how to send and receive characters.  */
	gdb_serial_recv = gdb_cons_getchar;
	gdb_serial_send = gdb_cons_putchar;
	gdb_com_port = com_port;

	/* Tell the GDB proxy trap handler to use the serial stub.  */
	gdb_signal = gdb_serial_signal;

	/* Hook in the GDB proxy trap handler as the main trap handler
	   for all traps. These might be overridden later. */
	for (i = 0; i < BASE_TRAP_COUNT; i++)
	  base_trap_handlers[i] = gdb_trap_ss;

	/*
	 * Initialize the serial port.
	 * If no com_params were specified by the caller,
	 * then default to base_raw_termios.
	 * (If we just passed through the NULL,
	 * then com_cons_init would default to base_cooked_termios,
	 * which messes up the GDB remote debugging protocol. MaRTE OS:
	 * now the default is base_raw_termios)
	 */
	if (com_params == 0)
		com_params = &base_raw_termios;
	com_cons_init(com_port, com_params);

	/* Hook the COM port's hardware interrupt.
	   The com_cons itself uses only polling for communication;
	   the interrupt is only used to allow the remote debugger
	   to stop us at any point, e.g. when the user presses CTRL-C.  */
	fill_irq_gate(com_irq, (unsigned)gdb_pc_com_intr, KERNEL_CS, ACC_PL_K);

	/* Enable the COM port interrupt.  */
	com_cons_enable_receive_interrupt(com_port);
	pic_enable_irq(com_irq);
}

#ifdef HAVE_DEBUG_REGS
void
gdb_pc_set_bkpt(void *bkpt)
{
	/*
	 * Set up the debug registers
	 * to catch null pointer references,
	 * and to take a breakpoint at the entrypoint to main().
	 */
	set_b0(NULL, DR7_LEN_1, DR7_RW_INST);
	set_b1(NULL, DR7_LEN_4, DR7_RW_DATA);
	set_b2((unsigned)bkpt, DR7_LEN_1, DR7_RW_INST);

	/*
	 * The Intel Pentium manual recommends
	 * executing an LGDT instruction
	 * after modifying breakpoint registers,
	 * and experience shows that this is necessary.
	 */
	base_gdt_load();
}
#endif /* HAVE_DEBUG_REGS */

// << MaRTE OS
/* ToDo: where does this really belong? */
/*
 * Setup a port as a kill switch to force the kernel into a
 * panic. Very useful when the kernel is looping someplace and
 * interrupts are still enabled. Simply cat some characters into the
 * special device file.
 */
// void
// gdb_pc_set_killswitch(int port)
// {
// 	int com_irq = port & 1 ? 4 : 3;
// 	extern void com_kill_intr();
// 
// 	/*
// 	 * Initialize the serial port with base_raw_termios.
// 	 */
// 	com_cons_init(port, &base_raw_termios);
// 
// 	/*
// 	 * Drain it.
// 	 */
// 	while (com_cons_trygetchar(port) != -1)
// 		;
// 
// 	/* Hook the COM port's hardware interrupt. */
// 	fill_irq_gate(com_irq,
// 		      (unsigned)com_kill_intr, KERNEL_CS, ACC_PL_K);
// 	
// 	/* Enable the COM port interrupt.  */
// 	com_cons_enable_receive_interrupt(port);
// 	pic_enable_irq(com_irq);
// }
// MaRTE OS >>
