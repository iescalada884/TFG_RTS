/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *                                 '_ e x i t'
 *
 *                                      C
 *
 * File '_exit.c'                                                     By MAR.
 *
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
/*
 * Copyright (c) 1996-1999 University of Utah and the Flux Group.
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

#include <oskit/x86/pc/reset.h>
#include <oskit/x86/base_gdt.h>
#include <oskit/x86/base_paging.h>
#include <oskit/x86/base_cpu.h>
#include <stdio.h>
#include <sys/pio.h>    /* cli   */

extern int debugger_active; /* Set by gdb_pc_com_init (MaRTE) */

extern void marte__hal__finish_hardware_use ();
extern unsigned char direct_read_from_stdin (); // from 'kernel_console'


/*
 * _exit() function in libc.
 * If remote GDB is being used, it waits until all the data has
 * cleared out of the FIFOs; if the VGA display is being used (normal
 * console), then it waits for a keypress.  When it is done, it calls
 * pc_reset() to reboot the computer.
 */
void _exit(int rc)
{
  extern size_t return_address;

  cli ();

  printc(" _exit(%d) called; rebooting...\n", rc);
  if (return_address)
    printc("Have return_address 0x%08x\n\n", return_address);

  if (debugger_active) {
    /* Detach from the remote GDB. */
    gdb_serial_exit(rc);
  }

  /* This is so that the user has a chance to SEE the output */
  printc("Press a key to reboot");
  direct_read_from_stdin();
  /* MaRTE OS. I use 'direct_read_from_stdin' and not 'getchar'
     because when it's the debugger who orders program finalization it
     disables interrupts, so getchar won't work any longer */

  // MaRTE OS.
  marte__hal__finish_hardware_use ();

  if (return_address) {
    /*
     * The cleanup needs to be done here instead of in the returned-to
     * code because the return address may not be accessible with our
     * current paging and segment state.
     *
     * The order is important here: paging must be disabled after we
     * reload the gdt.
     */
    printc("Return_address 0x%08x\n\n", return_address);
    cli();
    clts();
    phys_mem_va = 0;
    linear_base_va = 0;
    base_gdt_init();
    /* Reload all since we changed linear_base_va. */
    base_cpu_load();
    paging_disable();
    ((void (*)(void))return_address)();
  }
  else
    pc_reset();
}
