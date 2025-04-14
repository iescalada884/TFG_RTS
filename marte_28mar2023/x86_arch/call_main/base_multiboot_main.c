/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                    'b a s e _ m u l t i b o o t _ m a i n'
 *
 *                                      C
 *
 * File 'base_multiboot_main.c'                                        By MAR.
 *
 * The function 'multiboot_main' defined in this file is the first
 * function called after the completion of the hardware initialization
 * code.
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
 * Copyright (c) 1996, 1998 University of Utah and the Flux Group.
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


#include <oskit/x86/multiboot.h>
#include <oskit/x86/base_vm.h>
#include <oskit/x86/base_cpu.h>
#include <oskit/x86/pc/reset.h>
#include <oskit/x86/pc/base_multiboot.h>
#include <oskit/c/stdlib.h>
#include <debug_marte.h>
#include <string.h>
extern int init_dynamic_memory(); // 'dynamic_memory/init_dynamic_memory.c'

struct multiboot_info boot_info;

size_t return_address = 0;

extern int wrapper_main(int argc, char *argv[], char *envp[]);
extern char **environ;

#ifdef __ELF__
extern void __oskit_init(void);
extern void __oskit_fini(void);
#endif

#ifdef GPROF
extern int enable_gprof;
extern void base_gprof_init();
#endif

extern void direct_write_on_stdout (char * str, int count); // 'Kernel_Console'
extern void basic_stdout_initialization (); //  From 'Kernel_Console'
extern void basic_stderr_initialization (); //  From 'Kernel_Console'

/*
 * multiboot_main
 */
void multiboot_main(oskit_addr_t boot_info_pa)
{
  int argc = 0;
  char **argv = 0;
  int i;

  basic_stderr_initialization ();
  basic_stdout_initialization ();
  direct_write_on_stdout
    ("                       -=  M a R T E   O S  =-\n", 47);
  direct_write_on_stdout
    ("                           V2.0 2019-05-24\n", 44);
  direct_write_on_stdout
    ("            Copyright (C) Universidad de Cantabria, SPAIN\n", 58);

  /* Copy the multiboot_info structure into our pre-reserved area.
     This avoids one loose fragment of memory that has to be avoided.  */
  boot_info = *(struct multiboot_info*)phystokv(boot_info_pa);

  /* Identify the CPU and get the processor tables set up.  */
  base_cpu_setup();

  /* Initialize the memory allocator and find all available memory.  */
  base_multiboot_init_mem();

  /* Initialize dynamic_memory */
  init_dynamic_memory ();

  /* Parse the command line into nice POSIX-like args and environment. */
  base_multiboot_init_cmdline(&argc, &argv);

  /* Look for a return address. */
  for (i = 0; i < oskit_bootargc; i++)
    if (strcmp(oskit_bootargv[i], "-retaddr") == 0 && i+1 < oskit_bootargc) {
      return_address = strtoul(oskit_bootargv[i+1], 0, 0);
      break;
    }

#ifdef GPROF
  if (enable_gprof)
    base_gprof_init();
#endif

  // Debug
  //init_serial_communication_with_gdb (SERIAL_PORT_1);
  //set_break_point_here;

  /* Invoke the user's main program. */	
  asm volatile("cli");
  exit(wrapper_main(argc, argv, environ));
}

