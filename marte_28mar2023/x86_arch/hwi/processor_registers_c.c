/*----------------------------------------------------------------------------
 *--------------------        M a R T E     O S        -----------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                 'p r o c e s s o r _ r e g i s t e r s'
 *
 *                                   C
 *
 *
 * File 'processor_registers.c'                                       By Mar.
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
 *--------------------------------------------------------------------------*/

#define get_esp() \
    ({ \
	register unsigned int _temp__; \
	asm("movl %%esp, %0" : "=r" (_temp__)); \
	_temp__; \
    })

/*
 * Get ESP register
 */
unsigned int get_esp_register()
{
  return get_esp();
}

/*
 * get_flags_and_disable_interrupts_c
 */
unsigned get_flags_and_disable_interrupts_c ()
{
  unsigned eflags;
  asm volatile("\n"
	       "pushfl\n"
	       "popl %0\n"
	       "cli" : "=r" (eflags));
  return eflags;
}

/*
 * restore_flags_c
 */
void restore_flags_c (unsigned eflags)
{
  asm volatile("\n"
	       "pushl %0\n"
	       "popfl" : : "r" (eflags));
}
