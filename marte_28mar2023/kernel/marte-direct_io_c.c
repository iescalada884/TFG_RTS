/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                     'B a s i c _ C o n s o l e _ I O _ C'
 *
 *                                     C
 *
 *
 * File 'basic_console_io_c.c'                                        By Mar.
 *
 *
 * C functions imported by 'Basic_Console_IO'.
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

#include <stdio.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_GNAT_ARM_BB
#define printc printf
#endif

void _marte_dio_put_int8(char c, int base)
{
  if (base == 16)
    printc("16#%x#", (int)c);
  else
    printc("%d", (int)c);
}

void _marte_dio_put_int32(int i, int base)
{
  if (base == 16)
    printc("16#%x#", i);
  else
    printc("%d", i);
}

void _marte_dio_put_unsigned8(unsigned char c, int base)
{
  if (base == 16)
    printc("16#%x#", (unsigned int)c);
  else
    printc("%u", (unsigned int)c);
}

void _marte_dio_put_unsigned32(unsigned int i, int base)
{
  if (base == 16)
    printc("16#%x#", i);
  else
    printc("%u", i);
}

void _marte_dio_put_unsigned64(unsigned long long i, int base)
{
  if (base == 16)
    printc("16#%qx#", i);
  else
    printc("%qu", i);
}

void _marte_dio_newline()
{
  printc ("\n");
}
