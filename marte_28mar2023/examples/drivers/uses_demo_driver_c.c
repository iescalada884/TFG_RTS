/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                     'u s e s _ d e m o _ d r i v e r _ c'
 *
 *                                      C
 *
 *  File 'uses_demo_driver_c.c'                                        by MAR.
 *
 *  This program uses the demonstration C-driver in
 *  'drivers/demo_driver_c/' directory. In order to run the program
 *  you should install that driver in the system. To do so, uncomment
 *  the appropriate lines in 'kernel/k-devices_table.ads'. After that
 *  you must recompile the kernel (p.e. executing 'mkkernel -gnatn
 *  -gnatp -O3').
 *
 *  For more information about installing drivers in MaRTE OS refer to
 *  the "MaRTE OS User's Guide" (marte_ug.html).
 *
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
 *   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                      Michael Gonzalez Harbour      mgh@unican.es
 *
 *  MaRTE OS  is free software; you can  redistribute it and/or  modify it
 *  under the terms of the GNU General Public License  as published by the
 *  Free Software Foundation;  either  version 2, or (at  your option) any
 *  later version.
 *
 *  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 *  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 *  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 *  General Public License for more details.
 *
 *  You should have received  a  copy of  the  GNU General Public  License
 *  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 *  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 *  02111-1307, USA.
 *
 *  As a  special exception, if you  link this  unit  with other  files to
 *  produce an   executable,   this unit  does  not  by  itself cause  the
 *  resulting executable to be covered by the  GNU General Public License.
 *  This exception does  not however invalidate  any other reasons why the
 *  executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#define ERROR(s) {perror (s); exit (-1);}

int main ()
{
  int fd;
  int count;
  char msj_from_driver[20];

  if ((fd = open ("/dev/demo_c", O_RDWR)) == -1)
    ERROR ("open");

  if ((count = write(fd, "Hello driver!!", 15)) == -1)
    ERROR ("write");

  printf ("Application: written %d bytes in the device file\n", count);

  if ((count = read(fd, msj_from_driver, 15)) == -1)
    ERROR ("read");

  printf ("Application: read '%s' (%d bytes) from device file\n", 
	  msj_from_driver, count);

  if (ioctl(fd, 8, "command") == -1)
    ERROR ("ioctl");

  if (close(fd) == -1)
    ERROR ("close");

  return 0;
}


