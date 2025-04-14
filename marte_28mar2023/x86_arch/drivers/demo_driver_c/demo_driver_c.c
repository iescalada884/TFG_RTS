/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                         'd e m o _ d r i v e r _ c'
 *
 *                                      C
 *
 *  File 'demo_driver_c.c'                                  by Fguerreira and
 *                                                             MAR.
 *
 *  A demostration C-driver. Just to show how drivers works in MaRTE
 *  OS. You can test this driver with program 'uses_demo_driver_c.c'
 *  in the 'examples/drivers/' directory.
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
#include <drivers/drivers_marte.h>

#define BUFFER_SIZE 30
char demo_buffer [BUFFER_SIZE] = "";


  /************/
 /*  Create  */
/************/

int demo_c_create ()
{
  return 0;
}


  /************/
 /*  Remove  */
/************/

int demo_c_remove ()
{
  return 0;
}



  /**********/
 /*  Open  */
/**********/

int demo_c_open (int file_descriptor, int file_access_mode)
{
  printf ("Demo C Diver: Opening device file %d (Major:%d, Minor:%d)."
	  " Mode %d\n", file_descriptor, 
	  get_major(file_descriptor), get_minor(file_descriptor),
	  file_access_mode);
  return 0;
}



  /***********/
 /*  Close  */
/***********/

int demo_c_close (int file_descriptor)
{
  printf ("Demo C Diver: Closing device file %d (Major:%d, Minor:%d)\n",
	  file_descriptor, 
	  get_major(file_descriptor), get_minor(file_descriptor));
  return 0;
}




  /**********/
 /*  Read  */
/**********/

ssize_t demo_c_read (int file_descriptor, void *buffer, size_t bytes)
{
  size_t i, bytes_read = 0;

  if (bytes < BUFFER_SIZE)
    bytes_read = bytes;
  else
    bytes_read = BUFFER_SIZE;

  for (i=0; i<bytes_read; i++)
    ((char *)buffer)[i] = demo_buffer[i];
  
  printf ("Demo C Diver: read %d bytes\n", bytes_read);
  return bytes_read;
}



  /***********/
 /*  Write  */
/***********/

ssize_t demo_c_write (int file_descriptor, void *buffer, size_t bytes)
{
  size_t i, bytes_written = 0;

  if (bytes < BUFFER_SIZE)
    bytes_written = bytes;
  else
    bytes_written = BUFFER_SIZE;

  for (i=0; i<bytes_written; i++)
    demo_buffer[i] = ((char *)buffer)[i];
  
  printf ("Demo C Diver: written %d bytes\n", bytes_written);
  return bytes_written;
}


  /***********/
 /*  Ioctl  */
/***********/

int demo_c_ioctl (int file_descriptor, int request, void* argp)
{
  printf ("Demo C Diver: Ioctl. Request:%d, argp:%s\n",
	  request, (char *) argp);
  return 0;
}


