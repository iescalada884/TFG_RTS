/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                    'T e x t   C o n s o l e   D r i v e r'
 *
 *                                      C
 *
 *  File 'text_console.c'                                         By MAR.
 *
 *  MaRTE OS on XtratuM.
 *
 *  Console driver. Just a wrapper to 'XM_write_console' hypercall.
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

#include <sys/types.h>
#include <stdint.h>

// Function XM_write_console <xm/include/hypervisor.h>
//   xm_s32_t XM_write_console(char *buffer, xm_s32_t length)
// Don't include <xm.h> directly to avoid redefined symbols between MaRTE and
// XtratuM headers.
extern int32_t XM_write_console(char *buffer, int32_t length);


/************
 *  Create  *
 ************/

int text_console_create ()
{
  return 0;
}


/************
 *  Remove  *
 ************/

int text_console_remove ()
{
  return 0;
}



/**********
 *  Open  *
 **********/

int text_console_open (int file_descriptor, int file_access_mode)
{
  return 0;
}



/***********
 *  Close  *
 ***********/

int text_console_close (int file_descriptor)
{
  return 0;
}




/**********
 *  Read  *
 **********/

ssize_t text_console_read (int file_descriptor, void *buffer, size_t bytes)
{
  return 0;
}



/***********
 *  Write  *
 ***********/

ssize_t text_console_write (int file_descriptor, void *buffer, size_t bytes)
{
  XM_write_console((char *) buffer, bytes);
  return bytes;
}


/***********
 *  Ioctl  *
 ***********/

int text_console_ioctl (int file_descriptor, int request, void* argp)
{
  return 0;
}


/*------------------------------------*
 *--  End of kernel initialization  --*
 *------------------------------------*/
void text_console_end_of_kernel_initialization ()
{
}
