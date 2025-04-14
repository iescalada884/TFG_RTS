/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                    'w r i t e _ r e a d _ w r a p p e r'
 *
 *                                      C
 *
 * File 'write_read_wrapper.c'                                        by MAR.
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
#include <errno.h>
#include <unistd.h>
extern ssize_t linux_read(int fd, void *buf, size_t count);
extern ssize_t linux_write(int fd, const void *buf, size_t count);
// defined in 'linux_syscalls/lib/syscall_stdio.c'



// Read wrapper
//
// a-textio.adb get_immediate doesn't work when this read is uncommented
/*ssize_t read(int fd, void *buf, size_t count)
{
  ssize_t ret;
  char * chr_ptr = (char *) buf;

  do {
    if ((ret = linux_read(fd, chr_ptr, count)) != -1) {
      // No error
      count -= ret;
      chr_ptr += ret;
    } else {
      // Error
      if (errno != EINTR) return -1;
    }
  } while (count > 0);
  return ((int)buf - (int)chr_ptr);
}*/

// Write wrapper
ssize_t write(int fd, const void *buf, size_t count)
{
  ssize_t ret;
  char * chr_ptr = (char *) buf;

  do {
    if ((ret = linux_write(fd, chr_ptr, count)) != -1) {
      // No error
      count -= ret;
      chr_ptr += ret;
    } else {
      // Error
      if (errno != EINTR) return -1;
    }
  } while (count > 0);
  return ((int)buf - (int)chr_ptr);
}
