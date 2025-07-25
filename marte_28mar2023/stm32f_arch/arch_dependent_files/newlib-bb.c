/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                    Copyright (C) 2016-2018, AdaCore                      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <stdint.h>
#include <sys/stat.h>

/* Subprograms from System.Text_IO.  */
extern char system__text_io__initialized;
extern void system__text_io__initialize (void);
extern char system__text_io__is_tx_ready (void);
extern char system__text_io__is_rx_ready (void);
extern char system__text_io__use_cr_lf_for_new_line (void);
extern void system__text_io__put (char);
extern char system__text_io__get (void);

/* Assume that all fd are a tty.  */
int
isatty (int fd)
{
  return 1;
}

static void
write_console (char c)
{
  while (!system__text_io__is_tx_ready ())
    ;
  system__text_io__put (c);
}

static char
read_console (void)
{
  while (!system__text_io__is_rx_ready ())
    ;
  return system__text_io__get ();
}

int
// write (int fd, char *buf, int nbytes) MaRTE OS
write_newlib_bb (int fd, char *buf, int nbytes)
{
  int i;

  if (!system__text_io__initialized)
    system__text_io__initialize ();

  for (i = 0; i < nbytes; i++)
    {
      char c = buf[i];

      if (c == '\n' && system__text_io__use_cr_lf_for_new_line ())
	write_console ('\r');
      write_console (c);
    }

  return nbytes;
}

/* MaRTE OS
int
close (int fd)
{
  return 0;
   }
*/

int
fstat (int fd, struct stat*buf)
{
  return -1;
}

/* MaRTE OS
off_t
lseek (int fd, off_t offset, int whence)
{
  errno = ESPIPE;
  return -1;
}
*/

int
//read (int fd, char *buf, int count) MaRTE OS
read_newlib_bb (int fd, char *buf, int count)
{
  int i;

  if (!system__text_io__initialized)
    system__text_io__initialize ();

  for (i = 0; i < count;)
    {
      char c = read_console ();

      if (c == '\r' && system__text_io__use_cr_lf_for_new_line ())
	continue;
      buf[i++] = c;
      if (c == '\n')
	break;
    }
  return i;
}

/* __heap_start and __heap_end are defined in the commands script for the
   linker. They define the space of RAM that has not been allocated
   for code or data. */

extern void *__heap_start;
extern void *__heap_end;

void *
_sbrk (int nbytes)
{
  static void *heap_ptr = (void *)&__heap_start;
  void *base;

  if (((uintptr_t)&__heap_end - (uintptr_t)heap_ptr) >= nbytes)
    {
      base = heap_ptr;
      heap_ptr += nbytes;
      return base;
    }
  else
    {
      return (void *)-1;
    }
}
