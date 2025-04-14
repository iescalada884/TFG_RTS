/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                'p r i n t e'
 *
 *                                      C
 *
 * File 'printe.c'                                                     by MAR.
 *
 * Write directly on stderr. To be used inside kernel for debugging
 * purposes. The file system is not used.
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
#include <stdarg.h>
#include <unistd.h>
#include "doprint.h"

/*
 * vprinte
 */
int vprinte(const char *fmt, va_list args)
{
  doprint_destiny_t destiny;
  destiny.max = SPRINTF_UNLIMITED;
  destiny.len = 0;
  destiny.buf = NULL; // no buff
  destiny.fd  = -STDERR_FILENO; // '-STDERR_FILENO' is the special
	             // value used to indicate '_doprint' we want to
	             // write directly to stderr without passing
	             // through the file system.
  return _doprint(&destiny, fmt, args);
}

/*
 * printe
 */
int printe (const char *fmt, ...)
{
  va_list	args;
  int err;
  va_start(args, fmt);
  err = vprinte(fmt, args);
  va_end(args);

  return err;
}
