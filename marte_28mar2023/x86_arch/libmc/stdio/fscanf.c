/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                'f s c a n f'
 *
 *                                      C
 *
 * File 'fscanf.c'                                                    by Mar.
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
#include "doscan.h"

int vfscanf(FILE *fh, const char *fmt, va_list args)
{
  doscan_source_t source;
  source.buf = NULL; // no buff
  source.fd  = fileno(fh);
  return _doscanf(&source, fmt, args);
}

int fscanf(FILE *fh, const char *fmt, ...)
{
  int nmatch = 0;
  va_list	args;
  doscan_source_t source;
  source.buf = NULL; // no buff
  source.fd  = fileno(fh);

  va_start(args, fmt);
  nmatch = _doscanf(&source, fmt, args);	
  va_end(args);

  return nmatch;
}

int scanf(const char *fmt, ...)
{
  int nmatch = 0;
  va_list	args;
  doscan_source_t source;
  source.buf = NULL; // no buff
  source.fd  = STDIN_FILENO; // scan from console

  va_start(args, fmt);
  nmatch = _doscanf(&source, fmt, args);	
  va_end(args);

  return nmatch;
}

