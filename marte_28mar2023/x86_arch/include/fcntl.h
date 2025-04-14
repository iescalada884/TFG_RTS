/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                'f c n t l'
 *
 *                                      H
 *
 * File 'fcntl.h'                                              by Fguerreira
 *
 *    File control options
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

#ifndef _MARTE_FCNTL_H_
#define _MARTE_FCNTL_H_

#include <sys/types.h>
#include <sys/marte_general_constants.h>


/*--------------------------------------------------------------*/
/* File access modes used for open() */

/* Open for reading only.*/
#define O_RDONLY _MARTE_O_RDONLY

/* Open for writing only.*/
#define O_WRONLY _MARTE_O_WRONLY

/* Open for reading and writing.*/
#define O_RDWR _MARTE_O_RDWR

/* Open in blocking mode (reading (writing) from the device will block
   the thread until unless one byte has been read (written)) */
#define O_NONBLOCK _MARTE_O_NONBLOCK

/* Create File in case it does not exist */
#define O_CREAT _MARTE_O_CREAT

#define O_TRUNC 01000

/* Dummy values */
#define O_APPEND 0
#define O_NOCTTY 0
#define O_ACCMODE 0

/* Open the file specified by 'path', with access defined by
   'oflag'. */
int  open(const char *path, int oflag);

#endif /* _MARTE_FCNTL_H_ */

