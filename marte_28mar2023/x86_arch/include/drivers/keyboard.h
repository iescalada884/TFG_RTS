/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                         'K e y b o a r d   D r i v e r'
 *
 *                                      H
 *
 * File 'keyboard.h'                                                 by MAR.
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

#ifndef __MARTE_OS_PC_KEYBOARD_DRIVER__
#define __MARTE_OS_PC_KEYBOARD_DRIVER__  1

/*---------------------*
 *--  Ioctl commads  --*
 *---------------------*/
#define PC_KEYBOARD_SET_COOKED_MODE 150
/* "cooked mode": line editing is allowed, the textual unit of input
   in this mode is an entire "line" of text, where a "line" is a
   sequence of characters terminated by the line termination character
   CR. Thus, characters typed in this mode are not immediately made
   available to the calling program. They are first buffered to allow
   the user to perform line editing (erase characteres) on them. */

#define PC_KEYBOARD_SET_RAW_MODE 151
/* "raw mode": Every character is made available to the calling
   program as soon as it is typed, so no line editing is available in
   this mode. */

#define PC_KEYBOARD_ENABLE_ECHO 152
/* Characters are echoed to the console as they are typed. */

#define PC_KEYBOARD_DISABLE_ECHO 153
/* Characters are not echoed to the console. */

#define SET_BLOCKING_MODE 154
/*  Tasks are blocked in case there are not enough characters in
    buffer to fulfill a read operation. */

#define RESET_BLOCKING_MODE 155
/*  Read operations returns immediately the available characters
    (maybe none). */

#define PC_KEYBOARD_SAVE_MODE 156
/*  Save the current mode (Cooked or raw, echo and blocking) */

#define PC_KEYBOARD_RESTORE_MODE 157
/*  Restore the saved mode */

#endif  // __MARTE_OS_PC_KEYBOARD_DRIVER__
