/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                   'P r i n t e r   P o r t   D r i v e r'
 *
 *                                      H
 *
 * File 'printer_port.h'                                              by MAR.
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

#ifndef __MARTE_OS_PC_PRINTER_PORT_DRIVER__
#define __MARTE_OS_PC_PRINTER_PORT_DRIVER__  1

/*---------------------*
 *--  Ioctl commads  --*
 *---------------------*/
#define PP_ENABLE_INTERRUPTS      250
#define PP_DISABLE_INTERRUPTS     251

#define PP_SET_STROBE     252
#define PP_SET_AUTO_FEED     253
#define PP_SET_INIT_PRINTER     254
#define PP_SET_SELECT_INPUT     255

#define PP_RESET_STROBE     256
#define PP_RESET_AUTO_FEED     257
#define PP_RESET_INIT_PRINTER     258
#define PP_RESET_SELECT_INPUT     259

#define PP_READ_BUSY     260
#define PP_READ_ACKNOWLEDGE     261
#define PP_READ_PAPER_END     262
#define PP_READ_DEVICE_SELECT     263
#define PP_READ_ERROR     264

#endif  // __MARTE_OS_PC_KEYBOARD_DRIVER__
