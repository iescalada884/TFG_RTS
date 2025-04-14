/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.51  Sep 2005
 *
 *                					'cmps03 driver'
 *
 *                                      H
 *
 *  File 'cmps03.h'                               by Sangorrin
 *
 *  IOCTL values and data types used by the cmps03 driver users
 *  For an Ada Interface have a look at cmps03.ads. 
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2004   Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
 *   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                      Michael González Harbour      mgh@unican.es
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

#ifndef _MARTE_CMPS03_H_
#define _MARTE_CMPS03_H_

/*-----------------------------------------------------------------------
 * 1.- CONSTANTS
 *-----------------------------------------------------------------------*/
#define SLAVE_ADDRESS 0x60
#define REG_CMPS03_BEARING_BYTE 1
#define REG_CMPS03_BEARING_WORD 2
#define CMPS03_I2C_PRIORITY 4
#define I2C_ADAPTER ELITE
#define I2C_OPERATION OP_1

/*-----------------------------------------------------------------------	
 * 2.- DATA TYPES
 *-----------------------------------------------------------------------*/
/* These types are used in the POSIX READ function. You can read the compass
   value as a byte, i.e. 0-255 for a full circle (which may be easier for some
   applications than 0-360 which requires two bytes. For those who require 
   better resolution 'bearing_word' is a 16 bit variable in the range 0-3599.
   This represents 0-359.9º*/ 
//    a) bearing_byte
typedef uint8_t bearing_byte;

//    b) bearing_word
typedef uint16_t bearing_word;

/*-----------------------------------------------------------------------
 * 3.- IOCTL COMMANDS and ARGUMENTS
 *-----------------------------------------------------------------------*/
/* Control the device through a Generic IOCTL Posix_IO function. You can 
   either check if the conversion is done with the 'GET_STATUS' command or
   call directly the POSIX READ function and be blocked until the job is done*/
//    a) CMPS03 IOCTL COMMANDS  and	(Args needed)	
typedef int cmps03_ioctl_cmd;
#define START_CONVERSION  0     /*Args: mode*/
#define GET_STATUS  1           /*Args: status*/

//    b) CMPS03 IOCTL ARGUMENTS 
typedef uint8_t bearing_mode;
#define BEARING_MODE_BYTE 0
#define BEARING_MODE_WORD 1

typedef uint8_t cmps03_status;
#define NO_CONVERSION_STARTED 1
#define CONVERSION_DONE 2
#define CONVERSION_IN_PROGRESS 3
#define CMPS03_ERROR 4
 
typedef struct {
  bearing_mode   mode;
  cmps03_status	status;
} cmps03_ioctl_arg;

#endif // _MARTE_CMPS03_H_ 
