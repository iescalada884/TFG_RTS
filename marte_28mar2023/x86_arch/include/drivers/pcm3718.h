/*----------------------------------------------------------------------------
 *-- -------------------         M a R T E   O S         ------------------ --
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                 'pcm3718 driver'
 *
 *                                      H
 *
 * File 'pcm3718.h'                                              by Sangorrin
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
 *  IOCTL values and data types used by the pcm3718 driver users
 *  There are some important constants in pcm3718.ads
 *
 *---------------------------------------------------------------------------*/
#ifndef _MARTE_PCM3718_H_
#define _MARTE_PCM3718_H_

#include <stdint.h>

// 1.- CONSTANTS
#define AD_Code_Min      0 // Constants needed by the user program
#define AD_Code_Max   4095 // to convert Samples into real varia
#define AD_Code_Range 4096 // bles like Temperature, Voltage

// 2.- DATA TYPES
//    a) DIGITAL DATA
typedef uint16_t digital_data;

//    b) ANALOG DATA
typedef uint8_t channel_type; // range 0 .. Num_Channels-1
typedef uint16_t sample_type; // range AD_Code_Min .. AD_Code_Max
typedef struct{
   sample_type the_sample;
   channel_type the_channel;
} analog_data_type;
typedef uint32_t num_conv_type; // range 0 .. Buffer_Mx;

// 3.- IOCTL COMMANDS and ARGUMENTS
// Control the device through a Generic IOCTL Posix_IO function.
//    a) Digital I/O COMMANDS (Args needed)
typedef int dio_ioctl_cmd;
#define MODE_BYTE_1  0     // (No Args) RW only the first 8bit register
#define MODE_BYTE_2  1     // (No Args) RW only the second 8bit register
#define MODE_WORD    2	  // (No Args) RW both as a 16bit register

//    b) Analog Input COMMANDS (Args needed)
typedef int ai_ioctl_cmd;
#define SET_RANGE_OF_CHANNEL  0 //(Input_Range,Start_Ch)
#define SET_PARAMETERS        1 //(Trigger,Start_Ch,Stop_Ch,[Mode,Count,C1,C2])
#define GET_STATUS            2 //(Num_Conv) for scan_mode
#define FLUSH                 3 //(No Args) for scan_mode

//    c) ARGUMENTS
// Range of the input voltage
typedef uint8_t range_type;
#define BIPOLAR_5      0 //            -5 < Vin < 5
#define BIPOLAR_2_5    1 //          -2.5 < Vin < 2.5
#define BIPOLAR_1_25   2 //         -1.25 < Vin < 1.25
#define BIPOLAR_0_625  3 //        -0.625 < Vin < 0.625
#define UNIPOLAR_10    4 //             0 < Vin < 10
#define UNIPOLAR_5     5 //             0 < Vin < 5
#define UNIPOLAR_2_5   6 //             0 < Vin < 2.5
#define UNIPOLAR_1_25  7 //             0 < Vin < 1.25
#define BIPOLAR_10     8 //           -10 < Vin < 10

// -- There are FIVE configurations
// -- 1) SOFTWARE TRIGGER
// -- 2) PACER TRIGGER + FIXED MODE
// -- 3) PACER TRIGGER + SCAN MODE
// -- 4) EXTERNAL TRIGGER + FIXED MODE
// -- 5) EXTERNAL TRIGGER + SCAN MODE
// ------------------------------------------------------------------
// -- When Fixed Mode is set the driver adquires "Count" samples
// -- each time you call READ.
// ------------------------------------------------------------------
// -- When Scan Mode is set the driver is continuosly storing
// -- samples in a internal Buffer (It can overwrite the old ones),
// -- and when you call READ you read the samples in the Buffer.
// ------------------------------------------------------------------

typedef uint8_t trigger_type;
#define SOFTWARE 0
#define EXTERNAL 1
#define PACER    2

typedef uint8_t mode_type;
#define FIXED 0
#define SCAN  1

// -- SCAN_RATE_TYPE
// -- This is the rate used to acquire data when using PACER TRIGGER.
// -- If F_Clock_Pacer = 1E6Hz => Scan_Rate is in seconds*10e-6 (uS)
// -- If F_Clock_Pacer = 10E6Hz => Scan_Rate is in seconds*10e-7
// -- The driver will try to aproximate this rate and return this
// -- aproximation on the same variable of the Ioctl Argument.

typedef uint32_t scan_rate_type;

typedef struct {
   range_type   input_range;
   channel_type start_ch;
   channel_type stop_ch;
   trigger_type trigger;
   mode_type    mode;
   num_conv_type count;
   scan_rate_type scan_rate;
} ai_ioctl_arg;

#endif // _MARTE_PCM3718_H_
