/*----------------------------------------------------------------------------
 *-- -------------------         M a R T E   O S         ------------------ --
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                            'C l o c k _ M o d u l a t i o n'
 *
 *                                      H
 *
 * File 'clock_modulation.h'                                       by Daniel M.
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

#ifndef _MARTE_CLK_MODULATION_H_
#define _MARTE_CLK_MODULATION_H_

#define     CLOCK_MODULATION_DUTYCYCLE_100    0x06
#define     CLOCK_MODULATION_DUTYCYCLE_086    0x1E
#define     CLOCK_MODULATION_DUTYCYCLE_075    0x1C
#define     CLOCK_MODULATION_DUTYCYCLE_063    0x1A
#define     CLOCK_MODULATION_DUTYCYCLE_050    0x18
#define     CLOCK_MODULATION_DUTYCYCLE_037    0x16
#define     CLOCK_MODULATION_DUTYCYCLE_025    0x14
#define     CLOCK_MODULATION_DUTYCYCLE_012    0x12


int clock_modulation_set_dutycyclelevel(char dutycycle);
// EINVAL if dutycycle is invalid

char clock_modulation_get_dutycyclelevel();

float clock_modulation_get_dutycycle(char dutycycle);

int clock_modulation_is_supported();

int clock_modulation_calibrate();

#endif // _MARTE_CLK_MODULATION_H_
