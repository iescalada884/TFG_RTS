/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                   'r t c'
 *
 *                                      H
 *
 * File 'rtc.h'                                                       by Mar.
 *
 * Exported functions:
 *  'show_rtc_registers_on_console'
 *  'show_rtc_time_on_console'
 *  'get_rtc_time'
 *  'rtc_time_to_seconds_since_epoch'
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

#ifndef _RTC_H_
#define _RTC_H_

struct rtc_time {
  unsigned char year;  // year two last digits (1999 => 99 and 2000 => 0)
  unsigned char month; // range [1,12]
  unsigned char day;   // range [1,31]
  unsigned char hour;  // range [0,23]
  unsigned char min;   // range [0,59]
  unsigned char sec;   // range [0,59]
};

void show_rtc_registers_on_console();
void get_rtc_time (struct rtc_time *time);
void show_rtc_time_on_console();
int  rtc_time_to_seconds_since_epoch ();

#endif // _RTC_H_
