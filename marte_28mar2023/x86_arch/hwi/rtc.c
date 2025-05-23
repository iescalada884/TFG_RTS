/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                   'r t c'
 *
 *                                      C
 *
 * File 'rtc.c'                                                       by Mar.
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

#include <stdio.h>
#include <sys/pio.h>
#include <sys/rtc.h>
#include <time.h>

#define BCD_TO_BIN(val) ((val)=((val)&15) + ((val)>>4)*10) 

/*
 * RTC Register locations
 */
#define IO_RTC          0x070           /* RTC */

#define RTC_SEC         0x00    /* seconds */
#define RTC_MIN         0x02    /* minutes */
#define RTC_HRS         0x04    /* hours */
#define RTC_DAY         0x07    /* day of month */
#define RTC_MONTH       0x08    /* month of year */
#define RTC_YEAR        0x09    /* month of year */

#define RTC_STATUSA     0x0a    /* status register A */
#define  RTCSA_TUP       0x80   /* time update, don't look now */
#define RTC_STATUSB     0x0b    /* status register B */
#define RTC_INTR        0x0c    /* status register C (R) interrupt source */
#define RTC_STATUSD     0x0d    /* status register D (R) Lost Power */


/*-----------*
 *-- rtcin --*
 *-----------*
 *
 * Return the value of REG in the NVRAM.
 */
static unsigned char rtcin(unsigned char reg)
{
  unsigned char r;

  outb_p(IO_RTC, reg);
  r = inb_p(IO_RTC + 1);

  return r;
}

/*------------*
 *-- rtcout --*
 *------------*
 *
 * Set the value of REG to VAL in the NVRAM.
 */
/*static void rtcout(unsigned char reg, unsigned char val)
{
  outb_p(IO_RTC, reg);
  outb_p(IO_RTC + 1, val);
}*/

/*-----------------------------------*
 *-- show_rtc_registers_on_console --
 *-----------------------------------*/
void show_rtc_registers_on_console()
{
  unsigned char rega, regb, regc, regd;
  rega = rtcin(RTC_STATUSA);
  regb = rtcin(RTC_STATUSB);
  regc = rtcin(RTC_INTR); // status register C (R) interrupt source
  regd = rtcin(RTC_STATUSD);
  printf("RTC Reg A: %x\n", (unsigned int)rega);
  printf("RTC Reg B: %x\n", (unsigned int)regb);
  printf("RTC Reg C: %x\n", (unsigned int)regc);
  printf("RTC Reg D: %x\n", (unsigned int)regd);
}

/*------------------*
 *-- get_rtc_time --*
 *------------------*/
void get_rtc_time (struct rtc_time *time)
{
  int flags;

  while( rtcin(RTC_STATUSA) & RTCSA_TUP ); // wait for update end 
  save_flags(flags);
  cli();
  time->year  = rtcin(RTC_YEAR);
  time->month = rtcin(RTC_MONTH);
  time->day   = rtcin(RTC_DAY);
  time->hour  = rtcin(RTC_HRS);
  time->min   = rtcin(RTC_MIN);
  time->sec   = rtcin(RTC_SEC);
  restore_flags(flags);

  BCD_TO_BIN(time->year);
  BCD_TO_BIN(time->month);
  BCD_TO_BIN(time->day);
  BCD_TO_BIN(time->hour);
  BCD_TO_BIN(time->min);
  BCD_TO_BIN(time->sec);
}

/*------------------------------*
 *-- show_rtc_time_on_console --*
 *------------------------------*/
void show_rtc_time_on_console()
{
  struct rtc_time t;

  get_rtc_time (&t);

  printf("RTC time: %u/%u/%u %u:%u:%u\n", 
	 (unsigned int)t.month, (unsigned int)t.day, (unsigned int)t.year, 
	 (unsigned int)t.hour,  (unsigned int)t.min, (unsigned int)t.sec);
}

/*-------------------------------------*
 *-- rtc_time_to_seconds_since_epoch --*
 *-------------------------------------*/
int rtc_time_to_seconds_since_epoch ()
{
  struct rtc_time t;
  struct tm stm;

  get_rtc_time (&t);
  
  stm.tm_year = (t.year < 70)? t.year + 100: t.year; 
  // 'stm.tm_year' stores the number of years since 1900
  stm.tm_mon  = t.month - 1;
  // In 'struct tm' the months are numbered from 0 to 11
  stm.tm_mday = t.day;
  stm.tm_hour = t.hour;
  stm.tm_min  = t.min;
  stm.tm_sec  = t.sec;
  
  return mktime (&stm);
}



