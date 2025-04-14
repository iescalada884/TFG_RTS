/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                             'l o c a l t i m e'
 *
 *                                      C
 *
 * File 'localtime.c'                                                  by Mar.
 *
 * Based in 'oskit/freebsd/src/lib/libc/stdtime/localtime.c'.
 *
 * Export functions:
 *  'mktime'
 *  'localtime'
 *  'gmtime'
 *
 * The time zone is not implemented.
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

#include <time.h>
#include "private_time.h"

const static int days_to_month[MONSPERYEAR+1] = 
{  0 /* Jan */,  31 /* Feb */,  59 /* Mar */, 
  90 /* Apr */, 120 /* May */, 151 /* Jun */,
 181 /* Jul */, 212 /* Aug */, 243 /* Sep */,
 273 /* Oct */, 304 /* Nov */, 334 /*Dic */,
 365 /* end year */}; 
const static int days_to_month_leap[MONSPERYEAR+1] = 
{  0 /* Jan */,  31 /* Feb */,  60 /* Mar */, 
  91 /* Apr */, 121 /* May */, 152 /* Jun */,
 182 /* Jul */, 213 /* Aug */, 244 /* Sep */,
 274 /* Oct */, 305 /* Nov */, 335 /*Dic */,
 366 /* end year */}; 

/*------------*
 *-- mktime --*
 *------------*/
time_t mktime(struct tm *timeptr)
{
  time_t days;

  days = 365 * (timeptr->tm_year - BASE_TO_EPOCH_YEAR);
  days += leap_years_from_epoch(timeptr->tm_year + TM_YEAR_BASE); // leap years

  if (isleap(timeptr->tm_year + TM_YEAR_BASE)) 
    days += days_to_month_leap[timeptr->tm_mon];
  else 
    days += days_to_month[timeptr->tm_mon];

  days += timeptr->tm_mday - 1;

  return days * SECSPERDAY              +    timeptr->tm_hour * SECSPERHOUR 
    + timeptr->tm_min * SECSPERMIN      +    timeptr->tm_sec;
}

/*-----------------*
 *-- localtime_r --*
 *-----------------*/
struct tm *localtime_r(const time_t *timep, 
		       struct tm *result)
{
  int m;
  //int num_of_leap_years; (MaRTE OS: not used)

  result->tm_mday = *timep / SECSPERDAY;
  result->tm_hour = *timep % SECSPERDAY / SECSPERHOUR;
  result->tm_min  = *timep % SECSPERHOUR / SECSPERMIN;
  result->tm_sec  = *timep % SECSPERMIN;
  result->tm_isdst = 0; 
  result->tm_wday = (result->tm_mday + EPOCH_WDAY) % DAYSPERWEEK;

  result->tm_year = result->tm_mday / DAYSPERNYEAR;
  result->tm_year = (result->tm_mday - 
		     leap_years_from_epoch(result->tm_year 
					   + EPOCH_YEAR)) / DAYSPERNYEAR;
  
  result->tm_mday = (result->tm_mday - 
		     leap_years_from_epoch(result->tm_year 
					   + EPOCH_YEAR)) % DAYSPERNYEAR;

  result->tm_year += BASE_TO_EPOCH_YEAR;
  
  if (isleap(result->tm_year + TM_YEAR_BASE)) {
    for(m=TM_JANUARY; days_to_month_leap[m] <= result->tm_mday; m++);
    result->tm_mon = m - 1;
    result->tm_yday = result->tm_mday;
    result->tm_mday -= days_to_month_leap[m - 1];
    result->tm_mday += 1;
  } else {   
    for(m=TM_JANUARY; days_to_month[m] <= result->tm_mday; m++);
    result->tm_mon = m - 1;
    result->tm_yday = result->tm_mday;
    result->tm_mday -= days_to_month[m - 1];
    result->tm_mday += 1;
  }

  return result;
}


/*---------------*
 *-- localtime --*
 *---------------*/
struct tm *localtime(const time_t *timep)
{
  static struct tm tms = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  
  return localtime_r (timep, &tms);
}

/*------------*
 *-- gmtime --*
 *------------*/
struct tm *gmtime(const time_t *timep)
{
  return localtime(timep);
}
