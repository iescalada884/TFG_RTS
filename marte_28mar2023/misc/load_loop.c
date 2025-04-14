/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *                                  'l o a d'
 *
 *                                      C
 *
 * File 'load.c'                                                       by MAR.
 *
 * Functions to "eat" CPU-time.
 * The amount of time is consumed using a loop that iterates the
 * appropriate number of times
 *
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
#include <time.h>
#include <stdlib.h> // for 'labs()'
#include <misc/load_loop.h>

static long loops_per_second = 30000;

/*
 * eat
 *
 * Executes during the interval of time 'For_Seconds' 
 */
void eat (float for_seconds)
{
    long num_loop = (long)(loops_per_second * (float)for_seconds);
    long j = 1;
    long i;
  
    for (i=1; i<=num_loop; i++) {
        j++;
        if (j<i) {
            j = i-j;
        } else {
            j = j-1;
        }
    }
}

long subtract (struct timespec *a, struct timespec *b)
{
    long result, nanos;

    result = (a->tv_sec  - b->tv_sec)*1000000;
    nanos  = (a->tv_nsec - b->tv_nsec)/1000;
    return (result+nanos);
}


/*
 * adjust
 *
 * Measures the CPU speed (to be called before any call to 'eat')
 */
void adjust (void)
{
    struct timespec initial_time, final_time;
    long interval;
    int number_of_tries =0;
    long adjust_time = 1000000;
    int max_tries = 6;

    do {
        clock_gettime (CLOCK_REALTIME, &initial_time);
        eat(((float)adjust_time)/1000000.0);
        clock_gettime (CLOCK_REALTIME, &final_time);
        interval = subtract(&final_time,&initial_time);
        loops_per_second = (long)( 
       	      (float)loops_per_second*(float)adjust_time/(float)interval);
        number_of_tries++;
    } while (number_of_tries<=max_tries &&
	     labs(interval-adjust_time)>=adjust_time/50);
}


