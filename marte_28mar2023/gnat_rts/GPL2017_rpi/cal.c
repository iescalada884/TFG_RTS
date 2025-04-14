/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                   C A L                                  *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2014, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This file contains those routines named by Import pragmas in package    */
/*  GNAT.Calendar. It is used to do Duration to timeval conversion.         */
/*  These are simple wrappers function to abstract the fact that the C      */
/*  struct timeval fields type are not normalized (they are generally       */
/*  defined as int or long values).                                         */

// MaRTE OS

#include <sys/time.h>

void
__gnat_timeval_to_duration (struct timeval *t, long long *sec, long *usec)
{
  *sec  = (long long) t->tv_sec;
  *usec = (long) t->tv_usec;
}

void
__gnat_duration_to_timeval (long long sec, long usec, struct timeval *t)
{
  /* here we are doing implicit conversion to the struct timeval
     fields types. */

  t->tv_sec = sec;
  t->tv_usec = usec;
}
