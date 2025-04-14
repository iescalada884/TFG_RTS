/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *                                 'r a n d'
 *
 *                                      C
 *
 * File 'rand.c'                                            by Chema and MAR.
 *
 * Pseudo-random number generator functions. Uses a method used by VMS
 * FORTRAN, VMS BASIC, and others:
 *         RANDOM NUMBER = NEW SEED = (69069 * SEED + 1) mod 2**32
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
#include <stdlib.h>
                                                                               
static unsigned rand_seed = 0;

#define POSITIVE_INT_MASK 0x7fffffff
/*
 * rand
 */
int rand(void)
{                                                                      
  rand_seed = ((69069 * rand_seed) + 1);
  return (int) (rand_seed & POSITIVE_INT_MASK);
}

/*
 * srand
 */
void srand(unsigned int seed){
  rand_seed = seed;
}
