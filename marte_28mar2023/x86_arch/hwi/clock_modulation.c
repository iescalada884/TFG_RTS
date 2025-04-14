/*----------------------------------------------------------------------------
 *--------------------        M a R T E     O S        -----------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                  'C l o c k _ M o d u l a t i o n'
 *
 *                                   C
 *
 *
 * File 'clock_modulation.c'                                       By Daniel M.
 *
 *  Module the clock signal to change the CPU frequency
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
#include <sys/clock_modulation.h>

#define IA32_CLOCK_MODULATION        0x19A
#define     CLOCK_MODULATION_MASK    0x1E
#define     CM_DUTYCYCLE_MASK        0x0E

#define EINVAL 22

extern void eat (float for_seconds);
extern void adjust (void);
extern unsigned long long marte__hal__tsc__read_tsc();

float cm_dutycycle_ratios[8];

/*
 * clock_modulation_set_dutycyclelevel
 */
int clock_modulation_set_dutycyclelevel(char dutycycle) {
// EINVAL if dutycycle is invalid
  unsigned eax;

  // dutycycle bits cannot be 0x0
  if ( ( (dutycycle & CM_DUTYCYCLE_MASK)>>1 ) < 0 ) { return -EINVAL; }
  // dutycicle level cannot be higher than 0x1E
  if ( ( (dutycycle & CLOCK_MODULATION_MASK)>>1 ) > 0x1E ) { return -EINVAL; }

  // read register to keep unused bits
  asm volatile("rdmsr" : "=a" (eax) : "c" (IA32_CLOCK_MODULATION) );
  eax = (eax & ~CLOCK_MODULATION_MASK) | (dutycycle & CLOCK_MODULATION_MASK);
  asm volatile("wrmsr" : : "a" (eax), "d" (0), "c" (IA32_CLOCK_MODULATION) );

  return 0;
}

/*
 * clock_modulation_get_dutycyclelevel
 */
char clock_modulation_get_dutycyclelevel() {
  unsigned eax;
  asm volatile("rdmsr" : "=a" (eax) : "c" (IA32_CLOCK_MODULATION) );
  if ( eax & 0x10 ) 
    return eax & CLOCK_MODULATION_MASK;
  else
    return CLOCK_MODULATION_DUTYCYCLE_100;
}

/*
 * clock_modulation_get_dutycycle
 *
 * Returns the frequency ratio asigned to the dutycycle
 */
float clock_modulation_get_dutycycle(char dutycycle) {
  // dutycycle bits cannot be 0x0
  if ( ( (dutycycle & CM_DUTYCYCLE_MASK)>>1 ) < 0 ) { return -EINVAL; }
  // dutycicle level cannot be higher than 0x1E
  if ( ( (dutycycle & CLOCK_MODULATION_MASK)>>1 ) > 0x1E ) { return -EINVAL; }

  if (dutycycle == CLOCK_MODULATION_DUTYCYCLE_100)
    return cm_dutycycle_ratios[0];
  else
    return ( cm_dutycycle_ratios[(dutycycle & CM_DUTYCYCLE_MASK)>>1 ] );
}

/*
 * clock_modulation_is_supported
 *
 * Check if it is supported in the processor
 */
int clock_modulation_is_supported() {
  unsigned int eax, ecx;
  // read cpuid max registers, clock_modulation flag is at register 6
  asm volatile("cpuid" : "=a" (eax) : "a" (0x00) );
  if ( eax < 0x06 ) { return -1; }
  // read cpuid(6) clock_modulation flag (0x01)
  asm volatile("cpuid" : "=c" (ecx) : "a" (0x06) );
  if ( !(ecx & 0x0001) ) { return -1; }

  return 1;
}


/*
 * clock_modulation_calibrate
 *
 * Calibrate the frequency values asigned to each dutycycle level
 */
int clock_modulation_calibrate () {
  unsigned long long tsc1, tsc2, full_tsc=0;
  int perf, ret;

  cm_dutycycle_ratios[0] = 1.0;
  adjust();

  for ( perf = 0x20 ; perf > 0x11; perf -= 2) {
    if ( perf > 0x1F ) {
      ret = clock_modulation_set_dutycyclelevel(CLOCK_MODULATION_DUTYCYCLE_100);
      if ( ret != 0 ) {
        printf("error 1. %i\n", ret);
      }
    } else {
      ret = clock_modulation_set_dutycyclelevel(perf);
      if ( ret != 0 ){
        printf("error 2. %i\n", ret);
      }
    }

      tsc1 =marte__hal__tsc__read_tsc();
    eat(0.1);
      tsc2 =marte__hal__tsc__read_tsc();

    if (perf == 0x20) {full_tsc = tsc2 - tsc1; }
    cm_dutycycle_ratios[(perf-0x10)>>1] = (float)full_tsc/(float)(tsc2-tsc1);
  } // for()

  // Back to 100%
  ret = clock_modulation_set_dutycyclelevel(CLOCK_MODULATION_DUTYCYCLE_100);
  if (ret != 0 ) {
    printf("error 3. %i\n", ret);
  }

  return 0;
}
/* */

