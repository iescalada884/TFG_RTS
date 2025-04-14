/*----------------------------------------------------------------------------
 *--------------------        M a R T E     O S        -----------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                  'T i m e - S t a m p   C o u n t e r'
 *
 *                                   C
 *
 *
 * File 'time_stamp_counter_c.c'                                       By Mar.
 *
 * Code taken from 'linux/arch/i386/kernel/time.c' (Kernel 2.4.18)
 * Copyright (C) 1991, 1992, 1995  Linus Torvalds
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
#define CLOCK_TICK_RATE 1193180
#define HZ 100
#define LATCH  ((CLOCK_TICK_RATE + HZ/2) / HZ)

unsigned long long cpu_hz = 0; /* CPU cycles/sec. 
				* Detected as we calibrate the TSC */

#define rdtsc(low,high) \
      __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))

static unsigned long last_tsc_low;

#define CALIBRATE_LATCH (5 * LATCH)
#define CALIBRATE_TIME  (5 * 1000020/HZ)

/* 
 * calibrate_tsc
 *
 * Give value to 'cpu_hz'.
 * Too much 64-bit arithmetic here to do this cleanly in C, and for
 * accuracy's sake we want to keep the overhead on the CTC speaker (channel 2)
 * output busy loop as low as possible. We avoid reading the CTC registers
 * directly because of the awkward 8-bit access mechanism of the 82C54
 * device.
 */

void calibrate_tsc(void)
{
  printc ("Calibrating TSC... ");

  /* Set the Gate high, disable speaker */
  outb(0x61, (inb(0x61) & ~0x02) | 0x01);

  /*
   * Now let's take care of CTC channel 2
   *
   * Set the Gate high, program CTC channel 2 for mode 0,
   * (interrupt on terminal count mode), binary count,
   * load 5 * LATCH count, (LSB and MSB) to begin countdown.
   */
  outb(0x43, 0xb0);                       /* binary, mode 0, LSB/MSB, Ch 2 */
  outb(0x42, CALIBRATE_LATCH & 0xff);     /* LSB of count */
  outb(0x42, CALIBRATE_LATCH >> 8);       /* MSB of count */

  {
    unsigned long startlow, starthigh;
    unsigned long endlow, endhigh;
    unsigned long count;
    unsigned long eax=0, edx=1000000;
    unsigned long cpu_hz_long;

    rdtsc(startlow,starthigh);
    count = 0;
    do {
      count++;
    } while ((inb(0x61) & 0x20) == 0);
    rdtsc(endlow,endhigh);
    last_tsc_low = endlow;

    /* Error: ECTCNEVERSET */
    if (count <= 1) {
      printe ("Error: CTC NEVER SET\n");
      goto bad_ctc;
    }

    /* 64-bit subtract - gcc just messes up with long longs */
    __asm__("subl %2,%0\n\t"
	    "sbbl %3,%1"
	    :"=a" (endlow), "=d" (endhigh)
	    :"g" (startlow), "g" (starthigh),
	    "0" (endlow), "1" (endhigh));

    /* Error: ECPUTOOFAST */
    if (endhigh) {
      printe ("Error: CPU TOO FAST\n");
      goto bad_ctc;
    }

    /* Error: ECPUTOOSLOW */
    if (endlow <= CALIBRATE_TIME) {
      printe ("Error: CPU TOO SLOW\n");
      goto bad_ctc;
    }

    __asm__("divl %2"
	    :"=a" (endlow), "=d" (endhigh)
	    :"r" (endlow), "0" (0), "1" (CALIBRATE_TIME));

    // 'endlow' now is 2^32 * (1 / (TSC clocks per usec))
  
    /* report CPU clock rate in Hz.
     * The formula is (10^6 * 2^32) / (2^32 * 1 / (clocks/us)) =
     * clock/second. Our precision is about 100 ppm.
     */
    __asm__("divl %2"
	    :"=a" (cpu_hz_long), "=d" (edx)
	    :"r" (endlow),
	    "0" (eax), "1" (edx));
    printc("Detected %lu Hz processor.\n", cpu_hz_long);
    cpu_hz = (unsigned long long)cpu_hz_long;
    return;
  }

  /*
   * The CTC wasn't reliable: we got a hit on the very first read,
   * or the CPU was so fast/slow that the quotient wouldn't fit in
   * 32 bits..
   */
 bad_ctc:
  cpu_hz = 0;
}
