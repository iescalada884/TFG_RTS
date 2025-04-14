/*----------------------------------------------------------------------------
 *--------------------        M a R T E     O S        -----------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                'L o c a l   A P I C   M a n a g e m e n t'
 *
 *                                   C
 *
 *
 * File 'local_apic_c.c'                                               By Mar.
 *
 * Code taken from Linux sources.
 *
 * Function 'enable_p6_local_apic' taken from 'uka_apic_timer.c'
 * (Module providing precise timers using the local APIC
 * timer. Copyright (C) 2000 Vincent Oberle (vincent@oberle.com)).
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

unsigned long long apic_timer_hz; /* CPU Bus cycles/sec. 
				   * Detected as we calibrate the APIC timer */

/* -----------------  Access to machine-specific registers ---------------
 *
 * Access to machine-specific registers (available on 586 and better only)
 * Note: the rd* operations modify the parameters directly (without using
 * pointer indirection), this allows gcc to optimize better
 */

#define rdmsr(msr,val1,val2) \
     __asm__ __volatile__("rdmsr" \
		          : "=a" (val1), "=d" (val2) \
                          : "c" (msr))

#define wrmsr(msr,val1,val2) \
     __asm__ __volatile__("wrmsr" \
                          : /* no outputs */ \
                          : "c" (msr), "a" (val1), "d" (val2))

#define rdtsc(low,high) \
     __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))

#define rdtscl(low) \
     __asm__ __volatile__ ("rdtsc" : "=a" (low) : : "edx")

#define rdtscll(val) \
     __asm__ __volatile__ ("rdtsc" : "=A" (val))

#define rdpmc(counter,low,high) \
     __asm__ __volatile__("rdpmc" \
                          : "=a" (low), "=d" (high) \
                          : "c" (counter))

// ------------------------ APIC ----------------------------
#define         APIC_BASE 0xfee00000
#define         APIC_ID         0x20
#define                 APIC_ID_MASK            (0x0F<<24)
#define                 GET_APIC_ID(x)          (((x)>>24)&0x0F)
#define         APIC_LVR        0x30
#define                 APIC_LVR_MASK           0xFF00FF
#define                 GET_APIC_VERSION(x)     ((x)&0xFF)
#define                 GET_APIC_MAXLVT(x)      (((x)>>16)&0xFF)
#define                 APIC_INTEGRATED(x)      ((x)&0xF0)
#define         APIC_TASKPRI    0x80
#define                 APIC_TPRI_MASK          0xFF
#define         APIC_ARBPRI     0x90
#define                 APIC_ARBPRI_MASK        0xFF
#define         APIC_PROCPRI    0xA0
#define         APIC_EOI        0xB0
#define                 APIC_EIO_ACK            0x0  /* Write this to the EOI 
							register */
#define         APIC_RRR        0xC0
#define         APIC_LDR        0xD0
#define                 APIC_LDR_MASK           (0xFF<<24)
#define                 GET_APIC_LOGICAL_ID(x)  (((x)>>24)&0xFF)
#define                 SET_APIC_LOGICAL_ID(x)  (((x)<<24))
#define                 APIC_ALL_CPUS           0xFF
#define         APIC_DFR        0xE0
#define         APIC_SPIV       0xF0
#define         APIC_ISR        0x100
#define         APIC_TMR        0x180
#define         APIC_IRR        0x200
#define         APIC_ESR        0x280
#define                 APIC_ESR_SEND_CS        0x00001
#define                 APIC_ESR_RECV_CS        0x00002
#define                 APIC_ESR_SEND_ACC       0x00004
#define                 APIC_ESR_RECV_ACC       0x00008
#define                 APIC_ESR_SENDILL        0x00020
#define                 APIC_ESR_RECVILL        0x00040
#define                 APIC_ESR_ILLREGA        0x00080
#define         APIC_ICR        0x300
#define                 APIC_DEST_SELF          0x40000
#define                 APIC_DEST_ALLINC        0x80000
#define                 APIC_DEST_ALLBUT        0xC0000
#define                 APIC_ICR_RR_MASK        0x30000
#define                 APIC_ICR_RR_INVALID     0x00000
#define                 APIC_ICR_RR_INPROG      0x10000
#define                 APIC_ICR_RR_VALID       0x20000
#define                 APIC_INT_LEVELTRIG      0x08000
#define                 APIC_INT_ASSERT         0x04000
#define                 APIC_ICR_BUSY           0x01000
#define                 APIC_DEST_LOGICAL       0x00800
#define                 APIC_DM_FIXED           0x00000
#define                 APIC_DM_LOWEST          0x00100
#define                 APIC_DM_SMI             0x00200
#define                 APIC_DM_REMRD           0x00300
#define                 APIC_DM_NMI             0x00400
#define                 APIC_DM_INIT            0x00500
#define                 APIC_DM_STARTUP         0x00600
#define                 APIC_DM_EXTINT          0x00700
#define                 APIC_VECTOR_MASK        0x000FF
#define         APIC_ICR2       0x310
#define                 GET_APIC_DEST_FIELD(x)  (((x)>>24)&0xFF)
#define                 SET_APIC_DEST_FIELD(x)  ((x)<<24)
#define         APIC_LVTT       0x320
#define         APIC_LVTPC      0x340
#define         APIC_LVT0       0x350
#define                 APIC_LVT_TIMER_BASE_MASK        (0x3<<18)
#define                 GET_APIC_TIMER_BASE(x)          (((x)>>18)&0x3)
#define                 SET_APIC_TIMER_BASE(x)          (((x)<<18))
#define                 APIC_TIMER_BASE_CLKIN           0x0
#define                 APIC_TIMER_BASE_TMBASE          0x1
#define                 APIC_TIMER_BASE_DIV             0x2
#define                 APIC_LVT_TIMER_PERIODIC         (1<<17)
#define                 APIC_LVT_MASKED                 (1<<16)
#define                 APIC_LVT_LEVEL_TRIGGER          (1<<15)
#define                 APIC_LVT_REMOTE_IRR             (1<<14)
#define                 APIC_INPUT_POLARITY             (1<<13)
#define                 APIC_SEND_PENDING               (1<<12)
#define                 GET_APIC_DELIVERY_MODE(x)       (((x)>>8)&0x7)
#define                 SET_APIC_DELIVERY_MODE(x,y)     (((x)&~0x700)|((y)<<8))
#define                         APIC_MODE_FIXED         0x0
#define                         APIC_MODE_NMI           0x4
#define                         APIC_MODE_EXINT         0x7
#define         APIC_LVT1       0x360
#define         APIC_LVTERR     0x370
#define         APIC_TMICT      0x380
#define         APIC_TMCCT      0x390
#define         APIC_TDCR       0x3E0
#define                 APIC_TDR_DIV_TMBASE     (1<<2)
#define                 APIC_TDR_DIV_1          0xB
#define                 APIC_TDR_DIV_2          0x0
#define                 APIC_TDR_DIV_4          0x1
#define                 APIC_TDR_DIV_8          0x2
#define                 APIC_TDR_DIV_16         0x3
#define                 APIC_TDR_DIV_32         0x8
#define                 APIC_TDR_DIV_64         0x9
#define                 APIC_TDR_DIV_128        0xA

#define APIC_DIVISOR 16

#define LOCAL_TIMER_VECTOR      0x2a

#define APIC_VECTOR_MASK        0x000FF

#define APIC_BASE_MSR           0x1B


/*
 * APIC Write and Read operations
 */
__inline void apic_write(unsigned int reg, unsigned int v)
{
  *((volatile unsigned int *)(APIC_BASE+reg))=v;
}

__inline unsigned int apic_read(unsigned int reg)
{
  return *((volatile unsigned int *)(APIC_BASE+reg));
}

/*--------------------------*
 *-- calibrate_apic_timer --*
 *--------------------------*/
/*
 * Uses the PIT counter 2 (speaker).
 */
#define CLOCK_TICK_RATE 1193180
#define HZ 100
#define LATCH  ((CLOCK_TICK_RATE + HZ/2) / HZ)
#define CALIBRATE_LATCH (5 * LATCH)
#define CALIBRATE_TIME  (5 * 1000020/HZ)
#define rdtsc(low,high) __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))
extern unsigned long long cpu_hz; // defined in 'time_stamp_counter_c.c'

void calibrate_apic_timer(void)
{ 
  if (cpu_hz == 0) 
    // TSC not calibrated yet
    goto bad_calibration;

  /* Set the Gate high, disable speaker */
  outb(0x61, (inb(0x61) & ~0x02) | 0x01);

  /*
   * Now let's take care of PIT counter 2
   *
   * Set the Gate high, program PIT counter 2 for mode 0,
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
    unsigned int t1 = 0xFFFFFFFF;
    unsigned int t2;


    /*
     * Start the counter for calibration.
     * We assume that enable_p6_local_apic was already called.
     */
    rdtsc(startlow, starthigh);
    apic_write(APIC_TMICT, t1);
    count = 0;
    do {
      count++;
    } while ((inb(0x61) & 0x20) == 0);
    t2 = apic_read(APIC_TMCCT);
    rdtsc(endlow, endhigh);

    /* Error: EPITNEVERSET */
    if (count <= 1) {
      printe ("Error: PIT NEVER SET\n");
      goto bad_calibration;
    }

    /* 64-bit subtract - gcc just messes up with long longs */
    __asm__("subl %2,%0\n\t"
	    "sbbl %3,%1"
	    :"=a" (endlow), "=d" (endhigh)
	    :"g" (startlow), "g" (starthigh),
	    "0" (endlow), "1" (endhigh));

    /* Error: ECPUTOOFAST */
    if (endhigh) {
      printe ("Error: APIC TOO FAST\n");
      goto bad_calibration;
    }

    /* Error: ECPUTOOSLOW */
    if (endlow <= CALIBRATE_TIME) {
      printe ("Error: APIC TOO SLOW\n");
      goto bad_calibration;
    }

    apic_timer_hz = ((t1 - t2) * cpu_hz) / endlow;
    printc("Detected %ld Hz APIC timer clock.\n", 
	   (unsigned long) apic_timer_hz);
    return;
  }

  /*
   * The PIT wasn't reliable: we got a hit on the very first read,
   * or the CPU was so fast/slow that the quotient wouldn't fit in
   * 32 bits or the TSC hasn't been calibrated yet.
   */
 bad_calibration:
  apic_timer_hz = 0;
}


/*--------------------------*
 *-- enable_p6_local_apic --*
 *--------------------------*/
/* Enables the local APIC in "Through Mode". */
void enable_p6_local_apic (unsigned int timer_int_vector_num,
			   unsigned int divide_value)
{
        unsigned long msr_low_orig, dummy;
        unsigned long value;
        unsigned long lvr, maxlvt;

        printc("Enabling apic... ");
  
        rdmsr(APIC_BASE_MSR, msr_low_orig, dummy);
        wrmsr(APIC_BASE_MSR, msr_low_orig|(1<<11), 0);
  
        /* value = apic_read(APIC_SPIV); */
        value = 0x0000030F; // Disable focus proc. checking, enable APIC
        apic_write(APIC_SPIV, value);

        lvr = apic_read(APIC_LVR);
        if( !APIC_INTEGRATED(lvr) )
                goto not_local_p6_apic;
        maxlvt = GET_APIC_MAXLVT(lvr);
        if( maxlvt < 4 )  // MaRTE if( maxlvt != 4 )
                goto not_local_p6_apic;

        value = 0x00008700; // Level Trigger, deliver to ExtINT.
        apic_write(APIC_LVT0, value);
  
        value = 0x00000400; // Deliver to NMI
        apic_write(APIC_LVT1, value);
  
        /* apic_read(APIC_SPIV); */
        apic_write(APIC_ESR, 0);
        /* value = apic_read(APIC_ESR); */
        /* value = apic_read(APIC_LVTERR); */
        value = 0x00010000; // Mask Error Interrupt
        apic_write(APIC_LVTERR, value);
        /* apic_read(APIC_SPIV); */
        apic_write(APIC_ESR, 0);
        /* value = apic_read(APIC_ESR); */

        apic_write(APIC_LVTT, 
		   timer_int_vector_num); // Unmask timer int and set vector
  
        value = 0x00010000; // Mask Performace-monitoring counter Interrupt
        apic_write(APIC_LVTPC, value);

        /* Divide configuration register */
        apic_write(APIC_TDCR, divide_value);

        printc("enabled local P6 APIC\n");

        /* Calibrate APIC timer */
	calibrate_apic_timer();

        return;
  
 not_local_p6_apic:
        printe("APIC isn't local P6\n");
        apic_write(APIC_SPIV, 0xF);
        wrmsr(APIC_BASE_MSR, msr_low_orig, 0);
}

/*------------------------*
 *-- set_in_bypass_mode --*
 *------------------------*/
void set_in_bypass_mode ()
{
        unsigned long msr_low_orig, dummy, value;
  
        /* value = apic_read(APIC_SPIV); */
        value = 0x0000000F; // Disable APIC
        apic_write(APIC_SPIV, value);
   
	/* Disable access to APIC adress space (APIC in bypass mode) */
        rdmsr(APIC_BASE_MSR, msr_low_orig, dummy);
        wrmsr(APIC_BASE_MSR, msr_low_orig&~(1<<11), 0);
}

int local_apic_get_apic_id() {
  return GET_APIC_ID(apic_read(APIC_ID));
} 

extern int usleep(int usec);

void local_apic_AP_startup() {
  int i;

  // send an INIT IP
  apic_write(APIC_ICR, APIC_DEST_ALLBUT | APIC_INT_ASSERT | APIC_DM_INIT);

  // wait until finished
  while (apic_read(APIC_ICR) & APIC_ICR_BUSY);

  // delay 10ms
  printc("delay 10ms\n");
  usleep(10000);

  // send STARTUP IPI twice
  for(i=0; i<2; i++) {
    // send STARTUP IPI:
    apic_write(APIC_ICR,
               APIC_DEST_ALLBUT | APIC_INT_ASSERT | APIC_DM_STARTUP | 0x11);

    // wait until finished
    while (apic_read(APIC_ICR) & APIC_ICR_BUSY);

    // delay 200us
    printc("delay 200us\n");
    usleep(200);
  }

  printc("APs initialization IPIs done\n");
}

void local_apic_send_ipi(int target_proc_id) {
  apic_write(APIC_ICR2, SET_APIC_DEST_FIELD(target_proc_id));

  apic_write(APIC_ICR, APIC_INT_ASSERT | 0xfe);
  //apic_write(APIC_ICR, 0x05);
}
