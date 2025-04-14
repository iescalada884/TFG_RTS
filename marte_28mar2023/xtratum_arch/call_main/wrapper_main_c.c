/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *
 *                        'w r a p p e r _ m a i n _ c'
 *
 *                                      C
 *
 * File 'wrapper_main_c.c'                                             by MAR.
 *
 * The function 'PartitionMain' is called by XtratuM after having initialized
 * the platform.
 *
 * At linking time is included either this file or 'wrapper_main_c.o' depending
 * on the language of the main procedure of the application.
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
extern int init_dynamic_memory();
// Defined in:
//  - 'dynamic_memory/TLSF-x.x.x/tlsf_marte.c' (for TLSF)
//  - 'dynamic_memory/simple_dynamic_memory/simple_dynamic_memory_functions.c'
//    (for simple dynamic memory)


extern int main(int argc, char *argv[], char *envp[]);
extern void marte_init();  // MaRTE Ada packages initialization
extern void marte_final(); // MaRTE Ada packages finalization
extern void marte_c_init();  // MaRTE Ada packages initialization
extern void marte_c_final(); // MaRTE Ada packages finalization
//extern void init_lang_supp (void);   XtratuM
//extern void finish_lang_supp (void); XtratuM

extern void marte__hal__install_hw_interrupt_handler(int irqnr,
						   void (*handler)(int irqnr));
extern int marte__hal__timer_interrupt ();
extern void timer_interrupt_handler (int irqnr);
extern void marte__hal__hardware_interrupt_controller_enable_interrupt
                                                                 (int irqnr);
extern void marte__hal__enable_interrupts ();

extern int xprintf(const char *format, ...);
extern int XM_write_console(char *buffer, int length);

void marte_init_remaining_vcpus () {
	
  // Set the Timer IRQ for each VCPU
  marte__hal__install_hw_interrupt_handler(marte__hal__timer_interrupt(),
					   timer_interrupt_handler);
  marte__hal__enable_interrupts ();                                            
  marte__hal__hardware_interrupt_controller_enable_interrupt
    (marte__hal__timer_interrupt());
}

#include <irqs.h>
#include <xm.h>

void PartitionMain(void)
{
  int ret;

  SetupIrqs(); // It is required for each VCPU in XtratuM
    
  if (XM_get_vcpuid()==0) {  //  XtratuM SMP
		init_dynamic_memory();

  		marte_init();   // Call elaboration of MaRTE Ada packages  
		marte_c_init(); // Call elaboration of marte_os_c.ads 

		// init_lang_supp(); XtratuM
  }
  else {
	    marte_init_remaining_vcpus ();
  }

  ret = main(0, 0, 0);

  // finish_lang_supp(); XtratuM

  marte_c_final();
  marte_final();
}
