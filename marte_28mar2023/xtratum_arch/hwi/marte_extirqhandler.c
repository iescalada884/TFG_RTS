
//#include "std_c.h"
#include <xm.h>
#include <irqs.h>
#include <stdio.h>

extern void ExtIrqHandlerAda(int irqnr); // defined in 'marte-hal.adb'
extern int XM_write_console(char *buffer, int length);

#define XTRATUM_HWIRQ_OFFSET 0x20  //  XtratuM 3.7
#define XTRATUM_EXTENDED_IRQ 224  //  XtratuM 3.7

int marte_to_xm_interrupt (int hwirq) {
	
	return hwirq + XTRATUM_HWIRQ_OFFSET;	
}

int xm_to_marte_interrupt (int xmirq) {
	
	if (xmirq < XTRATUM_EXTENDED_IRQ) { 
		return (xmirq - XTRATUM_HWIRQ_OFFSET);	
	}
	else // Extended interrupts
	    return xmirq;
}

void HwIrqHandler(trapCtxt_t *ctxt) {
   //XM_write_console ("HwIrqHandler",12);
   //printf("Irq %d", ctxt->irqNr);
   ExtIrqHandlerAda(xm_to_marte_interrupt (ctxt->irqNr));
}

void install_handler (int irqnr) {

  //printf ("Installing %d\n", irqnr);
  InstallTrapHandler(marte_to_xm_interrupt (irqnr), HwIrqHandler);
}

int are_irqs_enabled () {
  return HwIsSti ();
}

void unmask (int irqnr) {
  //printf ("Clear %d\n", irqnr);
  if (irqnr < XTRATUM_EXTENDED_IRQ)
    XM_clear_irqmask((1<<marte_to_xm_interrupt (irqnr)), 0);
  else
    XM_clear_irqmask(0, (1<<marte_to_xm_interrupt (irqnr)));
}

void mask (int irqnr) {
  //printf ("Mask %d\n", irqnr);
  if (irqnr < XTRATUM_EXTENDED_IRQ)
    XM_set_irqmask((1<<marte_to_xm_interrupt (irqnr)), 0);
  else
    XM_set_irqmask(0, (1<<marte_to_xm_interrupt (irqnr)));
}
