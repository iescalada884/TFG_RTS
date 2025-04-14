
#define BASE_IRQ_MASTER_BASE 32
#define BASE_IRQ_SLAVE_BASE 40
/* Any change in these values should be refected in
   'BASE_IRQ_MASTER_BASE' and 'BASE_IRQ_SLAVE_BASE' (file
   'include/oskit/x86/pc/base_irq.h') and in 'PIC_Master_Base' and
   'PIC_Slave_Base' (file 'x86/hardware_interface.gpb') */

/* Local Apic Timer register map */
#define APIC_BASE  0xfee00000  // APIC base address
#define EOI_OFFSET 0x0b0       // End-Of-Interrupt Register offset
/* Any change in these values should be reflected in 'APIC_BASE'
   and 'EOI_OFFSET' (file 'x86/local_apic.adb'). */
