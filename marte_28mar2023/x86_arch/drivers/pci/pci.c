/*----------------------------------------------------------------------------
 *-------------------------      P C I     L I B      ------------------------
 *----------------------------------------------------------------------------
 *
 *                                  'p c i'
 *
 *                                     C
 *
 *                                               
 * File 'pci.c'                                                       By Chema.
 *                                                          Jose Maria Martinez
 *                                                         <martinjm@unican.es>
 *
 * Some PCI access and ethernet scan functions.
 *
 * This is not a driver to be installed in the MaRTE drivers framework
 * but a library to be used by drivers. The reason for having put it
 * into the 'drivers/' directory is because the functions defined here
 * will mainly be used by drivers.
 *
 *                ------------------------------
 * Most of this is taken from:
 *
 * /usr/src/linux/drivers/pci/pci.c
 * /usr/src/linux/include/linux/pci.h
 * /usr/src/linux/arch/i386/bios32.c
 * /usr/src/linux/include/linux/bios32.h
 * /usr/src/linux/drivers/net/ne.c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 *---------------------------------------------------------------------------*/

#include <sys/pci.h>
#include "pio.h"
  

#ifndef NULL
#define NULL            0
#endif

//#define	DEBUG	1


/* Direct PCI access mode -> CONFIG_PCI_DIRECT else PCBIOS*/
#define CONFIG_PCI_DIRECT 
//#define PCBIOS


#define  PCIBIOS_SUCCESSFUL                0x00

/*
 * Functions for accessing PCI configuration space with type 1 accesses
 */

#define CONFIG_CMD(bus, device_fn, where)   (0x80000000 | (bus << 16) | (device_fn << 8) | (where & ~3))



int pcibios_read_config_byte(unsigned int bus, unsigned int device_fn,
			       unsigned int where, uint8_t *value)
{
    outl(CONFIG_CMD(bus,device_fn,where), 0xCF8);
    *value = inb(0xCFC + (where&3));
    return PCIBIOS_SUCCESSFUL;
}

int pcibios_read_config_word (unsigned int bus,
    unsigned int device_fn, unsigned int where, uint16_t *value)
{
    outl(CONFIG_CMD(bus,device_fn,where), 0xCF8);
    *value = inw(0xCFC + (where&2));
    return PCIBIOS_SUCCESSFUL;
}

int pcibios_read_config_dword (unsigned int bus, unsigned int device_fn,
				 unsigned int where, uint32_t *value)
{
    outl(CONFIG_CMD(bus,device_fn,where), 0xCF8);
    *value = inl(0xCFC);
    return PCIBIOS_SUCCESSFUL;
}

int pcibios_write_config_byte (unsigned int bus, unsigned int device_fn,
				 unsigned int where, uint8_t value)
{
    outl(CONFIG_CMD(bus,device_fn,where), 0xCF8);
    outb(value, 0xCFC + (where&3));
    return PCIBIOS_SUCCESSFUL;
}

int pcibios_write_config_word (unsigned int bus, unsigned int device_fn,
				 unsigned int where, uint16_t value)
{
    outl(CONFIG_CMD(bus,device_fn,where), 0xCF8);
    outw(value, 0xCFC + (where&2));
    return PCIBIOS_SUCCESSFUL;
}

int pcibios_write_config_dword (unsigned int bus, unsigned int device_fn, 
				unsigned int where, uint32_t value)
{
    outl(CONFIG_CMD(bus,device_fn,where), 0xCF8);
    outl(value, 0xCFC);
    return PCIBIOS_SUCCESSFUL;
}

int pcibios_set_irq_routing(struct pci_device *dev, int pin, int irq){
  pcibios_write_config_byte (dev->bus, dev->devfn,
                             PCI_INTERRUPT_LINE, irq);
  pcibios_write_config_byte (dev->bus, dev->devfn,
                             PCI_INTERRUPT_PIN, pin);
  return 1;
}



int pci_read_config_byte(struct pci_device *dev, 
				unsigned int where, 
				uint8_t *value){
  return pcibios_read_config_byte(dev->bus, dev->devfn, where, value);
}

int pci_write_config_byte(struct pci_device *dev, 
			  unsigned int where, 
			  uint8_t value){
  return pcibios_write_config_byte(dev->bus, dev->devfn, where, value);
}

int pci_read_config_word(struct pci_device *dev, 
				unsigned int where, 
				uint16_t *value){
  return pcibios_read_config_word(dev->bus, dev->devfn, where, value);
}

int pci_write_config_word(struct pci_device *dev, 
				 unsigned int where, 
				 uint16_t value){
  return pcibios_write_config_word(dev->bus, dev->devfn, where, value);
}

int pci_read_config_dword(struct pci_device *dev, 
				 unsigned int where, 
				 uint32_t *value){
  return pcibios_read_config_dword(dev->bus, dev->devfn, where, value);
}

int pci_write_config_dword(struct pci_device *dev, 
				  unsigned int where, 
				  uint32_t value){
  return pcibios_write_config_dword(dev->bus, dev->devfn, where, value);
}


/**********************************************************************/

int pci_find_device(unsigned short vendor, 
		    unsigned short device, 
		    const struct pci_device *from,
		    struct pci_device *found){

  unsigned int first_bus, first_devfn;
  //const struct pci_driver *first_driver;
  unsigned int devfn, bus, buses;
  unsigned char hdr_type = 0;
  uint32_t class;
  uint16_t vendor_id, device_id;
  uint32_t l, romaddr;
  int i;
  unsigned char irq, pin; /* Added to support interrupts */
  int re_scan=0;
  //  struct pci_device *dev;
  static uint32_t addresses[] = {
    PCI_BASE_ADDRESS_0,
    PCI_BASE_ADDRESS_1,
    PCI_BASE_ADDRESS_2,
    PCI_BASE_ADDRESS_3,
    PCI_BASE_ADDRESS_4,
    PCI_BASE_ADDRESS_5,
    0
};

  
  first_bus    = 0;
  first_devfn  = 0;

  /* If we want to detect another device of the same vendor from is the last */
  /* detected device.*/
  if (from != NULL) {
    first_bus    = from->bus;
    first_devfn  = (from->devfn + 0x0001 ) % 0x100;
    re_scan=1;	
    pcibios_read_config_byte(first_bus, first_devfn & ~0x0007, 
			     PCI_HEADER_TYPE, &hdr_type);

  }
  /* Scan all PCI buses, until we find our card.
   * We could be smart only scan the required buses but that
   * is error prone, and tricky.
   * By scanning all possible pci buses in order we should find
   * our card eventually. 
   */
  buses=256;
  for (bus = first_bus; bus < buses; bus++) {
    for (devfn = first_devfn; devfn < 0x100; devfn++) {

      if (PCI_FUNC (devfn) == 0)
	pcibios_read_config_byte(bus, devfn, PCI_HEADER_TYPE, &hdr_type);

      else if (!(hdr_type & 0x80))	/* not a multi-function device */
	continue;

      // Read the vendor (16 bits) + device (16 bits) identifier.
      pcibios_read_config_dword(bus, devfn, PCI_VENDOR_ID, &l);
      /* Some broken boards return 0 or ~0 if a slot is empty: */
      if (l == 0xffffffff || l == 0x00000000  
	|| l == 0x0000ffff || l == 0xffff0000) {
	continue;
      }
      /* We now can extract the vendor and the device identifier.*/
      vendor_id = l & 0xffff;
      device_id = (l >> 16) & 0xffff;
      
      /* Once We have identified the PCI device (vendor + device),          */
      /* we have to check if it is the required hardware.                   */
      /* If vendor == 0 and device == 0 then returns the first device found */

      if( vendor != 0 && device != 0) {
	if ( !((vendor == vendor_id) && (device == device_id)) ){
	  continue;
	}
      }
      /* Once here, we have found the device, so we can interrogate it  */
      /* for some params.                                               */
      pcibios_read_config_byte(bus, devfn, PCI_INTERRUPT_LINE, &irq);
      pcibios_read_config_byte(bus, devfn, PCI_INTERRUPT_PIN, &pin);
      pcibios_read_config_dword(bus, devfn, PCI_REVISION, &l);
      class = (l >> 8) & 0xffffff;
      
      found->devfn = devfn;
      found->bus = bus;
      found->class = class;
      found->vendor = vendor_id;
      found->dev_id = device_id;
      found->irq = irq;
      found->pin = pin;
       
          
      /* Get the ROM base address */
      pcibios_read_config_dword(bus, devfn, 
				PCI_ROM_ADDRESS, &romaddr);
      romaddr >>= 10;
      found->romaddr = romaddr;
      
      /* This pair are deprecated. we set the to 0*/
      found->ioaddr = 0;
      found->membase= 0;
   
        for (i=0; addresses[i]; i++) {
            uint32_t curr, mask;
            
	    found->pci_region[i].base_addr=0;
	    //   found->pci_region[i].space='N';
            pci_read_config_dword(found, addresses[i],&curr);

	    /* We do not want that an interrupt arise when the device is in */
	    /* other address very far from the proper one.*/

	    cli();
	    pci_write_config_dword(found, addresses[i], ~0);
	    pci_read_config_dword(found, addresses[i], &mask);
            pci_write_config_dword(found, addresses[i], curr);
	    sti();
            
	    if (!mask){
	      found->pci_region[i].base_addr= 0x0;
	      found->pci_region[i].space='N';
	      found->pci_region[i].size = 0;
	      continue; /* there may be other regions */
	    }
	    /*
	     * apply the I/O or memory mask to current position
	     * note that I/O is limited to 0xffff, and 64-bit is not
	     * supported by this simple imeplementation
	     */
	    if (curr & PCI_BASE_ADDRESS_SPACE_IO) {
		curr &= PCI_BASE_ADDRESS_IO_MASK;
		found->ioaddr=curr;
		found->pci_region[i].base_addr=curr;
	    } else {
		curr &= PCI_BASE_ADDRESS_MEM_MASK;
		found->membase=curr; 	
		found->pci_region[i].base_addr=curr; 
	    }
 
            /* extract the type, and the programmable bits */
            if (mask & PCI_BASE_ADDRESS_SPACE_IO) {
	      found->pci_region[i].space='I';
	      mask &= PCI_BASE_ADDRESS_IO_MASK;
	      found->pci_region[i].size = (~mask + 1) & 0xffff; /* Bleah */

            } else {
	      found->pci_region[i].space='M';
	      mask &= PCI_BASE_ADDRESS_MEM_MASK;
	      found->pci_region[i].size = ~mask + 1;
            }
	} //end for addresses
      return 0; // correct.
    }//enf for.
    first_devfn = 0;
  }//end for.
  first_bus = 0;
  return -1; // Error. Device not found.
}//end function pci_find_device.

/**********************************************************************/
/*
 *	Set device to be a busmaster in case BIOS neglected to do so.
 *	Also adjust PCI latency timer to a reasonable value, 32.
 */

void adjust_pci_device(struct pci_device *p){
  unsigned short	new_command, pci_command;
  unsigned char	pci_latency;
  
  pcibios_read_config_word(p->bus, p->devfn, PCI_COMMAND, &pci_command);
  new_command = pci_command | PCI_COMMAND_MASTER|PCI_COMMAND_IO;
  if (pci_command != new_command) {
    pcibios_write_config_word(p->bus, p->devfn, PCI_COMMAND, new_command);
  }
  pcibios_read_config_byte(p->bus, p->devfn, PCI_LATENCY_TIMER, &pci_latency);
  if (pci_latency < 32) {
    pcibios_write_config_byte(p->bus, p->devfn, PCI_LATENCY_TIMER, 32);
  }
}

/*
 * Find the start of a pci resource.
 */
unsigned long pci_bar_start(struct pci_device *dev, unsigned int bar){
  uint32_t start;
  pci_read_config_dword(dev, bar, &start);
  if (start & PCI_BASE_ADDRESS_SPACE_IO) {
    start &= PCI_BASE_ADDRESS_IO_MASK;
  } else {
    start &= PCI_BASE_ADDRESS_MEM_MASK;
  }
  return start;
}

/*
 * Find the size of a pci resource.
 */
unsigned long pci_bar_size(struct pci_device *dev, unsigned int bar){
  uint32_t start, size;
  /* Save the original bar */
  pci_read_config_dword(dev, bar, &start);
  /* We do not want that an interrupt arise when the device is in other*/
  /* address very far from the proper one.*/
  cli();
  /* Compute which bits can be set */
  pci_write_config_dword(dev, bar, ~0);
  pci_read_config_dword(dev, bar, &size);
  sti();
  /* Restore the original size */
  pci_write_config_dword(dev, bar, start);
  /* Find the significant bits */
  if (start & PCI_BASE_ADDRESS_SPACE_IO) {
    size &= PCI_BASE_ADDRESS_IO_MASK;
  } else {
    size &= PCI_BASE_ADDRESS_MEM_MASK;
  }
  /* Find the lowest bit set */
  size = size & ~(size - 1);
  return size;
}
