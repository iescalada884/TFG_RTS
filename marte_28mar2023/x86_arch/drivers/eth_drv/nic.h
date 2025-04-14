/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *
 *   MaRTE OS web page: http://marte.unican.es
 *
 *----------------------------------------------------------------------------
 *
 *                                'n i c . h'
 *
 *                                     C
 *
 *
 * File 'nic.h'                                              Modified by Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>
 * Ethernet basic driver definitions

 *---------------------------------------------------------------------------*/
/*****************************************************************************/
/* Most of this module is taken from the Etherboot proyect:                  */
/* http://etherboot.sourceforge.net v.5.1.3                                  */


 /*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 */

#ifndef	NIC_H
#define NIC_H

#include "dev.h"

/*
 *  Structure returned from eth_probe and passed to other driver
 *  functions.
 */
struct nic
{
        struct dev    dev;  /* This must come first */
        int       (*poll)(struct nic *);
        void      (*transmit)(struct nic *, const unsigned char *d,
        unsigned int t, unsigned int s, const unsigned char *p);
        int (*handler)(unsigned short protocol_filter); // Interrups handler.
        int (*open)(struct nic *); //open function.
        int       flags;  /* driver specific flags */


        struct rom_info   *rom_info;  /* -> rom_info from main */
        unsigned char *node_addr;
        unsigned char *packet;
        unsigned int  packetlen;
        unsigned char irq; /* Assigned IRQ values (0 - 15)*/

        void      *priv_data; /* driver can hang private data here */
};

extern struct nic nic;
extern int eepro100_probe(struct dev *dev);
extern int sis900_probe(struct dev *dev);
extern int rtl8139_probe(struct dev *dev);
extern int pcnet32_probe(struct dev *dev);

#endif	/* NIC_H */
