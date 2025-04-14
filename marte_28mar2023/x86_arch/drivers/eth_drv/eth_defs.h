/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *
 *   MaRTE OS web page: http://marte.unican.es
 *
 *---------------------------------------------------------------------------
 *
 *                            'e t h _ d e f s . h'
 *
 *                                     C
 *
 *
 * File 'eth_defs.h'                                         Modified by Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>
 * Ethernet and driver definitions.
 *
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



#ifndef _MARTE_ETH_DEFS_H
#define _MARTE_ETH_DEFS_H

#include "osdep.h"
#include <stdint.h>

/*
   I'm moving towards the defined names in linux/if_ether.h for clarity.
   The confusion between 60/64 and 1514/1518 arose because the NS8390
   counts the 4 byte frame checksum in the incoming packet, but not
   in the outgoing packet. 60/1514 are the correct numbers for most
   if not all of the other NIC controllers.
*/



#define ETH_ALEN		6	/* Size of Ethernet address */
#define ETH_HLEN		14	/* Size of ethernet header */
#define	ETH_ZLEN		60	/* Minimum packet */
#define	ETH_FRAME_LEN		1514	/* Maximum packet */
#ifndef	ETH_MAX_MTU
#define	ETH_MAX_MTU		(ETH_FRAME_LEN-ETH_HLEN)
#endif

#define ARP_CLIENT	0
#define ARP_SERVER	1
#define ARP_GATEWAY	2
#define MAX_ARP		ARP_GATEWAY+1



/* MaRTE compability definitions*/

/* #define random eth_random */
#define printf eth_printf
#define vsprintf eth_vsprintf
#define sprintf eth_sprintf
/* #define putchar eth_putchar */
/* #define getchar eth_getchar */
#define pause eth_pause



//typedef unsigned char      uint8_t;
//typedef unsigned short     uint16_t;
//typedef unsigned long      uint32_t;
//typedef unsigned long long uint64_t;

//typedef signed char        int8_t;
//typedef signed short       int16_t;
//typedef signed long        int32_t;
//typedef signed long long   int64_t;


typedef struct {
        uint32_t        s_addr;
} in_addr;

struct rom_info {
        unsigned short  rom_segment;
        unsigned short  rom_length;
};


struct arptable_t {
	in_addr ipaddr;
	uint8_t node[ETH_ALEN];
};

extern struct arptable_t arptable[MAX_ARP];

/* misc.c */

//#define TICKS_PER_SEC           18

/* Inter-packet retry in ticks */
//#define TIMEOUT                 (10*TICKS_PER_SEC)
/* TICKS_PER_SEC is now a variable: ticks_per_sec*/
/* */

extern void eth_printf (const char *, ...);
extern int eth_sprintf (char *, const char *, ...);
//extern void putchar (int);
//extern int getchar (void);
extern int iskey (void);
//extern unsigned long currticks (void);


//extern unsigned long long hardware_interface__get_hwtime_slow(void);

#define currticks tsc__read_tsc
#define ticks_per_second marte__hal__get_hwclock_frequency

extern unsigned long long ticks_per_second(void);
extern unsigned long long currticks(void);

/* inizializated in nic.c : eth_create*/
unsigned long long ticks_per_sec;
unsigned long long tx_timeout;
/* Protocol definitions. */

#define RT_EP_PROTOCOL 0xA000
//#define RT_EP_PROTOCOL 0x0800  // IP Protocol


/* Usefull types definition.*/

#define TRUE      1
#define FALSE     0
typedef unsigned char boolean;


/* timer.c */

#define CONFIG_TSC_CURRTICKS

/* VERSION : Ethernet driver version.*/

#define VERSION "0.0"


/* General definitions: needed by driver */


#ifndef     NULL
#define NULL        ((void *)0)
#endif

/* RING_BUFFER_LEN : is the size in ethernet frames of the receiving RING */
/*                   queue. It can stores a maximum of                    */
/*                   (FIFO_RING_BUFFER_LEN -1) frames in buffer.          */

#define RING_BUFFER_LEN 500


/* pci_drivers is needed to detect Ethernet cards.*/
// extern const struct pci_driver pci_drivers[];



// we hace to define printc here because Mario deesn't have a proper
// header file with its definition.
extern int printc (const char *fmt, ...);

/* TIME_MEASURE : The use of this define is for timing purposes. */
/*                When is set causes the protocol to behave with */
/*                the active waiting in the transmit packet oper */
// Comment the line means disable timming measures.
//#define TIME_MEASURE
/* MAX_MEASURE_ITER: Defines the time in rounds at least we will */
/*                   be measuring. For now is the number of times*/
/*                   the interrupt handler acts. It show         */
/*                   informations of the measures each           */
/*                   MAX_MEASURE_ITER iterations.                */
#define MAX_MEASURE_ITER 50000
//Julio#define MAX_MEASURE_ITER 150000

#endif /*_MARTE_ETH_DEFS_H*/
