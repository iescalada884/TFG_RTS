/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *
 *                            'e e p r o 1 0 0 . c'
 *
 *                                     C
 *
 *
 * File 'eepro100.c'                                         Modified by Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>
 * eepro100 driver for MaRTE OS.
 *
 *---------------------------------------------------------------------------*/

/*
 * eepro100.c -- This file implements the eepro100 driver for etherboot.
 *
 *
 * Copyright (C) AW Computer Systems.
 * written by R.E.Wolff -- R.E.Wolff@BitWizard.nl
 *
 *
 * AW Computer Systems is contributing to the free software community
 * by paying for this driver and then putting the result under GPL.
 *
 * If you need a Linux device driver, please contact BitWizard for a
 * quote.
 *
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *              date       version  by   what
 *  Written:    May 29 1997  V0.10  REW  Initial revision.
 * changes:     May 31 1997  V0.90  REW  Works!
 *              Jun 1  1997  V0.91  REW  Cleanup
 *              Jun 2  1997  V0.92  REW  Add some code documentation
 *              Jul 25 1997  V1.00  REW  Tested by AW to work in a PROM
 *                                       Cleanup for publication
 *
 * This is the etherboot intel etherexpress Pro/100B driver.
 *
 * It was written from scratch, with Donald Beckers eepro100.c kernel
 * driver as a guideline. Mostly the 82557 related definitions and the
 * lower level routines have been cut-and-pasted into this source.
 *
 * The driver was finished before Intel got the NDA out of the closet.
 * I still don't have the docs.
 * */

/* Philosophy of this driver.
 *
 * Probing:
 *
 * Using the pci.c functions of the Etherboot code, the 82557 chip is detected.
 * It is verified that the BIOS initialized everything properly and if
 * something is missing it is done now.
 *
 *
 * Initialization:
 *
 *
 * The chip is then initialized to "know" its ethernet address, and to
 * start recieving packets. The Linux driver has a whole transmit and
 * recieve ring of buffers. This is neat if you need high performance:
 * you can write the buffers asynchronously to the chip reading the
 * buffers and transmitting them over the network.  Performance is NOT
 * an issue here. We can boot a 400k kernel in about two
 * seconds. (Theory: 0.4 seconds). Booting a system is going to take
 * about half a minute anyway, so getting 10 times closer to the
 * theoretical limit is going to make a difference of a few percent.
 *
 *
 * Transmitting and recieving.
 *
 * We have only one transmit descriptor. It has two buffer descriptors:
 * one for the header, and the other for the data.
 * We have only one receive buffer. The chip is told to recieve packets,
 * and suspend itself once it got one. The recieve (poll) routine simply
 * looks at the recieve buffer to see if there is already a packet there.
 * if there is, the buffer is copied, and the reciever is restarted.
 *
 * Caveats:
 *
 * The Etherboot framework moves the code to the 48k segment from
 * 0x94000 to 0xa0000. There is just a little room between the end of
 * this driver and the 0xa0000 address. If you compile in too many
 * features, this will overflow.
 * The number under "hex" in the output of size that scrolls by while
 * compiling should be less than 8000. Maybe even the stack is up there,
 * so that you need even more headroom.
 */

/* The etherboot authors seem to dislike the argument ordering in
 * outb macros that Linux uses. I disklike the confusion that this
 * has caused even more.... This file uses the Linux argument ordering.  */
/* Sorry not us. It's inherited code from FreeBSD. [The authors] */

#include "eth_defs.h"
#include "nic.h"
#include "pio.h"
#include <sys/pci.h>
#include <time.h>
#include <string.h>

// #define DEBUG_EEPRO100

void hd (void *where, int n);

static int ioaddr;

typedef unsigned char  u8;
typedef   signed char  s8;
typedef unsigned short u16;
typedef   signed short s16;
typedef unsigned int   u32;
typedef   signed int   s32;

enum speedo_offsets {
  SCBStatus = 0, SCBCmd = 2,      /* Rx/Command Unit command and status. */
  SCBPointer = 4,                 /* General purpose pointer. */
  SCBPort = 8,                    /* Misc. commands and operands.  */
  SCBflash = 12, SCBeeprom = 14,  /* EEPROM and flash memory control. */
  SCBCtrlMDI = 16,                /* MDI interface control. */
  SCBEarlyRx = 20,                /* Early receive byte count. */
};


enum SCBCmdBits {
	SCBMaskCmdDone=0x8000, SCBMaskRxDone=0x4000, SCBMaskCmdIdle=0x2000,
	SCBMaskRxSuspend=0x1000, SCBMaskEarlyRx=0x0800, SCBMaskFlowCtl=0x0400,
	SCBTriggerIntr=0x0200, SCBMaskAll=0x0100,
	/* The rest are Rx and Tx commands. */
	CUStart=0x0010, CUResume=0x0020, CUStatsAddr=0x0040,
	CUShowStats=0x0050,
	CUCmdBase=0x0060,	/* CU Base address (set to zero) . */
	CUDumpStats=0x0070, /* Dump then reset stats counters. */
	RxStart=0x0001, RxResume=0x0002, RxAbort=0x0004, RxAddrLoad=0x0006,
	RxResumeNoResources=0x0007,
};

static struct pci_id eepro100_nics[] = {
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_82557,
		"Intel EtherExpressPro100" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_82559ER,
		"Intel EtherExpressPro100 82559ER" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_ID1029,
		"Intel EtherExpressPro100 ID1029" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_ID1030,
		"Intel Corporation 82559 InBusiness 10/100" },
        { PCI_VENDOR_ID_INTEL,          0x1050,
                "Intel PRO100 VE 82562EZ" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_ID1031,
		"Intel 82801CAM Chipset Ethernet Controller" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_ID1039,
		"Intel Corporation 82559 InBusiness 10/100" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_ID103A,
		"Intel Corporation 82559 InBusiness 10/100" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_82562,
		"Intel EtherExpressPro100 82562EM" },
	{ PCI_VENDOR_ID_INTEL,		PCI_DEVICE_ID_INTEL_ID1038,
		"Intel(R) PRO/100 VM Network Connection" },
	{ PCI_VENDOR_ID_INTEL,		0x1039,
		"Intel PRO100 VE 82562ET" },
};


//volatile unsigned short protocol_filter=RT_EP_PROTOCOL;


static int do_eeprom_cmd(int cmd, int cmd_len);
void hd(void *where, int n);
/***********************************************************************/
/*                       I82557 related defines                        */
/***********************************************************************/

// Linux compatibility
#define virt_to_bus(x) x

/* Serial EEPROM section.
   A "bit" grungy, but we work our way through bit-by-bit :->. */
/*  EEPROM_Ctrl bits. */
#define EE_SHIFT_CLK    0x01    /* EEPROM shift clock. */
#define EE_CS           0x02    /* EEPROM chip select. */
#define EE_DATA_WRITE   0x04    /* EEPROM chip data in. */
#define EE_DATA_READ    0x08    /* EEPROM chip data out. */
#define EE_WRITE_0      0x4802
#define EE_WRITE_1      0x4806
#define EE_ENB          (0x4800 | EE_CS)

/* The EEPROM commands include the alway-set leading bit. */
#define EE_READ_CMD     6

/* The SCB accepts the following controls for the Tx and Rx units: */
#define  CU_START       0x0010
#define  CU_RESUME      0x0020
#define  CU_STATSADDR   0x0040
#define  CU_SHOWSTATS   0x0050  /* Dump statistics counters. */
#define  CU_CMD_BASE    0x0060  /* Base address to add to add CU commands. */
#define  CU_DUMPSTATS   0x0070  /* Dump then reset stats counters. */

#define  RX_START       0x0001
#define  RX_RESUME      0x0002
#define  RX_ABORT       0x0004
#define  RX_ADDR_LOAD   0x0006
#define  RX_RESUMENR    0x0007
#define INT_MASK        0x0100
#define DRVR_INT        0x0200          /* Driver generated interrupt. */
#define INT_MASK_NONE   0x0000

enum phy_chips { NonSuchPhy=0, I82553AB, I82553C, I82503, DP83840, S80C240,
                                         S80C24, PhyUndefined, DP83840A=10, };

/* Commands that can be put in a command list entry. */
enum commands {
  CmdNOp = 0,
  CmdIASetup = 1,
  CmdConfigure = 2,
  CmdMulticastList = 3,
  CmdTx = 4,
  CmdTDR = 5,
  CmdDump = 6,
  CmdDiagnose = 7,

  /* And some extra flags: */
  CmdSuspend = 0x4000,      /* Suspend after completion. */
  CmdIntr = 0x2000,         /* Interrupt after completion. */
  CmdTxFlex = 0x0008,       /* Use "Flexible mode" for CmdTx command. */
};

/* How to wait for the command unit to accept a command.
   Typically this takes 0 ticks. */
static inline void wait_for_cmd_done(int cmd_ioaddr)
{

  int wait = 0;
  int delayed_cmd;

  do
    if (inb_p(cmd_ioaddr) == 0) return;
  while(++wait <= 100);
  delayed_cmd = inb_p(cmd_ioaddr);
  do
    if (inb_p(cmd_ioaddr) == 0) break;
  while(++wait <= 1000);
  printf("Command %2.2x was not immediately accepted, %d ticks!\n",
      delayed_cmd, wait);

  /* MIRAR!!!!
  short wait = 100;
  do   ;
  while(inb(cmd_ioaddr) && --wait >= 0);*/
}

/* Elements of the dump_statistics block. This block must be lword aligned. */
static struct speedo_stats {
        u32 tx_good_frames;
        u32 tx_coll16_errs;
        u32 tx_late_colls;
        u32 tx_underruns;
        u32 tx_lost_carrier;
        u32 tx_deferred;
        u32 tx_one_colls;
        u32 tx_multi_colls;
        u32 tx_total_colls;
        u32 rx_good_frames;
        u32 rx_crc_errs;
        u32 rx_align_errs;
        u32 rx_resource_errs;
        u32 rx_overrun_errs;
        u32 rx_colls_errs;
        u32 rx_runt_errs;
        u32 done_marker;
} lstats;

/* A speedo3 TX buffer descriptor with two buffers... */
static struct TxFD {
	volatile s16 status;
	s16 command;
	u32 link;          /* void * */
	u32 tx_desc_addr;  /* (almost) Always points to the tx_buf_addr element. */
	s32 count;         /* # of TBD (=2), Tx start thresh., etc. */
	/* This constitutes two "TBD" entries: hdr and data */
	u32 tx_buf_addr0;  /* void *, header of frame to be transmitted.  */
	s32 tx_buf_size0;  /* Length of Tx hdr. */
	u32 tx_buf_addr1;  /* void *, data to be transmitted.  */
	s32 tx_buf_size1;  /* Length of Tx data. */
} txfd;

struct RxFD {               /* Receive frame descriptor. */
	volatile s16 status;
	s16 command;
	u32 link;                 /* struct RxFD * */
	u32 rx_buf_addr;          /* void * */
	u16 count;
	u16 size;
	char packet[1518];
};

#ifdef	USE_LOWMEM_BUFFER
#define rxfd ((struct RxFD *)(0x10000 - sizeof(struct RxFD)))
#define ACCESS(x) x->
#else
static struct RxFD rxfd;
#define ACCESS(x) x.
#endif

static int congenb = 0;         /* Enable congestion control in the DP83840. */
static int txfifo = 8;          /* Tx FIFO threshold in 4 byte units, 0-15 */
static int rxfifo = 8;          /* Rx FIFO threshold, default 32 bytes. */
static int txdmacount = 0;      /* Tx DMA burst length, 0-127, default 0. */
static int rxdmacount = 0;      /* Rx DMA length, 0 means no preemption. */

/* I don't understand a byte in this structure. It was copied from the
 * Linux kernel initialization for the eepro100. -- REW */
static struct ConfCmd {
  s16 status;
  s16 command;
  u32 link;
  unsigned char data[22];
} confcmd = {
  0, CmdConfigure,
  (u32) & txfd,
  {22, 0x08, 0, 0,  0, 0x80, 0x32, 0x03,  1, /* 1=Use MII  0=Use AUI */
   0, 0x2E, 0,  0x60, 0,
   0xf2, 0x48,   0, 0x40, 0xf2, 0x80,        /* 0x40=Force full-duplex */
   0x3f, 0x05, }
};

/***********************************************************************/
/*                       Locally used functions                        */
/***********************************************************************/

/* Support function: mdio_write
 *
 * This probably writes to the "physical media interface chip".
 * -- REW
 */

static int mdio_write(int phy_id, int location, int value)
{
  struct timespec sleep_time_us = { 0, 16e3};
  int val, boguscnt = 64*4;   /* <64 usec. to complete, typ 27 ticks */

  outl(0x04000000 | (location<<16) | (phy_id<<21) | value,
       ioaddr + SCBCtrlMDI);
  do {
    //udelay(16)
    nanosleep(&sleep_time_us,NULL);

    val = inl(ioaddr + SCBCtrlMDI);
    if (--boguscnt < 0) {
      printf(" mdio_write() timed out with val = %X.\n", val);
    }
  } while (! (val & 0x10000000));
  return val & 0xffff;
}

/* Support function: mdio_read
 *
 * This probably reads a register in the "physical media interface chip".
 * -- REW
 */
static int mdio_read(int phy_id, int location)
{
  struct timespec sleep_time_us = { 0, 16e3};
  int val, boguscnt = 64*4;        /* <64 usec. to complete, typ 27 ticks */
  outl(0x08000000 | (location<<16) | (phy_id<<21), ioaddr + SCBCtrlMDI);
  do {
    //udelay(16);
    nanosleep(&sleep_time_us,NULL);
    val = inl(ioaddr + SCBCtrlMDI);
    if (--boguscnt < 0) {
      printf( " mdio_read() timed out with val = %X.\n", val);
    }
  } while (! (val & 0x10000000));
  return val & 0xffff;
}

/* The fixes for the code were kindly provided by Dragan Stancevic
   <visitor@valinux.com> to strictly follow Intel specifications of EEPROM
   access timing.
   The publicly available sheet 64486302 (sec. 3.1) specifies 1us access
   interval for serial EEPROM.  However, it looks like that there is an
   additional requirement dictating larger udelay's in the code below.
   2000/05/24  SAW */
static int do_eeprom_cmd(int cmd, int cmd_len)
{
  struct timespec sleep_time_us = { 0, 2e3};
  unsigned retval = 0;
  long ee_addr = ioaddr + SCBeeprom;

  outw(EE_ENB, ee_addr);
  nanosleep(&sleep_time_us,NULL); //udelay(2);
  outw(EE_ENB | EE_SHIFT_CLK, ee_addr);
  nanosleep(&sleep_time_us,NULL); //udelay(2);

  /* Shift the command bits out. */
  do {
    short dataval = (cmd & (1 << cmd_len)) ? EE_WRITE_1 : EE_WRITE_0;
    outw(dataval, ee_addr);
    nanosleep(&sleep_time_us,NULL);//udelay(2);
    outw(dataval | EE_SHIFT_CLK, ee_addr);
    nanosleep(&sleep_time_us,NULL); //udelay(2);
    retval = (retval << 1) | ((inw(ee_addr) & EE_DATA_READ) ? 1 : 0);
  } while (--cmd_len >= 0);
  outw(EE_ENB, ee_addr);
  nanosleep(&sleep_time_us,NULL); //udelay(2);

  /* Terminate the EEPROM access. */
  outw(EE_ENB & ~EE_CS, ee_addr);
  return retval;
}

#if 0
static inline void whereami (const char *str)
{
  printf ("%s\n", str);
  sleep (2);
}
#else
#define whereami(s)
#endif

/* function: eepro100_transmit
 * This transmits a packet.
 *
 * Arguments: char d[6]:          destination ethernet address.
 *            unsigned short t:   ethernet protocol type.
 *            unsigned short s:   size of the data-part of the packet.
 *            char *p:            the data for the packet.
 * returns:   void.
 */
// #define DEBUG_EEPRO100
static void eepro100_transmit(struct nic *nic, const unsigned char *d,
                              unsigned int t, unsigned int s, const unsigned char *p)
{

  //struct timespec tpstart,tprunning;
  struct eth_hdr {
    unsigned char dst_addr[ETH_ALEN];
    unsigned char src_addr[ETH_ALEN];
    unsigned short type;
  } hdr;
  unsigned short status;
  int s1, i, s2;

  /* Disable interrupts. */
  //outw_p(SCBMaskAll, ioaddr + SCBCmd);
  status = inw_p(ioaddr + SCBStatus);
  /* Acknowledge all of the current interrupt sources ASAP. */
  //  outw(status & 0xfc00, ioaddr + SCBStatus);
  outw(status & 0xff00, ioaddr + SCBStatus);
#ifdef	DEBUG_EEPRO100
  printf ("transmitting type %hX packet (%d bytes). "
	  "status = %hX, cmd=%hX\n",
	  t, s, status, inw (ioaddr + SCBCmd));
#endif

  memcpy (&hdr.dst_addr, d, ETH_ALEN);
  memcpy (&hdr.src_addr, nic->node_addr, ETH_ALEN);

  //hdr.type = htons (t);
  hdr.type = t;
  txfd.status = 0;
  txfd.command = CmdSuspend | CmdTx | CmdTxFlex;
  txfd.link   =  (u32)&txfd;
  //	txfd.link   = virt_to_bus (&txfd);
  txfd.count   = 0x02208000;
  //	txfd.tx_desc_addr = virt_to_bus(&txfd.tx_buf_addr0);
  txfd.tx_desc_addr =  (u32)&txfd.tx_buf_addr0;
  //	txfd.tx_buf_addr0 = virt_to_bus (&hdr);
  txfd.tx_buf_addr0 = (u32)&hdr;
  txfd.tx_buf_size0 = sizeof (hdr);

  //	txfd.tx_buf_addr1 = virt_to_bus (p);
  txfd.tx_buf_addr1 = (u32)p;
  txfd.tx_buf_size1 = s;

#ifdef	DEBUG_EEPRO100
  printf ("txfd: \n");
  hd (&txfd, sizeof (txfd));
#endif

  //	outl(virt_to_bus(&txfd), ioaddr + SCBPointer);
  outl((u32)&txfd, ioaddr + SCBPointer);

  outw(INT_MASK | CU_START, ioaddr + SCBCmd);

  wait_for_cmd_done(ioaddr + SCBCmd);
  s1 = inw_p (ioaddr + SCBStatus);

  for (i = 1000; i > 0; i--)   if(txfd.status)  break;

  s2 = inw_p (ioaddr + SCBStatus);
  // This unmasks the interrupts...
  // We are not ACK-ing FCP and ER in the interrupt
  // handler yet so they should  remain masked --Dragan
  //  outw(CUStart | SCBMaskEarlyRx | SCBMaskFlowCtl, ioaddr + SCBCmd);
  //outw(CUStart | SCBMaskEarlyRx | SCBMaskFlowCtl, ioaddr + SCBCmd);
  outw(INT_MASK_NONE, ioaddr + SCBCmd);

#ifdef	DEBUG_EEPRO100
  printf ("s1 = %hX, s2 = %hX.\n", s1, s2);
#endif
}

/*
 * Sometimes the receiver stops making progress.  This routine knows how to
 * get it going again, without losing packets or being otherwise nasty like
 * a chip reset would be.  Previously the driver had a whole sequence
 * of if RxSuspended, if it's no buffers do one thing, if it's no resources,
 * do another, etc.  But those things don't really matter.  Separate logic
 * in the ISR provides for allocating buffers--the other half of operation
 * is just making sure the receiver is active.  speedo_rx_soft_reset does that.
 * This problem with the old, more involved algorithm is shown up under
 * ping floods on the order of 60K packets/second on a 100Mbps fdx network.
 */

static void
speedo_rx_soft_reset(void)
{
  wait_for_cmd_done(ioaddr + SCBCmd);
  /*
   * Put the hardware into a known state.
   */
  outb(RX_ABORT, ioaddr + SCBCmd);

  ACCESS(rxfd)rx_buf_addr = 0xffffffff;

  wait_for_cmd_done(ioaddr + SCBCmd);

  outb(RX_START, ioaddr + SCBCmd);
}


//#undef DEBUG_EEPRO100
/* function: eepro100_poll / eth_poll
 * This recieves a packet from the network.
 *
 * Arguments: none
 *
 * returns:   1 if a packet was recieved.
 *            0 if no pacet was recieved.
 * side effects:
 *            returns the packet in the array nic->packet.
 *            returns the length of the packet in nic->packetlen.
 */

static int eepro100_poll(struct nic *nic)
{
  unsigned int status;
  status = inw(ioaddr + SCBStatus);
  if (!ACCESS(rxfd)status)
    return 0;

  /*
   * The chip may have suspended reception for various reasons.
   * Check for that, and re-prime it should this be the case.
   */
  switch ((status >> 2) & 0xf) {
  case 0: /* Idle */
    break;
  case 1:	/* Suspended */
  case 2:	/* No resources (RxFDs) */
  case 9:	/* Suspended with no more RBDs */
  case 10: /* No resources due to no RBDs */
  case 12: /* Ready with no RBDs */
    speedo_rx_soft_reset();
    break;
  case 3:  case 5:  case 6:  case 7:  case 8:
  case 11:  case 13:  case 14:  case 15:
    /* these are all reserved values */
    break;
  }


  /* Ok. We got a packet. Now restart the reciever.... */
  ACCESS(rxfd)status = 0;
  ACCESS(rxfd)command = 0xc000;

  //	outl(virt_to_bus(&(ACCESS(rxfd)status)), ioaddr + SCBPointer);
  outl(&(ACCESS(rxfd)status), ioaddr + SCBPointer);
  outw(RX_START, ioaddr + SCBCmd);
  wait_for_cmd_done(ioaddr + SCBCmd);

#ifdef	DEBUG_EEPRO100
  printf ("Got a packet: Len = %d.\n", ACCESS(rxfd)count & 0x3fff);
#endif
  nic->packetlen =  ACCESS(rxfd)count & 0x3fff;
  memcpy (nic->packet, ACCESS(rxfd)packet, nic->packetlen);
  //#ifdef	DEBUG_EEPRO100
  //	hd (nic->packet, 00x30);
  //#endif
  // This unmasks the interrupts...
  // We are not ACK-ing FCP and ER in the interrupt
  // handler yet so they should  remain masked --Dragan
  outw(CUStart | SCBMaskEarlyRx | SCBMaskFlowCtl, ioaddr + SCBCmd);
  return 1;
}




/* function: eepro100_disable
 * resets the card. This is used to allow Etherboot or Linux
 * to probe the card again from a "virginal" state....
 * Arguments: none
 *
 * returns:   void.
 */
static void eepro100_disable(struct dev *dev __unused)
{
  /* from eepro100_reset */
  outl(0, ioaddr + SCBPort);
  /* from eepro100_disable */
  /* See if this PartialReset solves the problem with interfering with
     kernel operation after Etherboot hands over. - Ken 20001102 */
  outl(2, ioaddr + SCBPort);
}
static int eepro100_handler(unsigned short protocol_filter){

  unsigned short status;
  u16 protocol;

  // printf("entro handler, filter %d\n", protocol_filter);

  status = inw(ioaddr + SCBStatus);
  /* Acknowledge all of the current interrupt sources ASAP. */
  outw(status & 0xff00, ioaddr + SCBStatus);

  //break out of loop if done // if ((status & 0xfc00) == 0){
  if ((status & 0xff00) == 0){
    // printf("agur status\n");
    return 0;
  }
  if (status & 0x5000){
    // printf("valid packet\n");

    /*
     * The chip may have suspended reception for various reasons.
     * Check for that, and re-prime it should this be the case.
     */
    switch ((status >> 2) & 0xf) {
    case 0: /* Idle */
      break;
    case 1:	/* Suspended */
    case 2:	/* No resources (RxFDs) */
    case 9:	/* Suspended with no more RBDs */
    case 10: /* No resources due to no RBDs */
    case 12: /* Ready with no RBDs */
      speedo_rx_soft_reset();
      break;
    case 3:  case 5:  case 6:  case 7:  case 8:
    case 11:  case 13:  case 14:  case 15:
      /* these are all reserved values */
      break;
    }

    if(protocol_filter==0) {
      return 1;
    }

    /*We have received a frame.*/
    protocol = ntohs( *(u16 *) (ACCESS(rxfd)packet + 2 * ETH_ALEN) );
    // printf("proto: %X, filter %X\n",protocol, protocol_filter);
    /* We check if it is a valid protocol Frame */
    if( protocol == protocol_filter){
      //printc("Protocol is RT-EP: %x", protocol);
      return 1;
    } else {
      /*We have received a non-protocol-match frame and we discard it.*/
      //Restart the receiver, like in poll
      //printf("discard frame\n");
      //printf("Protocol is: %x", protocol);
      ACCESS(rxfd)status = 0;
      ACCESS(rxfd)command = 0xc000;
      //ensure not in power down mode.
      outl(&(ACCESS(rxfd)status), ioaddr + SCBPointer);
      outw(RX_START, ioaddr + SCBCmd);
      wait_for_cmd_done(ioaddr + SCBCmd);
      //we may have to take data from rxfd......
      return 0;
    } //end if else
  }
  return 0;
}


static int eepro100_open(struct nic *nic){

  u16 status;

  /* Disable interrupts. */
  outw(SCBMaskAll, ioaddr + SCBCmd);

  status = inw(ioaddr + SCBStatus);
  /* Acknowledge all of the current interrupt sources ASAP. */
  /* Will change from 0xfc00 to 0xff00 when we start handling
     FCP and ER interrupts --Dragan */
  // QUITADO.  outw(status & 0xfc00, ioaddr + SCBStatus);
  outw(status & 0xff00, ioaddr + SCBStatus);

  // This unmasks the interrupts...
  // We are not ACK-ing FCP and ER in the interrupt
  // handler yet so they should  remain masked --Dragan
  outw(CUStart | SCBMaskEarlyRx | SCBMaskFlowCtl, ioaddr + SCBCmd);
  return 0;
}


/* exported function: eepro100_probe / eth_probe
 * initializes a card
 *
 * side effects:
 *            leaves the ioaddress of the 82557 chip in the variable ioaddr.
 *            leaves the 82557 initialized, and ready to recieve packets.
 */

int eepro100_probe(struct dev *dev)
{
  struct timespec sleep_time_us = { 0, 10000e3}, tpstart, tprunning;

  struct nic *nic = (struct nic *)dev;
  unsigned short sum = 0;
  int i;
  int read_cmd, ee_size;
  int options;
  int rx_mode,ret;

  struct pci_device pci_dev,*p;
  /* we cache only the first few words of the EEPROM data
     be careful not to access beyond this array */
  unsigned short eeprom[16];

  for (i=0; i< sizeof(eepro100_nics)/sizeof(eepro100_nics[0]); i++){
    ret=pci_find_device(eepro100_nics[i].vendor,
			eepro100_nics[i].dev_id, NULL, &pci_dev);
    if (ret==0){
      pci_dev.name=eepro100_nics[i].name;
      break;
    }
  } // end for

  if(ret==-1){
    return 0;
  }
  /* There are enough "EEPRO 100" strings on the console already, so
   * be brief and concentrate on the interesting pieces of info... */
  printf("EEPRO100\n\t");

  p=&pci_dev;


  if (p->ioaddr == 0)
    return 0;
  /* Mask the bit that says "this is an io addr" */
  ioaddr = p->ioaddr & ~3;

  adjust_pci_device(p);

  if ((do_eeprom_cmd(EE_READ_CMD << 24, 27) & 0xffe0000)
      == 0xffe0000) {
    ee_size = 0x100;
    read_cmd = EE_READ_CMD << 24;
  } else {
    ee_size = 0x40;
    read_cmd = EE_READ_CMD << 22;
  }

  for (i = 0, sum = 0; i < ee_size; i++) {
    unsigned short value = do_eeprom_cmd(read_cmd | (i << 16), 27);
    if (i < (int)(sizeof(eeprom)/sizeof(eeprom[0])))
      eeprom[i] = value;
    sum += value;
  }

  for (i=0;i<ETH_ALEN;i++) {
    nic->node_addr[i] =  (eeprom[i/2] >> (8*(i&1))) & 0xff;
  }
  printf ("Ethernet addr: %! ", nic->node_addr);

  if (sum != 0xBABA)
    printf("eepro100: Invalid EEPROM checksum %#hX, "
	   "check settings before activating this device!\n", sum);
  outl(0, ioaddr + SCBPort);
  //udelay (10000);
  nanosleep(&sleep_time_us,NULL);

  whereami ("Got eeprom.");

  outl(virt_to_bus(&lstats), ioaddr + SCBPointer);
  outw(INT_MASK | CU_STATSADDR, ioaddr + SCBCmd);
  wait_for_cmd_done(ioaddr + SCBCmd);

  whereami ("set stats addr.");
  /* INIT RX stuff. */

  /* Base = 0 */
  outl(0, ioaddr + SCBPointer);
  outw(INT_MASK | RX_ADDR_LOAD, ioaddr + SCBCmd);
  wait_for_cmd_done(ioaddr + SCBCmd);

  whereami ("set rx base addr.");

  ACCESS(rxfd)status  = 0x0001;
  ACCESS(rxfd)command = 0x0000;
  ACCESS(rxfd)link    = (u32) virt_to_bus(&(ACCESS(rxfd)status));
  ACCESS(rxfd)rx_buf_addr = (int) &nic->packet;
  ACCESS(rxfd)count   = 0;
  ACCESS(rxfd)size    = 1528;

  outl(virt_to_bus(&(ACCESS(rxfd)status)), ioaddr + SCBPointer);
  outw(INT_MASK | RX_START, ioaddr + SCBCmd);
  wait_for_cmd_done(ioaddr + SCBCmd);

  whereami ("started RX process.");

  /* Start the reciever.... */
  ACCESS(rxfd)status = 0;
  ACCESS(rxfd)command = 0xc000;
  outl(virt_to_bus(&(ACCESS(rxfd)status)), ioaddr + SCBPointer);
  outw(INT_MASK | RX_START, ioaddr + SCBCmd);

  /* INIT TX stuff. */

  /* Base = 0 */
  outl(0, ioaddr + SCBPointer);
  outw(INT_MASK | CU_CMD_BASE, ioaddr + SCBCmd);
  wait_for_cmd_done(ioaddr + SCBCmd);

  whereami ("set TX base addr.");

  txfd.command      = (CmdIASetup);
  txfd.status       = 0x0000;
  txfd.link         = (u32) virt_to_bus (&confcmd);

  {
    char *t = (char *)&txfd.tx_desc_addr;

    for (i=0;i<ETH_ALEN;i++)
      t[i] = nic->node_addr[i];
  }

#ifdef	DEBUG_EEPRO100
  printf ("Setup_eaddr:\n");
  hd (&txfd, 0x20);
#endif
  /*      options = 0x40; */ /* 10mbps half duplex... */
  options = 0x00;            /* Autosense */


  // By default we enable promiscuous mode.


// #define PROMISC

#ifdef PROMISC
  rx_mode = 3;
#elif ALLMULTI
  rx_mode = 1;
#else
  rx_mode = 0;
#endif

  if (   ((eeprom[6]>>8) & 0x3f) == DP83840
	 || ((eeprom[6]>>8) & 0x3f) == DP83840A) {
    int mdi_reg23 = mdio_read(eeprom[6] & 0x1f, 23) | 0x0422;
    if (congenb)
      mdi_reg23 |= 0x0100;
    printf("  DP83840 specific setup, setting register 23 "
	   "to %hX.\n", mdi_reg23);
    mdio_write(eeprom[6] & 0x1f, 23, mdi_reg23);
  }
  whereami ("Done DP8340 special setup.");
  if (options != 0) {
    mdio_write(eeprom[6] & 0x1f, 0,
	       ((options & 0x20) ? 0x2000 : 0) |    /* 100mbps? */
	       ((options & 0x10) ? 0x0100 : 0)); /* Full duplex? */
    whereami ("set mdio_register.");
  }

  confcmd.command  = CmdSuspend | CmdConfigure;
  confcmd.status   = 0x0000;
  confcmd.link     = (u32)virt_to_bus (&txfd);
  confcmd.data[1]  = (txfifo << 4) | rxfifo;
  confcmd.data[4]  = rxdmacount;
  confcmd.data[5]  = txdmacount + 0x80;
  //confcmd.data[15] = (rx_mode & 2) ? 0x49: 0x48;
  confcmd.data[15] |= (rx_mode & 2) ? 1 : 0;
  confcmd.data[19] = (options & 0x10) ? 0xC0 : 0x80;
  confcmd.data[21] = (rx_mode & 1) ? 0x0D: 0x05;

  outl(virt_to_bus(&txfd), ioaddr + SCBPointer);
  outw(INT_MASK | CU_START, ioaddr + SCBCmd);
  wait_for_cmd_done(ioaddr + SCBCmd);

  whereami ("started TX thingy (config, iasetup).");

  clock_gettime (_MARTE_CLOCK_REALTIME, &tpstart);
  tprunning.tv_sec=tpstart.tv_sec;
  tprunning.tv_nsec=tpstart.tv_nsec;
  while (!txfd.status && tpstart.tv_nsec +10e3 > tprunning.tv_nsec)
    clock_gettime(_MARTE_CLOCK_REALTIME, &tprunning) /* Wait */;

  //	load_timer2(10*TICKS_PER_MS);
  //while (!txfd.status && timer2_running())
  //	/* Wait */;



  /* Read the status register once to disgard stale data */
  mdio_read(eeprom[6] & 0x1f, 1);
  /* Check to see if the network cable is plugged in.
   * This allows for faster failure if there is nothing
   * we can do.
   */
  if (!(mdio_read(eeprom[6] & 0x1f, 1) & (1 << 2))) {
    printf("Valid link not established\n");
    printf("Check if ethernet cable is connected\n");
    eepro100_disable(dev);
    return 0;
  }

  dev->disable  = eepro100_disable;
  nic->poll     = eepro100_poll;
  nic->transmit = eepro100_transmit;

  nic->handler  = eepro100_handler;
  nic->open     = eepro100_open;
  nic->irq      = p->irq;

  return 1;
}





/*********************************************************************/
// #define DEBUG_EEPRO100
#ifdef	DEBUG_EEPRO100

/* Hexdump a number of bytes from memory... */
void hd (void *where, int n)
{
  int i;

  while (n > 0) {
    printf ("%X ", where);
    for (i=0;i < ( (n>16)?16:n);i++)
      printf (" %hhX", ((char *)where)[i]);
    printf ("\n");
    n -= 16;
    where += 16;
  }
}
#endif



/*static struct pci_driver eepro100_driver __pci_driver = {
  .type      = NIC_DRIVER,
  .name      = "EEPRO100",
  .probe     = eepro100_probe,
  .ids       = eepro100_nics,
  .id_count  = sizeof(eepro100_nics)/sizeof(eepro100_nics[0]),
  .class     = 0
  };*/
