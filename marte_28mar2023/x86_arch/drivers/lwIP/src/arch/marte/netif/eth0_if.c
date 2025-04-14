
/*
 * Copyright (c) 2001, Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the Institute nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE INSTITUTE OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

/*********************************************************************************/
/* This file has been modified by 						 */
/* ByungGi Baek <gi@realtimewave.com||weapon100@empal.com> for eth0 RTlinux   */
/* driver  				                                                         */

/* This file has been modified by 						 */
/* Hector Perez <perezh@unican.es> for MaRTE OS          */
/* driver  				                                                         */

/* This file is based in one file part of the lwIP TCP/IP stack. The file is:    */
/* ethernetif.c                                                                  */
/* which author is: Adam Dunkels <adam@sics.se>                                  */
/*										 */
/* And partly based in rt_3c905x.c by Sergio Perez Alcañiz <serpeal@upvnet.upv.es>*/
/*                                                                               */
/*                                                                               */
/* The RTL-lwIP project has been supported by the Spanish Government Research    */
/* Office (CICYT) under grant TIC2002-04123-C03-03                               */
/*                                                                               */
/* Copyright (c) March, 2003 SISTEMAS DE TIEMPO REAL EMPOTRADOS, FIABLES Y       */
/* DISTRIBUIDOS BASADOS EN COMPONENTES                                           */
/*                                                                               */
/*  This program is free software; you can redistribute it and/or modify         */
/*  it under the terms of the GNU General Public License as published by         */
/*  the Free Software Foundation; either version 2 of the License, or            */
/*  (at your option) any later version.                                          */
/*                                                                               */
/*  This program is distributed in the hope that it will be useful,              */
/*  but WITHOUT ANY WARRANTY; without even the implied warrabnty of              */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                */
/*  GNU General Public License for more details.                                 */
/*                                                                               */
/*  You should have received a copy of the GNU General Public License            */
/*  along with this program; if not, write to the Free Software                  */
/*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA    */
/*                                                                               */
/*  Linking RTL-lwIP statically or dynamically with other modules is making a    */
/*  combined work based on RTL-lwIP.  Thus, the terms and conditions of the GNU  */
/*  General Public License cover the whole combination.                          */
/*                                                                               */
/*  As a special exception, the copyright holders of RTL-lwIP give you           */
/*  permission to link RTL-lwIP with independent modules that communicate with   */
/*  RTL-lwIP solely through the interfaces, regardless of the license terms of   */
/*  these independent modules, and to copy and distribute the resulting combined */
/*  work under terms of your choice, provided that every copy of the combined    */
/*  work is accompanied by a complete copy of the source code of RTL-lwIP (the   */
/*  version of RTL-lwIP used to produce the combined work), being distributed    */
/*  under the terms of the GNU General Public License plus this exception.  An   */
/*  independent module is a module which is not derived from or based on         */
/*  RTL-lwIP.                                                                    */
/*                                                                               */
/*  Note that people who make modified versions of RTL-lwIP are not obligated to */
/*  grant this special exception for their modified versions; it is their choice */
/*  whether to do so.  The GNU General Public License gives permission to        */
/*  release a modified version without this exception; this exception also makes */
/*  it possible to release a modified version which carries forward this         */
/*  exception.                                                                   */
/*********************************************************************************/


#include "lwip/opt.h"
#include "lwip/def.h"
#include "lwip/mem.h"
#include "lwip/pbuf.h"
#include <lwip/stats.h>
#include <lwip/snmp.h>
#include "netif/etharp.h"
#include "netif/ppp_oe.h"

#include "lwip/sys.h"
#include "lwip/timers.h"
#include "netif/eth0_if.h"
#include "netif/ethernetif.h"
#include <unistd.h>
#include <stdio.h>
#include "bcopy.h"
#include <signal.h>

// MaRTE OS
#include <drivers/if_ether.h>
#include <drivers/eth_ioctl.h>
#include <sched.h>
#include <fcntl.h>
#include <pthread.h>

#define IFNAME0 'e'
#define IFNAME1 't'

ethernetif_thread_t eth0_thread;

struct eth0_if {
  struct eth_addr *ethaddr;
};

static const struct eth_addr eth0_if_ethbroadcast = {{0xff,0xff,0xff,0xff,0xff,0xff}};
static int eth0_if_fd;
static struct netif *eth0_if_netif;

/* Forward declarations. */
static void eth0_if_input (struct netif *netif);
static void eth0_if_ethernetif_thread(void *arg);

/*-----------------------------------------------------------------------------------*/
static void
eth0_if_low_level_init(struct netif *netif)
{
  struct eth0_if *eth0_if;
  char dev_name[]=NIC_PATH;
  unsigned int All_Packets = 0x0U;

  eth0_if = netif->state;

  /* Do whatever else is needed to initialize interface. */
  if((eth0_if_fd=open(dev_name,O_RDWR)) == -1)
    printf("ERROR OPENING %s\n", NIC_PATH);

  /* Obtain MAC address from network interface. */
  ioctl(eth0_if_fd, ETH_HWADDR, (unsigned long) eth0_if->ethaddr->addr);

  /* We set an IP filter to the ethernet card */
  ioctl(eth0_if_fd, SET_PROTOCOL_FILTER, &All_Packets); // All the packets on the net
  // IP FILTER NUM?????

  /* set MAC hardware address length */
  netif->hwaddr_len = ETHARP_HWADDR_LEN;

  /* set MAC hardware address */
  netif->hwaddr[0] = eth0_if->ethaddr->addr [0];
  netif->hwaddr[1] = eth0_if->ethaddr->addr [1];
  netif->hwaddr[2] = eth0_if->ethaddr->addr [2];
  netif->hwaddr[3] = eth0_if->ethaddr->addr [3];
  netif->hwaddr[4] = eth0_if->ethaddr->addr [4];
  netif->hwaddr[5] = eth0_if->ethaddr->addr [5];

  /* maximum transfer unit */
  netif->mtu = 1500; //ETH_MAX_MTU;

  /* device capabilities */
  /* don't set NETIF_FLAG_ETHARP if this device is not an ethernet one */
  netif->flags = NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_LINK_UP;

  printf("ADRESS: %x:%x:%x:%x:%x:%x \n",eth0_if->ethaddr->addr[0],eth0_if->ethaddr->addr[1],eth0_if->ethaddr->addr[2],eth0_if->ethaddr->addr[3],eth0_if->ethaddr->addr[4],eth0_if->ethaddr->addr[5]);

  sys_thread_new("decoupling_thread", eth0_if_ethernetif_thread, netif, DEFAULT_THREAD_STACKSIZE, DEFAULT_THREAD_PRIO);

  return ;
}

static void eth0_if_ethernetif_thread(void *arg){
  do{
    sleep(1);
    eth0_if_input(eth0_if_netif);
  }while(1);
}

/*-----------------------------------------------------------------------------------*/
/*
 * eth0_if_low_level_output():
 *
 * This function should do the actual transmission of the packet. The packet is
 * contained in the pbuf that is passed to the function. This pbuf
 * might be chained.
 *
 * @param netif the lwip network interface structure for this ethernetif
 * @param p the MAC packet to send (e.g. IP packet including MAC addresses and type)
 * @return ERR_OK if the packet could be sent
 *         an err_t value if the packet couldn't be sent
 *
 * @note Returning ERR_MEM here if a DMA queue of your MAC is full can lead to
 *       strange results. You might consider waiting for space in the DMA queue
 *       to become availale since the stack doesn't retry to send a packet
 *       dropped because of memory failure (except for the TCP timers).
 */
/*-----------------------------------------------------------------------------------*/

err_t
eth0_if_low_level_output(struct netif *eth0_if, struct pbuf *p)
{

  struct pbuf *q;
  unsigned char buf[ETH_FRAME_LEN];
  unsigned char *bufptr;

  //initiate transfer;
  bufptr = buf;

#if ETH_PAD_SIZE
  pbuf_header(p, -ETH_PAD_SIZE); /* drop the padding word */
#endif

  for(q = p; q != NULL; q = q->next) {
    /* Send the data from the pbuf to the interface, one pbuf at a
       time. The size of the data in each pbuf is kept in the ->len
       variable. */
    bcopy(q->payload, bufptr, q->len);
    bufptr += q->len;
  }

  //signal that packet should be sent;
  {
    int tmp;
    int counter=0;

    while((tmp = write(eth0_if_fd,buf,p->tot_len)) == -1){
      counter++;
      usleep(1);
    }
  }
  //printf("Write %d bytes\n", p->len);

#if ETH_PAD_SIZE
  pbuf_header(p, ETH_PAD_SIZE); /* reclaim the padding word */
#endif

  return ERR_OK;
}
/*-----------------------------------------------------------------------------------*/
/*
 * eth0_if_low_level_input():
 *
 * Should allocate a pbuf and transfer the bytes of the incoming
 * packet from the interface into the pbuf.
 *
 * @param netif the lwip network interface structure for this ethernetif
 * @return a pbuf filled with the received packet (including MAC header)
 *         NULL on memory error
 */
/*-----------------------------------------------------------------------------------*/
static struct pbuf *
eth0_if_low_level_input(struct eth0_if *eth0_if)
{
  struct pbuf *p, *q;
  unsigned char *bufptr;
  char buf[1500];
  u16_t len;
   
  /* Obtain the size of the packet and put it into the "len"
     variable. */
  len = read(eth0_if_fd, buf, sizeof(buf));
  //printf("Read %d bytes\n", len);

#if ETH_PAD_SIZE
  len += ETH_PAD_SIZE; /* allow room for Ethernet padding */
#endif

  /* We allocate a pbuf chain of pbufs from the pool. */
  p = pbuf_alloc(PBUF_LINK, len, PBUF_POOL);

  if(p != NULL) {

#if ETH_PAD_SIZE
    pbuf_header(p, -ETH_PAD_SIZE); /* drop the padding word */
#endif
      bufptr = &buf[0];
    /* We iterate over the pbuf chain until we have read the entire
     * packet into the pbuf. */
    for(q = p; q != NULL; q = q->next) {
      /* Read enough bytes to fill this pbuf in the chain. The
       * available data in the pbuf is given by the q->len
       * variable.
       * This does not necessarily have to be a memcpy, you can also preallocate
       * pbufs for a DMA-enabled MAC and after receiving truncate it to the
       * actually received size. In this case, ensure the tot_len member of the
       * pbuf is the sum of the chained pbuf len members.
       */
      memcpy(q->payload, bufptr, q->len);
      bufptr += q->len;
      //bcopy(bufptr, q->payload, q->len);
      //bufptr += q->len;
    }

#if ETH_PAD_SIZE
    pbuf_header(p, ETH_PAD_SIZE); /* reclaim the padding word */
#endif
    LINK_STATS_INC(link.recv);
  } else {
    printf ("Drop packet not implemented!\n");
    LINK_STATS_INC(link.memerr);
    LINK_STATS_INC(link.drop);
  }
  return p;
}

/*-----------------------------------------------------------------------------------*/
/*
 * eth0_if_input():
 *
 * This function should be called when a packet is ready to be read
 * from the interface. It uses the function eth0_if_low_level_input() that
 * should handle the actual reception of bytes from the network
 * interface. Then the type of the received packet is determined and
 * the appropriate input function is called.
 *
 * @param netif the lwip network interface structure for this ethernetif
 */
static void

eth0_if_input (struct netif *netif)
{

  /* Ethernet protocol layer */
  struct eth0_if *eth0_if = netif->state;
  struct eth_hdr *ethhdr;
  struct pbuf *p = NULL;

  /* move received packet into a new pbuf */
  p = eth0_if_low_level_input(eth0_if);

  /* no packet could be read, silently ignore this */
  if (p == NULL) return;
  /* points to packet payload, which starts with an Ethernet header */
  ethhdr = p->payload;
  switch(htons(ethhdr->type)) {
  /* IP or ARP packet? */
  case ETHTYPE_IP:
  case ETHTYPE_ARP:
#if PPPOE_SUPPORT
  /* PPPoE packet? */
  case ETHTYPE_PPPOEDISC:
  case ETHTYPE_PPPOE:
#endif /* PPPOE_SUPPORT */
    /* full packet send to tcpip_thread to process */
    if (netif->input(p, netif)!=ERR_OK)
     { LWIP_DEBUGF(NETIF_DEBUG, ("ethernetif_input: IP input error\n"));
       pbuf_free(p);
       p = NULL;
     }
    break;

  default:
    pbuf_free(p);
    p = NULL;
    break;
  }
}

/*-----------------------------------------------------------------------------------*/
void rt_eth0_ifetharp_timer(int signo)
{
  etharp_tmr();
  sys_timeout(ARP_TMR_INTERVAL, (sys_timeout_handler) rt_eth0_ifetharp_timer, NULL);
}

/*-----------------------------------------------------------------------------------*/
/*
 * eth0_if_init():
 *
 * Should be called at the beginning of the program to set up the
 * network interface. It calls the function eth0_if_low_level_init() to do the
 * actual setup of the hardware.
 *
 * This function should be passed as a parameter to netif_add().
 *
 * @param netif the lwip network interface structure for this ethernetif
 * @return ERR_OK if the loopif is initialized
 *         ERR_MEM if private data couldn't be allocated
 *         any other err_t on error
 */
err_t
eth0_if_init(struct netif *netif)
{
  struct eth0_if *eth0_if;

  LWIP_ASSERT("netif != NULL", (netif != NULL));

  eth0_if = mem_malloc(sizeof(struct eth0_if));
  if (eth0_if == NULL) {
    LWIP_DEBUGF(NETIF_DEBUG, ("ethernetif_init: out of memory\n"));
    return ERR_MEM;
  }

#if LWIP_NETIF_HOSTNAME
  /* Initialize interface hostname */
  netif->hostname = "lwip";
#endif /* LWIP_NETIF_HOSTNAME */

  /*
   * Initialize the snmp variables and counters inside the struct netif.
   * The last argument should be replaced with your link speed, in units
   * of bits per second.
   */
  //NETIF_INIT_SNMP(netif, snmp_ifType_ethernet_csmacd, LINK_SPEED_OF_YOUR_NETIF_IN_BPS);

  netif->state = eth0_if;
  netif->name[0] = IFNAME0;
  netif->name[1] = IFNAME1;
  /* We directly use etharp_output() here to save a function call.
   * You can instead declare your own function an call etharp_output()
   * from it if you have to do some checks before sending (e.g. if link
   * is available...) */
  netif->output = etharp_output;
  netif->linkoutput = eth0_if_low_level_output;

  eth0_if->ethaddr = (struct eth_addr *)&(netif->hwaddr[0]);

  eth0_if_netif = netif;
  
  /* initialize the hardware */
  eth0_if_low_level_init(netif);
  //etharp_init();

  //sys_timeout(ARP_TMR_INTERVAL, (sys_timeout_handler) rt_eth0_ifetharp_timer, NULL);

  return ERR_OK;
}


/*-----------------------------------------------------------------------------------*/
void eth0_if_close(void){

  /* Closing the eth0 card */
  close(eth0_if_fd);
}

