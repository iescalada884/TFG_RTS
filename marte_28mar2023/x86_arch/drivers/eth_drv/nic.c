/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                               {ETH_VERSION}
 *
 *                                'n i c . c'
 *
 *                                     C
 *
 *
 * File 'nic.c'                                                       By Chema.
 *                                                          Jose Maria Martinez
 *                                                          <chema@gmx.net>
 * Ethernet basic driver functions
 *
 * {CHEMA_COPYRIGHT}
 *
 *---------------------------------------------------------------------------*/
/*****************************************************************************/
/* This module is based on nic.c from the Etherboot proyect:                 */
/* http://etherboot.sourceforge.net v.5.1.3                                  */


/**************************************************************************
Etherboot -  BOOTP/TFTP Bootstrap Program

Author: Martin Renters
  Date: Dec/93
**************************************************************************/

/*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 */

//#include <stdio.h>
//#include "sis900.h"
#include "eth_defs.h"
#include "nic.h"
#include "rx_queue.h"
#include "dev.h"
#include "ethernet.h"
#include "pio.h"
#include <drivers/eth_ioctl.h>
#include <string.h>
#include <intr.h>
#include <drivers/if_ether.h>

#ifdef TIME_MEASURE
#include <sys/rdtscll.h>
#endif

//extern int sis900_probe(struct dev *dev);

#define ETH_POLL_ERR     -1

// Here we define the receiving protocol filter by default is RT-EP
static volatile unsigned short protocol_filter=RT_EP_PROTOCOL;

#ifdef TIME_MEASURE
static long long overhead_ticks,ttps;
/* Sleep 1 sec. in order to get the ticks per second of the processor.*/
struct timespec sleep_time_state={1,0};
#endif

struct arptable_t	arptable[MAX_ARP];

/* Currently no other module uses rom, but it is available */
struct rom_info		rom;
/* static unsigned long	netmask; */


/* We define here the reception global queues. It is probably not the */
/* proper place to put them but it's not so bad. */
/* rx_ring_queue give us one rx ring queue for each ethernet interface.*/

static ring_queue_t rx_ring_queue;


/*Here we store the number of pending rx paquets. */

volatile int pending_rx_paquets=0;

/* Only one open and one close per process. */
boolean device_opened=FALSE;

static int dummy(void *a)
{
	return (0);
}

static void dummy_handler(void){

  printf("Handler function not installed\n");

}

static unsigned char	packet[ETH_FRAME_LEN];

struct nic	nic =
{
	{
		0,				/* dev.disable */
		{
			DEV_ID_SIZE-1,
			0,
			5,
			PCI_BUS_TYPE,
			0,
			0
		},				/* dev.devid */
		0,				/* index */
		0,				/* type */
		PROBE_FIRST,			/* how_pobe */
		PROBE_NONE,			/* to_probe */
		0,				/* failsafe */
		{},				/* state */
	},
	(int (*)(struct nic *))dummy,		/* poll */
	(void (*)(struct nic *, const unsigned char *,
		unsigned int, unsigned int,
		const unsigned char *))dummy,		/* transmit */
	(void (*))dummy_handler,                /* interrupt handler*/
	(int (*)(struct nic *))dummy,		/* open */
	0,					/* flags */
	&rom,					/* rom_info */
	//	{00,00,00,00,00,00},	        	/* node_addr */
	arptable[ARP_CLIENT].node,
	packet,					/* packet */
	0,					/* packetlen */
	0,					/* priv_data */
};



/* eth_poll : Polls the ethernet interface to get a rx packet               */
/*            and stores it in 'data'. 'data' might point a free reserved   */
/*            space on rx ring queue. I might be used with eth_frame_t type.*/
/*            It also stores in len the data length of data                 */
/*            Returns the number of receibed bytes.                         */
/*            On error returns ETH_POLL_ERR when this come up the returned  */
/*            values from the driver are stored in *data and *len           */
/*            Both *data and *len supposed to point to free reserved area   */
/*            inside the reception (ring) buffer.                           */
/*            This function is supposed to be used inside the interrupt     */
/*            handler.                                                      */
/* This define will set tx and Rx debug info on.*/
// #define DEBUG_ETH
/* int eth_poll(char *data) */
int eth_poll(unsigned char *data)
{
  int ret;
  /* With this, the driver writes directly into the buffer*/
  nic.packet=data;
  ret=(*nic.poll)(&nic);
#ifdef DEBUG_ETH
  printc("\neth_poll : Dest_Addr: %X:%X:%X:%X:%X:%X - "
	 "Sour_Addr: %X:%X:%X:%X:%X:%X - \n"
	 " Proto: %X_%X - Info: %X:%X:%X ... ",
	 data[0], data[1], data[2], data[3], data[4], data[5], data[6],
	 data[7], data[8], data[9], data[10], data[11], data[12], data[13],
	 data[14], data[15], data[16]);
#endif

  if (ret)
    return nic.packetlen;
  return ret;
}


/* eth_transmit : Transmits a packet and waits for completion or timeout.   */
/*                Arguments:                                                */
/*                           dest_addr:    The destination ethernet address.*/
/*                           eth_protocol: Ethernet protocol type.          */
/*                           data_len:     The size of the Data             */
/*                           data:         The data for the ethernet packet.*/

void eth_transmit( const unsigned char *dest_addr,
		   unsigned int eth_protocol,
		   unsigned int data_len, const unsigned char *data)
{


#ifdef DEBUG_ETH
  printc("\neth_transmit : Dest_Addr: %X:%X:%X:%X:%X:%X - Proto: %X \n"
	 "Info: %X:%X:%X ... ; Info Length = %d\n ",
	 dest_addr[0], dest_addr[1], dest_addr[2], dest_addr[3],dest_addr[4],
	 dest_addr[5], eth_protocol, data[0], data[1],
	 data[2], data_len);
#endif
	(*nic.transmit)(&nic, dest_addr, eth_protocol, data_len, data);
}

#ifdef TIME_MEASURE
long long tacc_handler=0;
long long tmax_handler=0;
long long tmin_handler=0;
unsigned char first_time=TRUE;
int msr_iter = 0;
#endif

int eth_handler(void *area, intr_t intr){

  eth_frame_t *E;
  eth_frame_t temp;

  // printc("IRQ-");

#ifdef TIME_MEASURE
  long long first_ticks, last_ticks, msr_ticks;
  rdtscll(first_ticks);
  if (first_time == TRUE){
    tmin_handler=first_ticks;

  }
#endif

  if( (*nic.handler)(protocol_filter) ){

    if( (E=alloc_ring_element(&rx_ring_queue)) == RING_FULL ){
      printf("Eth_Handler: Reception Buffer Overflow. Consider Redimension."
           "Packet LOST\n");
      temp.info_length=eth_poll(temp.eth_frame);
#ifdef TIME_MEASURE

      if (first_time == TRUE){
	first_time=FALSE;
      }else{
	// If not the first round we start storing the measures.
	rdtscll(last_ticks);
	msr_ticks=last_ticks - first_ticks -overhead_ticks;

	if ( msr_ticks < tmin_handler) {
	  tmin_handler = msr_ticks;
	}
	if ( msr_ticks > tmax_handler) {
	  tmax_handler = msr_ticks;
	}
	tacc_handler+=msr_ticks;
	msr_iter++;
      } // end if first time.
#endif

      return POSIX_INTR_HANDLED_NOTIFY;
    }
    E->info_length=eth_poll(E->eth_frame);
    sem_post(&sem);

#ifdef TIME_MEASURE

      if (first_time == TRUE){
	first_time=FALSE;
      }else{
	// If not the first round we start storing the measures.
	rdtscll(last_ticks);
	msr_ticks=last_ticks - first_ticks - overhead_ticks;

	if ( msr_ticks < tmin_handler) {
	  tmin_handler = msr_ticks;
	}
	if ( msr_ticks > tmax_handler) {
	  tmax_handler = msr_ticks;
	}
	tacc_handler+=msr_ticks;
	msr_iter++;
      } // end if first time.
      //printf("Measure Ireration: %d\n",msr_iter);
      if (msr_iter > MAX_MEASURE_ITER) {
	printc("Manejador de interrupciones:\n"
	       "Tmax: %lf  ; Tmin: %lf ; Tmed (iter: %d): %lf. "
	       "T. en u_seg \n (max_ticks: %qd - ticks_second: %qd)\n",
	       ((double)((double)tmax_handler/(double)ttps)*1e6),
	       ((double)((double)tmin_handler/(double)ttps)*1e6),
	       msr_iter,
	       ((double)((double)tacc_handler/(double)(ttps * msr_iter))*1e6),
	       tmax_handler, ttps );
	/*We reset values.*/
	msr_iter=0;
	tmax_handler = 0;
	tmin_handler = first_ticks;
	tacc_handler=0;
      }
#endif
    return POSIX_INTR_HANDLED_NOTIFY;
  }
#ifdef TIME_MEASURE

      if (first_time == TRUE){
	first_time=FALSE;
      }else{
	// If not the first round we start storing the measures.
	rdtscll(last_ticks);
	msr_ticks=last_ticks - first_ticks -overhead_ticks;

	if ( msr_ticks < tmin_handler) {
	  tmin_handler = msr_ticks;
	}
	if ( msr_ticks > tmax_handler) {
	  tmax_handler = msr_ticks;
	}
	tacc_handler+=msr_ticks;
	msr_iter++;
      } // end if first time.
#endif
  return POSIX_INTR_HANDLED_NOTIFY;
}//end function eth_handler


ssize_t eth_create(int create_arg)
{
  enum {sis900, rtl8139, eepro100, pcnet32} nic_card;
  //nic.dev.how_probe = PROBE_FIRST;
  //nic.dev.type = NIC_DRIVER;
#ifdef TIME_MEASURE
  long long tt0,tt1;
#endif

#ifdef TIME_MEASURE
  /* assess timing overhead. we can use currticks instead of rdtscll, but  */
  /* the former has more overhead since it is an ADA call.                 */
  rdtscll(tt0);
  rdtscll(tt1);
  overhead_ticks = tt1-tt0;
  /* get processor speed */
  rdtscll(tt0);
  nanosleep(&sleep_time_state,NULL);
  rdtscll(tt1);
  ttps = tt1-tt0-overhead_ticks;
  //printc("");
#endif

  if(sis900_probe(&nic.dev)){
    nic_card=sis900;
  }else if (eepro100_probe(&nic.dev)) {
    nic_card=eepro100;
  }else if(rtl8139_probe(&nic.dev)){
    nic_card=rtl8139;
  } else if (pcnet32_probe(&nic.dev)){
    nic_card=pcnet32;
  } else
    return -1;

  /* We inicialice the Rx queue. */
  init_ring(&rx_ring_queue);
  /* We inicialize ticks_per_sec */
  ticks_per_sec = ticks_per_second();
  tx_timeout = ticks_per_sec;

  if(posix_intr_associate(nic.irq, eth_handler, NULL, 0) )
          printc("eth_probe: Error installing IRQ handler %d\n",nic.irq);
  // else
    // MIRAR!!!!!
    //printf("eth_probe: IRQ %d\n",nic.irq);

  return 0;

} // end eth_create;

/******
  Open
*******/

ssize_t eth_open(int file_descriptor, int file_access_mode){

  if(!device_opened){
    (*nic.open)(&nic);
    posix_intr_unlock (nic.irq);
    //hwinterrupts_enable(nic.irq);
    device_opened=TRUE;
  }
  return 0;
}

/******
  Read
*******/

ssize_t eth_read(int file_descriptor, void *msg, size_t len){

  //  struct ethhdr *eth_header;
  //  char *eth_data;
  unsigned int data_len;

  if( (data_len=read_ring(&rx_ring_queue,msg)) < 0 )
    return -1;
  else
    return data_len;
}//end function eth_read

/*******
  Write
********/

ssize_t eth_write(int file_descriptor, void *msg, size_t len){

        struct ethhdr eth_header;
        unsigned char *eth_data;
        int i;
        i=0;
        eth_header = *(struct ethhdr *)msg;
        eth_data = (unsigned char *) (msg + sizeof(struct ethhdr));

        /*    eth_transmit(dest_addr,protocol,data_len,data); */
        eth_transmit(eth_header.h_dest,   // destination address.
                     eth_header.h_proto,  // Higher level protocol.
                     len - ETH_HLEN,       // data size.
                     eth_data);            // data.
        return len;
}

/*******
  Ioctl
********/

ssize_t eth_ioctl(int file_descriptor, ssize_t request, void *priv){

  switch(request){

  case ETH_HWADDR:
    memcpy(priv,nic.node_addr,ETH_ALEN);
    break;
  case ETH_BLOCKING_READ:
    rx_ring_queue.blocking_read=TRUE;
    break;
  case ETH_NON_BLOCKING_READ:
    rx_ring_queue.blocking_read=FALSE;
    break;
  case SET_PROTOCOL_FILTER:
    cli();
    protocol_filter=*(unsigned short *)priv;
    sti();
    break;
  case GET_PROTOCOL_FILTER:
    *(unsigned short *)priv=protocol_filter;
    break;

  default:
    break;
  }
  return 0;
}


void disable(struct dev *dev)
{
	if (dev->disable) {
		dev->disable(dev);
		dev->disable = 0;
	}
}


/********
  Close
*********/
int  eth_close(int file_descriptor)
{
  if(device_opened){
    disable(&nic.dev);
    device_opened=FALSE;
  }
  return 0;
}
