/*------------------------------------------------------------------------------
 *---------------------     ATH5K Driver Example     ---------------------------
 *------------------------------------------------------------------------------
 *                                                           V1.0   08/02/2010
 *
 *
 *  Feb 2010 - Samuel Cabrero <samuelcabrero@gmail.com>
 *		Initial release
 *
 *  ----------------------------------------------------------------------------
 *  Copyright (C) 2000-2009, Universidad de Zaragoza, SPAIN
 *
 *  Authors:
 *		Samuel Cabrero        <samuelcabrero@gmail.com>
 *		Danilo Tardioli	      <dantard@unizar.es>
 *		Jose Luis Villarroel  <jlvilla@unizar.es>
 *
 *  This is a sample program that send frames over the air. Frames can be 
 *  seen with wireshark or another MaRTE OS host running ath5k_rx_test.c 
 *  program.
 *
 *----------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <misc/timespec_operations.h>
#include <drivers/osdep.h>
#include "../drivers/ath5k/ath5k_interface.h"

/* Interface configuration */
#define FREQ            2412
#define RATE            RATE_12M
#define TXPOWER_DBM     15
#define ANTENNA_MODE	AR5K_ANTMODE_FIXED_A

#define ETHER_PROTO     0x6996

/* Pointer to the wifi card */
static struct ath5k_softc *tarjeta1;

/* Cached frame used for tx */
static unsigned char ethernet_frame[2500];

int initLowLevelCom() 
{
	bool broadcast, control, promisc, wait_for_ack, use_short_preamble;
	unsigned int count;
	unsigned char mac[ETH_ALEN];
	struct ethhdr *ethernet_header;

	/* Send output to serial port */
	//SERIAL_CONSOLE_INIT();

	/* Find and init the first card */
	tarjeta1 = ath5k_find(NULL, FREQ, RATE, TXPOWER_DBM, ANTENNA_MODE);

	if (!tarjeta1)
		return -1;

	/* Config the hardware rx filter */
	broadcast = true;
	control = false;
	promisc = false;
	ath5k_config_filter(tarjeta1, broadcast, control, promisc);

	/* Select tx rate, disable soft retries and disable ACK waiting for unicast frames */
	wait_for_ack = false;
	use_short_preamble = false;
	count = 1;
	ath5k_config_tx_control(tarjeta1, count, wait_for_ack, use_short_preamble);

	/* Disable ACK unicast frames */
	ath5k_config_disable_ack(tarjeta1, true);

	/* Set debug level */
//	ath5k_config_debug_level(tarjeta1, 0xFFFFFFFF);	/* All */
//	ath5k_config_debug_level(tarjeta1, 0x00000200); /* Tx */
	ath5k_config_debug_level(tarjeta1, 0x00000000); /* None */

	/* Get card's mac address */
	ath5k_get_interface_mac(tarjeta1, mac);

	/* Init cached ethernet frame */
	ethernet_header = (struct ethhdr *)ethernet_frame;
	memset(ethernet_header->h_dest, 0xFF, ETH_ALEN);
	memcpy(ethernet_header->h_source, mac, ETH_ALEN);
	ethernet_header->h_proto = htons(ETHER_PROTO);

	printf("Initialization Completed...\n");

	return 1;
}


int llsend(char * f, int size)
{
	unsigned char *p;
	int ret;

	p = ethernet_frame + ETH_HLEN;

	/* Copy data */
	memcpy(p, f, size);
	p += size;

	/* Send to driver */
	usleep(100); //wait 100 us to allow the scheduler to work
	ret = ath5k_send(tarjeta1, ethernet_frame, p - ethernet_frame);
	
	if (ret < 0)
		return -1;

	return 1;
}


int main (int argc, char *argv[])
{
	struct timespec siguiente, periodo;
	unsigned long ordinal;
	
	ordinal = 0;
	periodo.tv_sec = 0;
//	periodo.tv_nsec = 100 * 1000 * 1000;	/* 100 ms */
	periodo.tv_nsec = 10 * 1000 * 1000;		/* 100 ms */
//	periodo.tv_nsec = 1 * 1000 * 1000;		/* 1 ms   */
//	periodo.tv_nsec = 500 * 1000;			/* 500 us */
//	periodo.tv_nsec = 100 * 1000;			/* 100 us */
//	periodo.tv_nsec = 50 * 1000;			/* 10 us  */

	initLowLevelCom();

	while (1)
	{
		ordinal++;

		llsend((char *)&ordinal, sizeof(ordinal));

		nanosleep(&periodo, NULL);		
	}
}
