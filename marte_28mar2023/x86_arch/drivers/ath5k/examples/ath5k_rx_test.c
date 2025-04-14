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
 *  This is a sample program that receive the frames sent by ath5k_tx_test.c
 *  and show some statistics.
 *
 *----------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <misc/timespec_operations.h>
#include <drivers/osdep.h>
#include <drivers/if_ether.h>
#include "../drivers/ath5k/ath5k_interface.h"

/* Interface configuration */
#define FREQ            2412
#define RATE            RATE_12M
#define TXPOWER_DBM     15
#define ANTENNA_MODE    AR5K_ANTMODE_FIXED_A

#define ETHER_PROTO     0x6996

/* Pointer to the wifi card */
static struct ath5k_softc *tarjeta1;

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

	/* Select tx rate, disable soft retries and disable ACK waiting for 
	   unicast frames */
	wait_for_ack = false;
	use_short_preamble = false;
	count = 1;
	ath5k_config_tx_control(tarjeta1, count, wait_for_ack, 
	                        use_short_preamble);

	/* Disable ACK unicast frames */
	ath5k_config_disable_ack(tarjeta1, true);

	/* Set debug level */
//	ath5k_config_debug_level(tarjeta1, 0xFFFFFFFF);	/* All */
//	ath5k_config_debug_level(tarjeta1, 0x00000300); /* Rx */
	ath5k_config_debug_level(tarjeta1, 0x00000000); /* None */
	
	/* Get card's mac address */
	ath5k_get_interface_mac(tarjeta1, mac);

	printf("Initialization Completed...\n");

	return 1;
}

double avg (double *v, int vlen)
{
	double total, avg;
	int i;

	total = 0.0;
	avg = 0.0;

	for (i = 0; i < vlen; i++)
		total += v[i];

	return (total / vlen);
}

int main (int argc, char *argv[])
{
	frame_t frame;
	int i, perdidos, ultimo_ordinal, contador,offset;
	struct ethhdr *ethernet_header;
	struct timespec ts1, ts2;
	double lq[100], delay[100];
	double elapsed, delay_avg;
	
	i = 0;
	perdidos = 0;
	ultimo_ordinal = 0;
	contador = 0;

	initLowLevelCom();

	while (1)
	{
		unsigned long ordinal;
		int media;

		/* Wait for a frame */
		clock_gettime(CLOCK_REALTIME, &ts1);
		ath5k_recv(tarjeta1, &frame, NULL);
		clock_gettime(CLOCK_REALTIME, &ts2);
		elapsed = timespec_to_double(&ts2) - timespec_to_double(&ts1);
		
		/* Check if it is an ETHER_PROTO frame */
		ethernet_header = (struct ethhdr *)frame.info;
		if (ntohs(ethernet_header->h_proto) != ETHER_PROTO)
			continue;

		/* Statistics */
		ordinal = *((unsigned long *)(frame.info + 14));
		if (contador == 0)
			offset = ordinal;
		if (ultimo_ordinal == 0)
			ultimo_ordinal = ordinal - 1;

		contador++;
		perdidos += ordinal - ultimo_ordinal - 1;
		lq[i] = (double)frame.link_quality;
		delay[i] = elapsed;
		i = (i + 1) % 100;
		ultimo_ordinal = ordinal;

		if ((contador % 100) == 0)
		{
			double perdida;
			perdida = ((double)perdidos / ((double)(ordinal-offset))) * 100;
			media = avg(lq, 100);
			delay_avg = avg(delay, 100);
			printf("Dly(avg): %.4f  Seq: %6d  Lost: %4d  Loss: %.3f  Rate: %3d  LQ(avg): %3d\n", 
					delay_avg, ordinal, perdidos, perdida, frame.rate, media);
		}
	}
}
