/**
 * File: Simple TCP client for test in MaRTE OS
 *
 * @Author: H. Perez <marte>
 * @Date:   28-Feb-2018
 * @Email:  perezh@unican.es
 * @Last modified by:   H. Perez
 * @Last modified time: 01-Mar-2018
 *
 */

#include <sys/types.h>

#include <time.h>
#include "lwip/sockets.h"
#include "netif/etharp.h"
#include "netif/eth0_if.h"
#include "lwip/tcpip.h"
#include "lwip/sys.h"

#include <stdio.h>
#include <unistd.h>

//  Errors
#include "error_checks.h"

// Payload generator
#include "payload.h"

#define MAX_ITERATIONS 10
#define MAX_PAYLOAD    20        /* Create 20 bytes of data */

//  Network variables
static char* buffer = NULL;
static char reply[256];
static int sock, length, retcode;
static struct sockaddr_in server, from;

struct netif netif;

/**
 * [error Helper function to output errors]
 * @param [msg] [msg for the output]
 */
void error(char *msg) {
		perror(msg);
		exit(0);
}

/**
 * [main_loop  Thread's main loop to send / receive data through the network]
 * @return 0 on success
 */
int main_loop (){

		int index;

		for (index=1; index<=MAX_ITERATIONS; index++) {
				/* Operate with data */
				printf ("Sending to...%s\n", inet_ntoa(server.sin_addr));
				retcode=sendto(sock,buffer,
				               strlen(buffer),0,(struct sockaddr *)&server,length);
				if (retcode < 0) error("Sendto");

				printf ("Waiting reply from server...\n");
				retcode = recvfrom(sock,&reply,256,0,(struct sockaddr *)&from,
				                   (socklen_t *) &length);
				if (retcode < 0) error("recvfrom");
		}
		return 0;
}

/**
 * [tcpip_init_done User-defined init operations after LWIP initialization.
 * This function is executed by LWIP.]
 * @param arg [semaphore to let LWIP signal user's thread]
 */
static void tcpip_init_done(void *arg) {
		ip_addr_t ipaddr, netmask, gateway;
		sys_sem_t *sem;
		sem = arg;

		/*
		   CHANGE THESE to suit your own network configuration:
		 */
		//  Setup my Network interface
		IP4_ADDR(&gateway, 193,144,198,1);
		IP4_ADDR(&ipaddr, 193,144,198,38);
		IP4_ADDR(&netmask, 255,255,255,0);

		//  add network interface and set init functions
		netif_add(&netif, &ipaddr, &netmask, &gateway, NULL, eth0_if_init, tcpip_input);
		netif_set_default(&netif);
		netif_set_up(&netif);

		sys_sem_signal(sem);
}

/**
 * [my_init User-defined init operations before LWIP initialization]
 */
void my_init(void){
		sys_sem_t sem;

		if(sys_sem_new(&sem, 0) != ERR_OK) {
				LWIP_ASSERT("failed to create semaphore", 0);
		}
		// Init LWIP layer
		tcpip_init(tcpip_init_done, &sem);
		sys_sem_wait(&sem); // Wait for LWIP to finish init
		sys_sem_free(&sem);
}

/**
 * [Test_TCP_Network  Networking operations for TCP/IP]
 * @param  argc [not used]
 * @param  argv [not used]
 * @return      [0 on success]
 */
int Test_TCP_Network (int argc, char *argv[]) {
		ip_addr_t ipaddr;

		//  Stack init
		my_init();

		//  Create TCP socket
		sock= socket(AF_INET, SOCK_STREAM, 0);
		if (sock < 0) error("socket");

		memset(&server, 0, sizeof(server));
		server.sin_family = AF_INET;

		// CHANGE THIS: Set IP address for remote server
		IP4_ADDR(&ipaddr, 193,144,198,40);
		inet_addr_from_ipaddr (&server.sin_addr, &ipaddr);

		// CHANGE THIS: Set port for remote server
		server.sin_port = htons(50000);
		length=sizeof(struct sockaddr_in);

		//  Ask for connection to the remote server
		if (connect(sock,(struct sockaddr *)&server,sizeof(server)) < 0)
				error("ERROR connecting to remote server");

		//  Create payload data
		buffer = create_char_buffer (MAX_PAYLOAD);

		// Set static ARP table if required
		// struct eth_addr server_eth = {{0x18,0x60,0x24,0xEE,0x77,0x55}};
		// etharp_add_static_entry(&ipaddr, &server_eth);

		retcode = main_loop ();

		/* Release data */
		release_char_buffer (buffer);
		return 0;
}

int main (int argc, char *argv[]) {

		printf ("Test for TCP network operations\n");

		Test_TCP_Network (argc, argv);
		printf ("Done\n");
		return 0;
}
