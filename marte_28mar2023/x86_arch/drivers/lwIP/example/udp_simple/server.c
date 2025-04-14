//      server_udp.c
//      
//      Copyright 2011 Hector Perez <perezh@unican.es>
//      
//      This program is free software; you can redistribute it and/or modify
//      it under the terms of the GNU General Public License as published by
//      the Free Software Foundation; either version 2 of the License, or
//      (at your option) any later version.
//      
//      This program is distributed in the hope that it will be useful,
//      but WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//      GNU General Public License for more details.
//      
//      You should have received a copy of the GNU General Public License
//      along with this program; if not, write to the Free Software
//      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
//      MA 02110-1301, USA.


// Creates a datagram server.  The port number is passed as an argument.  
// This server runs forever

#include "lwip/opt.h"

#include "lwip/api.h"
#include "lwip/sys.h"
#include "lwip/sockets.h"

//#include "lwip/init.h" //v.14
#include "lwip/tcpip.h"
#include "netif/etharp.h"
/*#include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/pbuf.h"
#include "lwip/tcp.h"
#include "lwip/netif.h"
#include "lwip/stats.h"*/

#include "netif/eth0_if.h"

/*#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>*/

#include <stdio.h>
#include <pthread.h>

//  Errors
#include "error_checks.h"

static pthread_t thread_id;
static int sock, fromlen, n;
static struct sockaddr_in server;
static struct sockaddr_in from;
#define BUFFER_SIZE 5000
static char buf[BUFFER_SIZE];

void error(char *msg)
{
    perror(msg);
    exit(0);
}

int endless_loop () {
	
   //  Endless loop
   printf ("Receiving data...\n");
   while (1) {
       fromlen = sizeof(from);
       n = recvfrom(sock,buf,BUFFER_SIZE,0,(struct sockaddr *)&from,(socklen_t *) &fromlen);
       if (n < 0) error("recvfrom");
       printf ("Received %d bytes!!!!!!!!!!!!!!!!!!\n", n);
       //printf ("%s", (char *)buf);
       puts (buf);
       n = sendto(sock,"Got your message\n",17,
                  0,(struct sockaddr *)&from,fromlen);
   
       if (n  < 0) error("sendto");
   }
	
}

struct netif netif;

static void
tcpip_init_done(void *arg)
{
  ip_addr_t ipaddr, netmask, gateway;
  sys_sem_t *sem;
  sem = arg;

  /*
    CHANGE THESE to suit your own network configuration:
  */
 //  Setup my Network interface 
 IP4_ADDR(&gateway, 192,168,0,1);
 IP4_ADDR(&ipaddr, 192,168,0,2);
 IP4_ADDR(&netmask, 255,255,255,0);

 netif_add(&netif, &ipaddr, &netmask, &gateway, NULL, eth0_if_init, tcpip_input);
 
 netif_set_default(&netif);
 netif_set_up(&netif);
 
 // static ARP table
 struct eth_addr client_eth = {{0x00,0x30,0x64,0x07,0xA2,0x64}};
 IP4_ADDR(&ipaddr, 192,168,0,3);
 etharp_add_static_entry(&ipaddr, &client_eth);

  sys_sem_signal(sem);

}

void my_init(void){
  sys_sem_t sem;
 
  if(sys_sem_new(&sem, 0) != ERR_OK) {
    LWIP_ASSERT("failed to create semaphore", 0);
  }
  tcpip_init(tcpip_init_done, &sem);
  sys_sem_wait(&sem);
  sys_sem_free(&sem);
}

void init_lwip_stack() {
    my_init();
    
    //lwip_socket_init ();
}

pthread_t create_new_app_thread (int priority, void *code_to_execute, void *args) {
	
	  pthread_t created_thread;
	  pthread_attr_t attr;
	  struct sched_param param;
	  
	  // Set main priority
	  //assert (max_priority >= priority && min_priority <= priority);
	  //CHK( pthread_setschedprio (pthread_self(), priority) );
	  
	  // Create thread
	  CHK( pthread_attr_init (&attr) );
	  CHK( pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED) );
	  CHK( pthread_attr_setschedpolicy (&attr, SCHED_FIFO) );
	  
	  param.sched_priority = priority;
	  CHK( pthread_attr_setschedparam (&attr, &param) );
	  CHK( pthread_create (&created_thread, &attr, code_to_execute, args) );
	  return created_thread;
	
}

int Test_Server_UDP_Network (int argc, char *argv[])
{
   int THREAD_PRIORITY = 0;
   	  
   init_lwip_stack();
   
   //  UDP Server
   sock=socket(AF_INET, SOCK_DGRAM, 0);
   if (sock < 0) error("Opening socket");
   
   memset(&server, 0, sizeof(server));
   
   server.sin_family=AF_INET;
   server.sin_addr.s_addr=htonl(INADDR_ANY);
   server.sin_port=htons(50000);
   
   if (bind(sock,(struct sockaddr *)&server,sizeof(server))<0) 
       error("binding");
   fromlen = sizeof(struct sockaddr_in);
    
   THREAD_PRIORITY = 70;
   //  Create RT Thread   
   thread_id = create_new_app_thread (THREAD_PRIORITY, 
	                                  endless_loop, 
	                                  NULL /* Args */);
	
   //  Wait for RT Thread                                 
   pthread_join (thread_id, NULL);

 }
 
 int main (int argc, char *argv[]) {

	printf ("Test for Server UDP network operations\n");
	
	Test_Server_UDP_Network (argc, argv);

	printf ("Done\n");
	return 0;
}

