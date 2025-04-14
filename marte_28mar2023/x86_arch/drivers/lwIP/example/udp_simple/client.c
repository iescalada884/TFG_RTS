//      client_udp.c
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

//      UDP client to take 10_000 time measures

#include <sys/types.h>

#include <time.h>
#include <misc/timespec_operations.h>
#include <pthread.h>
#include "lwip/sockets.h"
//#include "lwip/netdb.h" //v1.4
#include "netif/etharp.h"

//#include "lwip/init.h" //v.1.4
#include "netif/eth0_if.h"
#include "lwip/tcpip.h"
#include "lwip/sys.h"
/* #include "lwip/api.h"
   #include "lwip/sys.h"
   #include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/pbuf.h"
#include "lwip/tcp.h"
#include "lwip/tcpip.h"
#include "lwip/netif.h"
#include "lwip/stats.h"*/

/*#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>*/
#include <stdio.h>
#include <unistd.h>

//  Errors
#include "error_checks.h"

// Payload generator
#include "payload.h"

#define CLOCK CLOCK_MONOTONIC
#define PERIOD_NSECS 1E+6

//  Timing variables
static struct timespec next_activation, the_period;	
static char* buffer = NULL;
static char reply[256];
static int sock, length, retcode, max_measures;
static struct sockaddr_in server, from;
//static struct hostent *hp;   
static long the_payload = 0;
static pthread_t thread_id;

void error(char *);

int main_loop (){
	
   int index;
	 
   the_period.tv_sec = 1;
   the_period.tv_nsec = PERIOD_NSECS;
   CHK ( clock_gettime(CLOCK, &next_activation) );
   
   printf ("Sending to...%s\n", inet_ntoa(server.sin_addr));
   for (index=1;index<=max_measures;index++){
	   
	    CHK (clock_nanosleep(CLOCK, TIMER_ABSTIME,
				 &next_activation, NULL));
	
  	      
	    /* Operate with data */ 
	    retcode=sendto(sock,buffer,
	                   strlen(buffer),0,(struct sockaddr *)&server,length);
	    if (retcode < 0) error("Sendto");
	       
	    printf ("Waiting reply...\n");
	    retcode = recvfrom(sock,&reply,256,0,(struct sockaddr *)&from, (socklen_t *) &length);
	    if (retcode < 0) error("recvfrom");
	        	        
	    //  Compute next activation
	    incr_timespec (&next_activation, &the_period);
	    
	    //write(1,"Got an ack: ",12);   	   
	   
   }
   /* Display log results */
   // Output to file 
    printf( "File cannot be opened\n" );

	
	
	
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
 IP4_ADDR(&ipaddr, 192,168,0,3);
 IP4_ADDR(&netmask, 255,255,255,0);

 netif_add(&netif, &ipaddr, &netmask, &gateway, NULL, eth0_if_init, tcpip_input);
 
 netif_set_default(&netif);
 netif_set_up(&netif);

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

int Test_UDP_Network (int argc, char *argv[])
{
   int THREAD_PRIORITY = 0;
   ip_addr_t ipaddr;
   
   //  Stack init
   my_init();
   
   //  Create UDP socket
   sock= socket(AF_INET, SOCK_DGRAM, 0);
   if (sock < 0) error("socket");

   memset(&server, 0, sizeof(server));
   server.sin_family = AF_INET;
   
   //  Server parameter as hostname   
   /*hp = gethostbyname("192.168.0.2");
   if (hp==0) error("Unknown host");
   memcpy(&server.sin_addr, hp->h_addr, hp->h_length);*/
   
   //IP4_ADDR(&ipaddr, 193,144,198,38);
   IP4_ADDR(&ipaddr, 192,168,0,2);
   inet_addr_from_ipaddr (&server.sin_addr, &ipaddr);
   
   //  Server port      
   server.sin_port = htons(50000);
   length=sizeof(struct sockaddr_in);
   
   //  Number of measures
   max_measures = 10;
   
   //  Payload
   /* Create data */
   the_payload = 300;
   buffer = create_char_buffer (the_payload);
   
   // static ARP table
   struct eth_addr server_eth = {{0x00,0x30,0x64,0x07,0xA2,0x62}};
   etharp_add_static_entry(&ipaddr, &server_eth);
   
   THREAD_PRIORITY = 40;
   //  Create RT Thread
   printf ("App thread\n"); 
   sleep (2);  
   thread_id =create_new_app_thread (THREAD_PRIORITY, 
				     main_loop, 
	                             NULL /* Args */);
	
   //  Wait for RT Thread                                 
   pthread_join (thread_id, NULL);

   /* Release data */
   release_char_buffer (buffer);
}

void error(char *msg)
{
    perror(msg);
    exit(0);
}

int main (int argc, char *argv[]) {

	printf ("Test for UDP network operations\n");
	
	Test_UDP_Network (argc, argv);

	printf ("Done\n");
	return 0;
}
