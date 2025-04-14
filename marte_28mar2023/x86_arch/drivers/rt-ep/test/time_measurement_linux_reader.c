/*----------------------------------------------------------------------------
 *-------------------------        TIME_MEASURE       ------------------------
 *----------------------------------------------------------------------------
 *                                                             {LINUX_VERSION}
 *
 *                  'r e a d e r _ t i m e _ m e a  s u r e . h'
 *
 *                                     C
 *
 *
 * File 'reader_time_measure.h'                                       By Chema.
 *                                                          Jose Maria Martinez
 *                                                         <martinjm@unican.es>
 *
 * In this file we implement the reception part of the time measure.
 *
 *
 *---------------------------------------------------------------------------*/


#include <stdio.h>
#include <unistd.h>
#include <pthread.h>

 // Linux includes.
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/ether.h>
#include <sys/utsname.h>
#include <net/if.h>
#include <fcntl.h>
#include <linux/if_packet.h>
#include <time.h>
#include <string.h>
#include <signal.h>
#include "time_measurement_linux_reader.h"


/* We define the length of an ethernet address */
#define ETH_ALEN 6

/* eth_header_t : The header of an ethernet frame.                     */
typedef struct {
  unsigned char   h_dest[ETH_ALEN];       /* destination eth addr */
  unsigned char   h_source[ETH_ALEN];     /* source ether addr    */
  unsigned short  h_proto;                /* packet type ID field */

} eth_header_t;


/* ethernet_frame_t : A full ethernet frame.                           */
typedef struct {
  eth_header_t eth_header;
  char data[1500];
} ethernet_frame_t;

/*  measures_t - The receiving data type. */
typedef struct{
  unsigned char measure_name[64];
  unsigned int name_length;
  double first_measure;
  double tmax;
  double tmin;
  double tmed;
  unsigned int iter;
} measures_t;

/* measure_packet_t : Full ethernet packet with header and measure type */
typedef struct {
  eth_header_t eth_header;
  /* The size of the data (measure).*/
  unsigned short int packet_size;
  measures_t measure;
} measure_packet_t;

/* print_measure_info : Will output the ethernet II packet information  */
/*                      included in measure_packet_t. The output info   */
/*                      will be numered and identified with the         */
/*                      packet_counter argument.                        */
int print_measure_info( measure_packet_t *msr_packet){

    printf(" Source Address: %X:%X:%X:%X:%X:%X : ",
	       msr_packet->eth_header.h_source[0],
	       msr_packet->eth_header.h_source[1],
	       msr_packet->eth_header.h_source[2],
	       msr_packet->eth_header.h_source[3],
	       msr_packet->eth_header.h_source[4],
	       msr_packet->eth_header.h_source[5]);

    printf(" Destination Address: %X:%X:%X:%X:%X:%X \n",
	       msr_packet->eth_header.h_dest[0],
	       msr_packet->eth_header.h_dest[1],
	       msr_packet->eth_header.h_dest[2],
	       msr_packet->eth_header.h_dest[3],
	       msr_packet->eth_header.h_dest[4],
	       msr_packet->eth_header.h_dest[5]);

    printf(" Info Size: %d - ", msr_packet->packet_size);

    msr_packet->measure.measure_name[msr_packet->measure.name_length]='\0';
    printf("Identifier: %s \n", msr_packet->measure.measure_name);
    printf(" First_Measure: %g sec", msr_packet->measure.first_measure);
    printf(" - Iter: %d \n",msr_packet->measure.iter);
    printf(" Tmax: %g sec", msr_packet->measure.tmax);
    printf(" - Tmin: %g sec", msr_packet->measure.tmin);
    printf(" - Tavg: %g sec\n\n", msr_packet->measure.tmed);

    fflush(stdin);
    return 0;
} // end print_measure_info

/* write_shared_resource : will be used to write a proper Shared resource */
/*                         entry                                          */

typedef enum {inmediate_ceiling = 0,
	      priority_inheritance = 1,
	      stack_resource = 2} shared_resource_t;

int write_shared_resource(FILE *Fd, shared_resource_t resource_type,
			  char *resource_name){

  switch (resource_type) {
  case inmediate_ceiling:
    fprintf(Fd,"Shared_Resource (\n\tType\t => Inmediate_Ceiling_Resource,"
	    "\n\tName => %s );\n", resource_name);
    break;
  case priority_inheritance:
    /* Not implemented */
    return -1;
    //    break;
  case stack_resource:
    /* Not implemented */
    return -1;
    // break;
  default:
    /* Error */
    return -1;
  }// end switch.

  return 0;
}//write_shared_resource


/* write_operation :  Will be used to write a proper Shared resource entry  */

typedef enum { simple = 0,
	       composite = 1,
	       enclosing = 2} operation_t;

/* write_operation : Will write in the file the operation corresponding to */
/*                   the measure_t receiving packet, if the measure is not */
/*                   valid (p.e. the iteractions of the measure is 0)      */
/*                   nothing is written and -1 is returned.                */
int write_operation(FILE *Fd, operation_t type, measures_t *measure_info,
		    char *station_name){

  char *operation_name;
  char *shared_resource_name;
  /* Since the separator is ':' we need to extract the shared resource and */
  /* the operation name. Remember Resource : operation                     */
  shared_resource_name = strtok(measure_info->measure_name, ":");
  operation_name = strtok(NULL,":");
  /* We now write the operation */
  if(measure_info->iter == 0){
    /* There is a no valid operation, it never get called*/
    return -1;
  }
  fprintf(Fd, "Operation(\n\tType\t\t\t => ");

  switch (type) {
  case simple:
    fprintf(Fd,"Simple,\n");
    break;
  case composite:
    fprintf(Fd,"Composite,\n");
    break;
  case enclosing:
    fprintf(Fd,"Enclosing,\n");
    break;
  default:
    return -1;
    //    break;
  }// end switch
  fprintf(Fd,"\tName\t\t\t => %s_%s,\n",operation_name, station_name);
  fprintf(Fd,"\tWorst_Case_Execution_Time => %g,\n",measure_info->tmax);
  fprintf(Fd,"\tBest_Case_Execution_Time  => %g,\n",measure_info->tmin);
  if (measure_info->iter > ITERATIONS_THRESHOLD){
    fprintf(Fd,"\tAvg_Case_Execution_Time\t => %g,\n",measure_info->tmed);
  }
  fprintf(Fd,"\tShared_Resources_List\t => (%s_%s));\n",shared_resource_name,
	  station_name);

  return 0;
}// write_operation

/* finish_handler : For now the only way to finish the program is by ctrl+C */
/*                  That means we need to close the files when that signal  */
/*                  arrives (SIGINT) that is the purpose of this handler.   */

/* This file descriptors and socket have to be global to be visible by the  */
/* handler.                                                                 */
FILE *fd_operations, *fd_resources;
int socket_srv;

void finish_handler(int x) {

  switch(x) {
  case SIGINT:
    fprintf(stdout,"Finishing the time measure reporter...");
    close(socket_srv);
    fclose(fd_operations);
    fclose(fd_resources);
    fprintf(stdout,"OK!\n");
    exit(0);
    break;
  default:
    break;
  }
  return;
}// end finish_handler

static int addrcmp(unsigned char *addr1,
		    unsigned char *addr2, const int len){

  int i;
  for(i=0;i<len;i++){
    if(addr1[i]==addr2[i]){
      if(i==len-1)
	return 1;
    } else
      return 0;
  }// en for.
  return 0;
}// end function addrcmp



int main(){
  int reception_counter, status, i;
  struct ether_addr recv_sta;

  struct sockaddr_ll current_sta_addr;

  struct packet_mreq opcion;
  struct ifreq ifr;
  measure_packet_t measure_packet;
  ethernet_frame_t receive_buffer;

  char device[]=NIC_INTERFACE;
  char *sta_name;


  /*********************************************************************/
  /* SET UP THE SOCKET *************************************************/
  /*********************************************************************/

  if((socket_srv=socket(PF_PACKET,SOCK_RAW,htons(TIME_MEASURE_PROTOCOL)))< 0 ){
    perror("Creation of socket_srv");
    return -1;
  }

  /*  we attempt to achieve the MAC address of the current station. */
  memset(&ifr,0,sizeof(struct ifreq));
  strcpy(ifr.ifr_name, device );

  if(ioctl(socket_srv, SIOCGIFHWADDR, &ifr) < 0 ){
    perror("ioctl:SIOCGIFHADDR");
    exit(-1);
  }
  memcpy(current_sta_addr.sll_addr,ifr.ifr_addr.sa_data,ETH_ALEN);

  /*  we now try to fetch the interface index. */
  if(ioctl(socket_srv, SIOGIFINDEX, &ifr) < 0 ){
    perror("ioctl:SIOGIFINDEX");
    exit(-1);
  }
  current_sta_addr.sll_ifindex=ifr.ifr_ifindex;
  // current_sta_addr.sll_ifindex=2;

  /* we also have to inicialize the other fields of current_sta_addr */
  current_sta_addr.sll_family=AF_PACKET;
  current_sta_addr.sll_protocol=htons(TIME_MEASURE_PROTOCOL);
  current_sta_addr.sll_halen=ETH_ALEN;

  bind( socket_srv, (struct sockaddr*)&current_sta_addr,
	sizeof(current_sta_addr));
  /*Now we set promiscuous mode.*/
  memset(&opcion,0,sizeof(opcion));
  opcion.mr_ifindex=ifr.ifr_ifindex;
  opcion.mr_type=PACKET_MR_PROMISC;
  status=setsockopt(socket_srv, SOL_PACKET, PACKET_ADD_MEMBERSHIP,
		    (void *)&opcion,(socklen_t)sizeof(opcion));

  /*******************************************************************/
  /* DISPLAYING CURRENT STATION INFO *********************************/
  /*******************************************************************/
  printf("Current MAC: %X:%X:%X:%X:%X:%X.\n",
	 current_sta_addr.sll_addr[0],
	 current_sta_addr.sll_addr[1],
	 current_sta_addr.sll_addr[2],
	 current_sta_addr.sll_addr[3],
	 current_sta_addr.sll_addr[4],
	 current_sta_addr.sll_addr[5]);

  printf("Listening to protocol: 0x%X \n", TIME_MEASURE_PROTOCOL);

  /*******************************************************************/
  /* Opening the result files.                                       */
  fd_operations = fopen(OUTPUT_OPERATIONS_FILE, "w+");
  fd_resources = fopen(OUTPUT_RESOURCES_FILE, "w+");

  /*******************************************************************/
  /* Enabling the handler                                            */
  signal(SIGINT,finish_handler);

  /********************************************************************/
  /* Place a header in each file                                      */
  fprintf(fd_operations, "-- Operations\n-- Critical Sections\n\n ");
  fprintf(fd_resources, "-- Resources\n\n ");

  // We would like the first packet numbered with 1 not 0.
  reception_counter=1;
  /* Begin the reception part */
  for(;;){
    /*****************************************************************/
    /* Receiving a packet from the net                               */
    /*****************************************************************/
    status=recvfrom(socket_srv, &receive_buffer,
		    sizeof(receive_buffer), 0, NULL, 0);


    memcpy(&measure_packet, &receive_buffer, sizeof(measure_packet) );

    /*    if (measure_packet.packet_size != sizeof(measure_packet.measure) ) {
      printf("Recibido un paquete de tama� erroneo.\n "
	     "Descartando paquete %d ...",reception_counter);
      continue;
      }*/
    /******************************************************************/
    /* Displaying packet Info                                         */
    /******************************************************************/
    printf("Packet %d - Frame Length: %d, Protocol: %x \n",
	   reception_counter, status,
	   ntohs( measure_packet.eth_header.h_proto));
    // We print the measure information
    print_measure_info(&measure_packet);

    /******************************************************************/
    /* Need to find out the which station the operation / resource    */
    /* belongs to: We are ready to write the files properly           */
    /* attaching to the operation the name of the station.            */
    for (i=0; i < sizeof(stations_table) / sizeof(stations_table[0]); i++){

      if(addrcmp((unsigned char *)ether_aton(stations_table[i].mac_addr),
			     measure_packet.eth_header.h_source, ETH_ALEN )){
      	write_operation(fd_operations, simple, &measure_packet.measure,
			stations_table[i].name);



      } // end if
    } // end for

    reception_counter++;
  } // end for.


  return 0;
}//end main.
