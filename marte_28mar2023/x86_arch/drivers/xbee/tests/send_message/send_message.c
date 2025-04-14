#include <stdlib.h>
#include <stdio.h>
#include <drivers/xbeeAPI.h>
#include <pthread.h>
#include <string.h>

int main(){
    char msg[200];
    char addr[8]={0x00,0x13,0xA2,0x00,0x40,0x0A,0x41,0x3B};

    struct sched_param param;

    param.sched_priority = PRIO_MED;

    if (xbee_init_module("/dev/ttyS0", "74", 115200, param, SCHED_RR)==FALSE)	// Abre el puerto serie. create()
    	exit(0);	// Configura el modulo y el puerto serie.

    while(1){
	printf("Write a message--> \n");
	gets(msg);
	xbee_writeTransmiteRequest(Coordinator,msg);
    }
    xbee_end_module();                     // Cierro el puerto serie.

    return 0;
}

