#include <stdlib.h>
#include <stdio.h>
#include <drivers/xbeeAPI.h>
#include <pthread.h>
#include <string.h>

int main(){
    unsigned char command[2];
    unsigned char value[8]; 
    unsigned char addr[8]={0x00,0x13,0xA2,0x00,0x40,0x0A,0x41,0x3B};

    struct sched_param param;

    param.sched_priority = PRIO_MED;

    if (xbee_init_module("/dev/ttyS0", "74", 115200, param, SCHED_FIFO)==FALSE)	// Abre el puerto serie. create()
    	exit(0);	// Configura el modulo y el puerto serie.

    xbee_writeATCommand("ID",NULL,0);

    while(1){
	printf("Escribe el comando--> \n");
	gets(command);
	printf("Escribe el valor--> \n");
	gets(value);
	printf("Comando-->%s, value-->%s\n",command,value);
	if(strcmp(value,"null")==0)
	    xbee_writeRemoteATCommand(addr,command,NULL,0);
	else
	    xbee_writeRemoteATCommand(addr,command,value,strlen(value));
    }
    xbee_end_module();                     // Cierro el puerto serie.

    return 0;
}

