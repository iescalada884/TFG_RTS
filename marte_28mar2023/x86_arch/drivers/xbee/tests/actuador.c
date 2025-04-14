#include <stdlib.h>
#include <stdio.h>
#include <drivers/xbeeAPI.h>
#include <pthread.h>
#include <string.h>

int main(){
    char action[1];
    unsigned char *mi_data; 
    char addr[8]={0x00,0x13,0xA2,0x00,0x40,0x78,0xF0,0x66};		//Router 1
    char addrRobot[8]={0x00,0x12,0x4B,0x00,0x00,0x81,0x68,0x36};	//Robot 1
    char clusterRequest[2]={0xC0,0x7E};
    unsigned int mi_lenght=0;  

    if (xbee_init_module("/dev/ttyS0", "74", 115200)==NULL)	// Abre el puerto serie. create()
    	exit(0);

    printf("Puerto ttyS0 configurado\n");  

    while(1){
	xbee_writeATCommand("ID",NULL,0);
	if(xbee_readData(&mi_data)==FALSE)
	    exit(0);

	printf("orden-->%x\n",*mi_data);

	if(*mi_data==0x02){
	    xbee_explicitAddressingZigBee(addrRobot,forward,clusterRequest);
	}
	if(*mi_data==0x01){
	    xbee_explicitAddressingZigBee(addrRobot,stop,clusterRequest);
	}
	if(*mi_data==0x03){
	    xbee_explicitAddressingZigBee(addrRobot,backward,clusterRequest);
	}
	if(*mi_data==0x08){
	    xbee_explicitAddressingZigBee(addrRobot,shoot,clusterRequest);
	}
	if(*mi_data==0x09){
	    xbee_explicitAddressingZigBee(addrRobot,sideLeft,clusterRequest);
	}
	if(*mi_data==0x0a){
	    xbee_explicitAddressingZigBee(addrRobot,sideRight,clusterRequest);
	}
	if(*mi_data==0x11){
	    xbee_explicitAddressingZigBee(addrRobot,leftWalkBackward,clusterRequest);
	}
	if(*mi_data==0x14){
	    xbee_explicitAddressingZigBee(addrRobot,hello,clusterRequest);
	}
	if(*mi_data==0x04){
	    xbee_explicitAddressingZigBee(addrRobot,spinLeft,clusterRequest);
	}
	if(*mi_data==0x05){
	    xbee_explicitAddressingZigBee(addrRobot,spinRight,clusterRequest);
	}
	if(*mi_data==0x06){
	    xbee_explicitAddressingZigBee(addrRobot,getUp,clusterRequest);
	}
	if(*mi_data==0x07){
	    xbee_explicitAddressingZigBee(addrRobot,getDown,clusterRequest);
	}
    	free(mi_data);
    }

    xbee_end_module();                     // Cierro el puerto serie.

    return 0;
}
