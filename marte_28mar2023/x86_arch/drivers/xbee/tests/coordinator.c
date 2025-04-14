#include <stdlib.h>
#include <stdio.h>
#include "xbeeAPI.h"
#include <pthread.h>
#include <string.h>

int main(){
    char action[1];
    char addr[8]={0x00,0x13,0xA2,0x00,0x40,0x78,0xF0,0x66};		//Router 1
    char addrRobot[8]={0x00,0x12,0x4B,0x00,0x00,0x81,0x68,0x36};	//Robot 1
    //char addrRobot[8]={0x00,0x12,0x4B,0x00,0x00,0x81,0x68,0x42}; 	//Robot 2
    char clusterRequest[2]={0xC0,0x7E};
    //char clusterCoord[2]={0x12,0x95};

    xbee_init_module("/dev/ttyUSB0","74",115200);	// Configura el modulo y el puerto serie.

    while(1){
	gets(action);
	if(strcmp(action,"w")==0){
	    printf("Forward\n");
	    xbee_explicitAddressingZigBee(addrRobot,forward,clusterRequest);
	    //xbee_explicitAddressingZigBee(addr,0x02,clusterRequest);
	}
	if(strcmp(action,"s")==0){
	    printf("Backward\n");
	    xbee_explicitAddressingZigBee(addrRobot,backward,clusterRequest);
	}
	if(strcmp(action,"a")==0){
	    printf("Side Left\n");
	    xbee_explicitAddressingZigBee(addrRobot,sideLeft,clusterRequest);
	}
	if(strcmp(action,"d")==0){
	    printf("Side Right\n");
	    xbee_explicitAddressingZigBee(addrRobot,sideRight,clusterRequest);
	}
	if(strcmp(action,"f")==0){
	    printf("Stop\n");
	    xbee_explicitAddressingZigBee(addrRobot,stop,clusterRequest);
	}
	if(strcmp(action,"r")==0){
	    printf("Shoot\n");
	    xbee_explicitAddressingZigBee(addrRobot,shoot,clusterRequest);
	}
	if(strcmp(action,"e")==0){
	    printf("Scanneck On\n");
	    xbee_explicitAddressingZigBee(addrRobot,scanNeckOn,clusterRequest);
	}
	if(strcmp(action,"h")==0){
	    printf("Tell Hello\n");
	    xbee_explicitAddressingZigBee(addrRobot,hello,clusterRequest);
	}
	if(strcmp(action,"z")==0){
	    printf("Get Up\n");
	    xbee_explicitAddressingZigBee(addrRobot,getUp,clusterRequest);
	}
	if(strcmp(action,"x")==0){
	    printf("Get Down\n");
	    xbee_explicitAddressingZigBee(addrRobot,getDown,clusterRequest);
	}
	if(strcmp(action,"c")==0){
	    printf("Spin Left\n");
	    xbee_explicitAddressingZigBee(addrRobot,spinLeft,clusterRequest);
	}
	if(strcmp(action,"v")==0){
	    printf("Spin Right\n");
	    xbee_explicitAddressingZigBee(addrRobot,spinRight,clusterRequest);
	}
    }

    xbee_end_module();                     // Cierro el puerto serie.

    return 0;
}

