#include <stdlib.h>
#include <stdio.h>
#include <drivers/xbeeAPI.h>
#include <pthread.h>
#include <string.h>

/*
* 
*/
enum motion {
	stop = 0x01,
	forward = 0x02, 
	backward = 0x03, 
	spinLeft = 0x04,
	spinRight = 0x05,
	getUp = 0x06,
	getDown = 0x07,
	shoot = 0x08,
	sideLeft = 0x09,
	sideRight = 0x0A,
	fallDown = 0x0B,
	FrontWalk = 0x0C,
	backWalk = 0x0D,
	rightWalkFordward = 0x0E,
	leftWalkFordward = 0x0F,
	rightWalkBackward = 0x10,
	leftWalkBackward = 0x11,
	scanNeckOn = 0x12,
	scanNeckoff = 0x13,
	hello = 0x14
};

int main(){
    char action[1];
    unsigned char mi_data[1]; 
    char addrRobot[8]={0x00,0x12,0x4B,0x00,0x00,0x81,0x68,0x36};	//Robot 1
    unsigned char clusterRequest[2]={0xC0,0x7E};
    unsigned char profile_id[2]={0xC1,0x05};
    unsigned char source_endpoint=0xEB; 
    unsigned char destination_endpoint=0xEA;
    unsigned int mi_lenght=0;  

    struct sched_param param;

    param.sched_priority = PRIO_MED;

    if (xbee_init_module("/dev/ttyS0", "74", 115200, param, SCHED_RR)==FALSE)	// Abre el puerto serie. create()
    	exit(0);	// Configura el modulo y el puerto serie.

     while(1){
	gets(action);
	if(strcmp(action,"w")==0){
	    printf("Forward\n");
	    *mi_data=forward;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"s")==0){
	    printf("Backward\n");
	    *mi_data=backward;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"a")==0){
	    printf("Side Left\n");
	    *mi_data=sideLeft;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"d")==0){
	    printf("Side Right\n");
	    *mi_data=sideRight;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"f")==0){
	    printf("Stop\n");
	    *mi_data=stop;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"r")==0){
	    printf("Shoot\n");
	    *mi_data=shoot;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"e")==0){
	    printf("Scanneck On\n");
	    *mi_data=scanNeckOn;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"h")==0){
	    printf("Tell Hello\n");
	    *mi_data=hello;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"z")==0){
	    printf("Get Up\n");
	    *mi_data=getUp;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"x")==0){
	    printf("Get Down\n");
	    *mi_data=getDown;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"c")==0){
	    printf("Spin Left\n");
	    *mi_data=spinLeft;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
	if(strcmp(action,"v")==0){
	    printf("Spin Right\n");
	    *mi_data=spinRight;
	    xbee_explicitAddressingZigBee(addrRobot,mi_data,clusterRequest,profile_id,source_endpoint,destination_endpoint);
	}
    }
    free(mi_data);
    xbee_end_module();                     // Cierro el puerto serie.

    return 0;
}
