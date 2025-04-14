#include "xbeeAPI.h"
#include <stdio.h>

int main(){
    char dir[8]={0x00,0x13,0xA2,0x00,0x40,0x0A,0x41,0x3B};
    char action[1];

    init_module("/dev/ttyUSB0", "FFFE",115200);
    while(1){
	gets(action);
    	xbee_writeTransmiteRequest(dir, action);
    }
    end_module();

    return 0;
}
