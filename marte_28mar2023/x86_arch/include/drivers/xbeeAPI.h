#ifndef _xbeeAPI_h_ 
#define _xbeeAPI_h_ 

#ifndef FALSE
   #define FALSE 0
#endif
    
#ifndef TRUE
   #define TRUE  1
#endif

#include <sched.h>

#define PRIO_HIGH 70
#define PRIO_MED  69
#define PRIO_LOW  68

//DEFINES ERRORS
#define XBEE_NO_ERROR 0
#define XBEE_FRAME_TOO_LONG 1

const char Broadcast[8]={0x00,0x00,0x00,0x00,0x00,0x00,0xFF,0xFF};
char Coordinator[8]={0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};



/*********************************************************************************
** This method is used to opening the serial port and setting the module XBee.	**
** The serial port is controlled by threads with a priority and a policy.	**
**  		    								**
** param: port, is the port's name (ttyS0).		    			**
** param: pan_id, is the Personal Area Network Identifier.  			**
** param: baudrate, is the speed we need to work between the serial port 	**
** 	  and the module XBee.		    					**
** param: thread_params, we use this parameter to specify the priority	    	**
** param: thread_sched_policy, we use this parameter to specify the policy	**
** return: fd, the serial port index						**
*********************************************************************************/
int xbee_init_module(char *port, char *pan_id, unsigned int baudrate, struct sched_param thread_params, int thread_sched_policy);

/*********************************************************************************
** This method build an AT command frame(0x08). It's used to setting or asking	**
** the configuration of the module XBee.					**
** param: command, is the parameter we want to set or ask	  		**
** param: value, is the value of the parameter we want to set. It will be NULL	**
**	  if we are asking the module what the value of the parameter is.	**
** param: lenght, is the lenght of the value.					**
** return: result, is the integer value of the character			**
*********************************************************************************/
int xbee_writeATCommand(char *command, char *value, int lenght);

/*********************************************************************************
** This method build a remote AT command frame(0x17). It's used to setting 	**
** or asking the configuration of a remote module XBee.				**
**										**
** param: dir, is the address of destination.					**
** param: command, is the parameter we want to set or ask	  		**
** param: value, is the value of the parameter we want to set. It will be NULL	**
**	  if we are asking the module what the value of the parameter is.	**
** param: lenght, is the lenght of the value.					**
** return: result, is the integer value of the character			**
*********************************************************************************/
int xbee_writeRemoteATCommand(char *dir, char *command, char *value, int lenght);

/*********************************************************************************
** This method build a ZigBee transmite request frame(0x10). It's used to  	**
** sending a message to an other module XBee, which is part of the PAN.		**
**										**
** param: dir, is the address of destination.					**
** param: data, is the message we want to send.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_writeTransmiteRequest(char *dir, char *data);

/*********************************************************************************
** This method build an Explicit Addressing ZigBee frame(0x11). It's used to  	**
** sending a character to an other module XBee, which is part of the PAN, like 	**
** we do with frame(0x10), but in this case we need to specify which 		**
** 'cluster id' and 'profile id' we are using.					**
**										**
** param: dir, is the address of destination.					**
** param: data, is the message we want to send.			  		**
** param: cluster, is the cluster id we decided to use in the transmission.	**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_explicitAddressingZigBee(unsigned char *dir, unsigned char *data, unsigned char *cluster, unsigned char *profile, unsigned char source_endpoint, unsigned char destination_endpoint);

//
int xbee_frameResponseATCommand(unsigned char *frame,int lenght);
int xbee_frameReceivePacket(unsigned char *frame,int lenght,unsigned char *data, unsigned int *data_lenght);
int xbee_frameModemStatus(unsigned char *frame, int lenght);
int xbee_frameTransmitStatus(unsigned char *frame, int lenght);
int xbee_frameNodeIdentificationIndicator(unsigned char *frame, int lenght);
int xbee_frameRemoteCommandResponse(unsigned char *frame, int lenght);
int xbee_frameIODataSampleRx(unsigned char *frame, int lenght);

/*********************************************************************************
** This method is used to closing the serial port.				**
*********************************************************************************/
int xbee_end_module();

/*********************************************************************************
** This method is used to reading the variable where we have the data 		**
** we have received.								**
*********************************************************************************/
int xbee_readData(unsigned char **mi_data);

#endif 
