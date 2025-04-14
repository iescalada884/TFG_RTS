#define ENABLE_SERIAL_PORT_EVENT

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <pthread.h>

#include <sched.h>

#include <sys/time.h>
#include <unistd.h>  /* UNIX standard function definitions. */

#define MaRTE_OS 1 // define: MaRTE or undef: Linux

#ifndef MaRTE_OS
    #include <sys/ioctl.h>
    #include <termios.h> /* POSIX terminal control definitions. */
#else
    #include <drivers/serial_port_driver.h>
    #define FIONREAD 21531
#endif

#ifndef FALSE
   #define FALSE 0
#endif
    
#ifndef TRUE
   #define TRUE  1
#endif

#define FRAME_LENGHT 200

//Definimos los erroes
#define CREATION_ERROR -3
#define SYNCH_ERROR -2
#define BUFFER_FULL -1
#define OPEN_PORT_ERROR -4
#define GETATTR_ERROR -5
#define SETATTR_ERROR -6
#define CREATE_MUTEX_ERROR -7
#define CREATE_THREAD_ERROR -8
#define WRITE_PORT_ERROR -9
#define WRITE_FRAME08_ERROR -10
#define WRITE_FRAME17_ERROR -11
#define WRITE_FRAME10_ERROR -12
#define WRITE_FRAME11_ERROR -13
#define CONFIG_MODULE_ERROR -14
#define CLOSE_PORT_ERROR -15

//Definimos los tipos de tramas API
#define ATCommand 0x08
#define TransmiteRequest 0x10
#define explicitAddressingZigBee 0x11
#define RemoteATCommand 0x17
#define ATCommandResponse 0x88

//Definimos los parametros comunes de las tramas API
#define delimiter 0x7E
#define ACK 0x01
#define ZERO 0x00

#ifdef DEBUG
    #define DEBUG_PRINT printf
#else
    #define DEBUG_PRINT
#endif

#ifdef DEBUG
    #define DEBUG_PERROR perror
#else
    #define DEBUG_PERROR
#endif

typedef int HANDLE;

int Kbhit_Port(HANDLE fd){
    int bytes;
    ioctl(fd, FIONREAD, &bytes);
    return bytes;

}

/**************************************************************************/
    #ifdef ENABLE_SERIAL_PORT_EVENT
    #include <pthread.h>

    void SERIAL_PORT_EVENT(HANDLE * hPort);

    void *Thread_Port(void *hPort)
    {
        int n=0;
        HANDLE *fd;
        fd=(HANDLE *)hPort;

        printf("SERIAL_PORT_EVENT [OK]\n");

        do {
            if(Kbhit_Port(*fd)!=0)
               SERIAL_PORT_EVENT(fd);
        } while(TRUE);
    }
  /** \fn pthread_t Create_Thread_Port(HANDLE *fd) 
   *  \brief Se usa para crear un hilo que ejecuta la función:<br>
   *         void SERIAL_PORT_EVENT(HANDLE *hPort) <br>
   *         Esta se ejecuta cuando se produce el evento de recepción de un 
   *         caracter por el puerto serie.
   *  \param fd Es el manejador del puerto serie. 
   *  \return El manejador del hilo creado.
   *  \ingroup HeaderLinux    
   */      
    pthread_t Create_Thread_Port(HANDLE *fd, struct sched_param thread_params, int thread_sched_policy) 
    {
	pthread_t idHilo;
	#ifdef MaRTE_OS
	    pthread_attr_t attr;
	    pthread_attr_init (&attr);
	    pthread_attr_setinheritsched (&attr, PTHREAD_EXPLICIT_SCHED);
	    pthread_attr_setschedpolicy (&attr, thread_sched_policy);
	    pthread_attr_setschedparam (&attr, &thread_params);
	    if(pthread_create (&idHilo, &attr, Thread_Port, fd)!=0){
	    	printf("Error al crear el thread\n");
	    	return FALSE;
	    }
	#else
	    if(pthread_create (&idHilo, NULL, Thread_Port, fd)!=0){
	    	printf("Error al crear el thread\n");
	    	return FALSE;
	    }
        #endif
        
        return idHilo;
    }
    #endif
/********************************************************************/


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
int fd;				//The serial port index

#ifdef MaRTE_OS			//If MaRTE_OS is defined
    termios_t OldConf;		//we use termios_t of serial_port_driver.h
#else				//Else
    struct termios OldConf;	//We use struct termios of termios.h
#endif
int xbee_init_module(char *port, char *pan_id, unsigned int baudrate, struct sched_param thread_params, int thread_sched_policy){

    //Opening the serial port
    fd=open(port, O_RDWR | O_NOCTTY);

    if (fd==-1) {
      DEBUG_PERROR("Error al abrir el fichero");
      return OPEN_PORT_ERROR;
    }

    //If MaRTE_OS is defined, we set the serial port for MaRTE
    #ifdef MaRTE_OS 
    	termios_t newtio;
	//Store the default configuration
    	ioctl(fd,SERIAL_GETATTR,((void *)&newtio));
    	OldConf=newtio;

	// Set new serial port. 8N1 B115200
    	newtio.cflag =CLOCAL | CREAD ;

    	newtio.ospeed = B115200;
    	newtio.ispeed = B115200;

    	newtio.cflag &= ~PARENB;
    	newtio.cflag &= ~CSTOPB;
    	newtio.cflag &= ~CSIZE;
    	newtio.cflag |= CS8;

    	newtio.iflag = 0;
    	newtio.oflag = 0;
    	newtio.lflag = 0;

	//Timer between characters is not used
    	newtio.cc[VTIME]    = 0;
	//Stop reading until the new character arrival
    	newtio.cc[VMIN]     = 1;
   
	//Save the new configuration
    	ioctl(fd,SERIAL_SETATTR,((void *)&newtio));
    #else //If is not defined MaRTE_OS, we set the serial port for Linux
    	struct termios newtio;
	//Get the configuration
    	if(tcgetattr(fd,&OldConf)!=0)
    	{
	    printf("Error pidiendo la configuración de puerto serie.\n");
	    return GETATTR_ERROR;
    	}

	//Set the new configuration
    	bzero(&newtio, sizeof(newtio));
    	newtio.c_cflag =CLOCAL | CREAD ;
	
    	//cfsetispeed(&newtio,baudrate);
    	//cfsetospeed(&newtio,baudrate);
    	cfsetispeed(&newtio,B115200);
    	cfsetospeed(&newtio,B115200);

    	newtio.c_cflag &= ~PARENB;
    	newtio.c_cflag &= ~CSTOPB;
    	newtio.c_cflag &= ~CSIZE;
    	newtio.c_cflag |= CS8;

    	newtio.c_iflag = 0;
    	newtio.c_oflag = 0;
    	newtio.c_lflag = 0;

	//Timer between characters is not used
    	newtio.c_cc[VTIME]    = 0;
	//Stop reading until the new character arrival
    	newtio.c_cc[VMIN]     = 1;

	//Save the new configuration
    	if(tcsetattr(fd,TCSANOW,&newtio)!=0)
    	{
	    printf("ERROR: No se pudo poner la configuración del puerto serie\n" );
	    return SETATTR_ERROR;
    	}
    #endif

    //we create the mutex and the conditional variable
    if(create()==CREATION_ERROR){
	printf("Error al crear el mutex\n");
	return CREATE_MUTEX_ERROR;
    }
    //we create the serial port thread with a priority and policy
    if(Create_Thread_Port(&fd,thread_params,thread_sched_policy)==FALSE){
	printf("Error al crear el thread del puerto serie\n");
	return CREATE_THREAD_ERROR;
    }

    //Setting the module XBee
    xbee_config_module(pan_id, baudrate);

    DEBUG_PRINT("Fichero abierto\n");

    return fd;
}

/*********************************************************************************
** This method is used to writing a frame in the serial port 	    		**
** param: frame, is the frame API we want to send	    			**
** param: lenght, is the lenght of the frame		  			**
** return: TRUE, if everything was right					**
*********************************************************************************/
int xbee_write_port(unsigned char *frame, int lenght){

    if(write(fd,frame,lenght)==-1)
	return WRITE_PORT_ERROR;

    return TRUE;
}

/*********************************************************************************
** This method gives integer value of a character	 	    		**
** param: c, is the character				  			**
** return: result, is the integer value of the character			**
*********************************************************************************/
int hex_to_int(char c){
        int first = c / 16 - 3;
        int second = c % 16;
        int result = first*10 + second;
        if(result > 9) result--;

        return result;
}

/*********************************************************************************
** This method gives ascii value of two characters	 	    		**
** param: c and d, are characters			  			**
** return: hifh+low, is the ascii value of both characters			**
*********************************************************************************/
int hex_to_ascii(char c, char d){
        int high = hex_to_int(c) * 16;
        int low = hex_to_int(d);
        return high+low;
}

/*********************************************************************************
** We use this method when we want to set the baudrate. Each index is    	**
** representing a baudrate.							**
**										**
** param: baudrate, is the baudrate we want to use.				**
** param: indexBaudrate, is the index which represent the baudrate.  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_getIndexBaudrate(unsigned int baudrate, unsigned char *indexBaudrate){
    int baudios[8]={1200,2400,4800,9600,19200,38400,57600,115200};

    printf("Valor--> %d",baudrate);
    switch(baudrate){
	case 1200:
		*indexBaudrate=0x00;
		break;
	case 2400:
		*indexBaudrate=0x01;
		break;
	case 4800:
		*indexBaudrate=0x02;
		break;
	case 9600:
		*indexBaudrate=0x03;
		break;
	case 19200:
		*indexBaudrate=0x04;
		break;
	case 38400:
		*indexBaudrate=0x05;
		break;
	case 57600:
		*indexBaudrate=0x06;
		break;
	case 115200:
		*indexBaudrate=0x07;
		break;
	default:
		*indexBaudrate=0x00;
		break;
    }

    DEBUG_PRINT("Baudios--> %s\n",indexBaudrate);
    return TRUE;
}

/*********************************************************************************
** This method becomes a string into integer		 	    		**
** param: baudrate, is the string			  			**
** return: the integer								**
*********************************************************************************/
int xbee_BaudrateParseToInt(unsigned char *baudrate){
    char *ep;
    long l;
    unsigned int i;
    unsigned char indexBaudrate;

    printf("baud-->%s",baudrate);
    l=strtol(baudrate,&ep,0);

    i=(int)l;
    printf("i--> %d\n",i);
    xbee_getIndexBaudrate(i,&indexBaudrate);
    printf("index-->%d\n",indexBaudrate);

    return indexBaudrate;
}

/*********************************************************************************
** This method build an AT command frame(0x08). It's used to setting or asking	**
** the configuration of the module XBee.					**
** param: command, is the parameter we want to set or ask	  		**
** param: value, is the value of the parameter we want to set. It will be NULL	**
**	  if we are asking the module what the value of the parameter is.	**
** param: lenght, is the lenght of the value.					**
** return: result, is the integer value of the character			**
*********************************************************************************/
int xbee_writeATCommand(char *command, char *value, int lenght){
    unsigned char *frame,*value1;
    int i,cont=(lenght/2);
    int cociente=0, resto=0, resto1=0;

    //we keep the memory we will use
    frame=(char *)calloc((8+(lenght-cont)),sizeof(char));

    //Start creating the frame 
    frame[0]=delimiter;
    frame[1]=ZERO;
    frame[3]=ATCommand;
    frame[4]=ACK;
    frame[5]=*command;
    frame[6]=*(command+1);

    //If we want to modify some parameter
    if(value!=NULL){
	//The parameters ID, DH and DL have a specific process. We want to have two numers represented in one byte
	if((strcmp(command,"ID")==0) || (strcmp(command,"DH")==0) || (strcmp(command,"DL")==0) ||
	   (strcmp(command,"id")==0) || (strcmp(command,"dh")==0) || (strcmp(command,"dl")==0)){
	    //If the lenght is odd
	    if((lenght%2)!=0){
		//we keep memory for the value +1 to get a pair lenght
	    	value1=(char *)calloc((lenght+1),sizeof(char));
		//we make sure we have an 0 at the begining
	    	strcpy(value1,"0");
		//we concatenate the value
	    	strcat(value1,value);
	    	lenght++;
	    	cont=lenght/2;
	    }else{//If the lenght is pair we dont do anything
	    	value1=value;
	    }
	    //we check if the lenght of the frame can be represented with one byte or we need to use two
	    if((lenght-(255-0x04))>0){
	    	frame[1]=(lenght-cont)-(255-0x04);
	    	frame[2]=0xFF;
    	    }else{
	    	frame[2]=0x04+(lenght-cont);
	    }
	    //Now, we write the value
	    for(i=lenght-1;i>=0;i-=2){
		if(i>cont){
		    frame[7+cont-1]=hex_to_ascii(*(value1+i-1), (*(value1+i)));
	    	    DEBUG_PRINT("F[%d]-->%02x\n",7+cont-1,frame[7+cont-1]);
		}else{
		    if(i==cont){
			frame[7+cont-1]=hex_to_ascii(*(value1+i-1), (*(value1+i)));
			DEBUG_PRINT("F[%d]-->%02x\n",7+cont-1,frame[7+cont-1]);
		    }
		}
		cont--;
	    }
	    //Finnaly we calculate the checksum
	    cont=(lenght/2);
	    for(i=0;i<(4+(lenght-cont));i++){
    	    	frame[7+(lenght-cont)]+=frame[i+3];
    	    }
	    frame[7+(lenght-cont)]=0xFF-frame[7+(lenght-cont)];
	}else{//If the command isn't ID, DH, DL then
	    if((strcmp(command,"BD") || strcmp(command,"bd")) && lenght>1){
		lenght=1;
		value1=(char *)calloc((lenght),sizeof(char));
		*value1=xbee_BaudrateParseToInt(value);
		frame[2]=0x04+lenght;
		frame[7]=*value1;
		for(i=0;i<(4+lenght);i++){
    	    	    frame[7+lenght]+=frame[i+3];
    	    	}
	    	frame[7+lenght]=0xFF-frame[7+lenght];
    	    }else{
	    	if((lenght-(255-0x04))>0){
	    	    frame[1]=lenght-(255-0x04);
	    	    frame[2]=0xFF;
    	    	}else{
	    	    frame[2]=0x04+lenght;
	    	}

	    	for(i=0;i<lenght;i++){
	    	    frame[7+i]=*(value+i);
	    	    DEBUG_PRINT("%x\n",frame[7+i]);
	    	}
	
	    	for(i=0;i<(4+lenght);i++){
    	    	    frame[7+lenght]+=frame[i+3];
    	    	}
	    	frame[7+lenght]=0xFF-frame[7+lenght];
	    }
	}
    }else{//If there isn't a value because we want to ask for one then 
	frame[2]=0x04;
	frame[7]=0xFF-(frame[3]+frame[4]+frame[5]+frame[6]);
    }

    DEBUG_PRINT("Frame-->");
    for(i=0;i<(8+lenght);i++){
	DEBUG_PRINT("%02x",frame[i]);
    }
    DEBUG_PRINT("\n");

    //Sending the frame
    if(xbee_write_port(frame,(8+lenght))==WRITE_PORT_ERROR)
	return WRITE_FRAME08_ERROR;

    return TRUE;
}
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
int xbee_writeRemoteATCommand(char *dir, char *command, char *value, int lenght){
    unsigned char *frame,*value1;
    int i,cont=(lenght/2);
    int cociente=0, resto=0, resto1=0;

    //we keep the memory we will use
    frame=(char *)calloc((19+lenght),sizeof(char));
    //Start creating the frame
    frame[0]=delimiter;
    frame[1]=ZERO;
    frame[3]=RemoteATCommand;
    frame[4]=ACK;
    frame[5]=*dir;
    frame[6]=*(dir+1);
    frame[7]=*(dir+2);
    frame[8]=*(dir+3);
    frame[9]=*(dir+4);
    frame[10]=*(dir+5);
    frame[11]=*(dir+6);
    frame[12]=*(dir+7);
    frame[13]=0xFF;
    frame[14]=0xFE;
    frame[15]=0x02;
    frame[16]=*command;
    frame[17]=*(command+1);

    //If we want to modify some parameter
    if(value!=NULL){
	//The parameters ID, DH and DL have a specific process. We want to have two numers represented in one byte
	if((strcmp(command,"ID")==0) || (strcmp(command,"DH")==0) || (strcmp(command,"DL")==0) ||
	   (strcmp(command,"id")==0) || (strcmp(command,"dh")==0) || (strcmp(command,"dl")==0)){
	    //If the lenght is odd
	    if((lenght%2)!=0){
		//we keep memory for the value +1 to get a pair lenght
	    	value1=(char *)calloc((lenght+1),sizeof(char));
		//we make sure we have an 0 at the begining
	    	strcpy(value1,"0");
		//we concatenate the value
	    	strcat(value1,value);
	    	lenght++;
	    	cont=lenght/2;
	    }else{//If the lenght is pair we dont do anything
	    	value1=value;
	    }
	    //we check if the lenght of the frame can be represented with one byte or we need to use two
	    if((lenght-(255-0x0F))>0){
	    	frame[1]=(lenght-cont)-(255-0x0F);
	    	frame[2]=0xFF;
    	    }else{
    	    	frame[2]=0x0F+(lenght-cont);
    	    }
	    //Now, we write the value
	    for(i=lenght-1;i>=0;i-=2){
		if(i>cont){
		    frame[18+cont-1]=hex_to_ascii(*(value1+i-1), (*(value1+i)));
	    	    DEBUG_PRINT("F[%d]-->%02x\n",18+cont-1,frame[18+cont-1]);
		}else{
		    if(i==cont){
			frame[18+cont-1]=hex_to_ascii(*(value1+i-1), (*(value1+i)));
			DEBUG_PRINT("F[%d]-->%02x\n",18+cont-1,frame[18+cont-1]);
		    }
		}
		cont--;
	    }
	    //Finnaly we calculate the checksum
	    cont=(lenght/2);
	    for(i=0;i<(15+(lenght-cont));i++){
    	    	frame[18+(lenght-cont)]+=frame[i+3];
    	    }
	    frame[18+(lenght-cont)]=0xFF-frame[18+(lenght-cont)];
	}else{//If the command isn't ID, DH, DL then
	    if((strcmp(command,"BD") || strcmp(command,"bd")) && lenght>1){
		lenght=1;
		value1=(char *)calloc((lenght),sizeof(char));
		*value1=xbee_BaudrateParseToInt(value);
		frame[2]=0x0F+lenght;
		frame[18]=*value1;
		for(i=0;i<(15+lenght);i++){
    	    	    frame[18+lenght]+=frame[i+3];
    	    	}
	    	frame[18+lenght]=0xFF-frame[18+lenght];
    	    }else{
	    	if((lenght-(255-0x0F))>0){
	    	    frame[1]=lenght-(255-0x0F);
	    	    frame[2]=0xFF;
    	    	}else{
    	    	    frame[2]=0x0F+lenght;
    	    	}

	    	for(i=0;i<lenght;i++){
	    	    frame[18+i]=*(value+i);
	    	}

	    	for(i=0;i<(15+lenght);i++){
    	    	    frame[18+lenght]+=frame[i+3];
    	    	}
	    	frame[18+lenght]=0xFF-frame[18+lenght];
	    }
	}
    }else{//If there isn't a value because we want to ask for one then
	frame[2]=0x0F;
	for(i=0;i<15;i++){
    	    frame[18]+=frame[i+3];
    	}
    	frame[18]=0xFF-frame[18];
    }

    DEBUG_PRINT("Frame-->");
    printf("Frame-->");
    for(i=0;i<strlen(frame);i++){
	DEBUG_PRINT("%02x",frame[i]);
	printf("%02x",frame[i]);
    }
    DEBUG_PRINT("\n");
    printf("\n");

    //Sending the frame
    if(xbee_write_port(frame,(19+lenght))==WRITE_PORT_ERROR)
	return WRITE_FRAME17_ERROR;

    return TRUE;
}

/*********************************************************************************
** This method build a ZigBee transmite request frame(0x10). It's used to  	**
** sending a message to an other module XBee, which is part of the PAN.		**
**										**
** param: dir, is the address of destination.					**
** param: data, is the message we want to send.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_writeTransmiteRequest(char *dir, char *data){
    unsigned char *frame;
    int lenght,i;

    strcat(data,"\0");
    lenght=strlen(data);
    //we keep the memory we will use
    frame=(char *)calloc((18+lenght),sizeof(char));

    //start creating the frame
    frame[0]=delimiter;
    //checking how many bytes we will need to
    if((lenght-(255-0x0E))>0){
	frame[1]=lenght-(255-0x0E);
	frame[2]=0xFF;
    }else{
    	frame[1]=ZERO;
    	frame[2]=(0x0E)+lenght;
    }
    frame[3]=TransmiteRequest;
    frame[4]=ACK;
    frame[5]=*dir;		//addr64 destination address
    frame[6]=*(dir+1);
    frame[7]=*(dir+2);
    frame[8]=*(dir+3);
    frame[9]=*(dir+4);
    frame[10]=*(dir+5);
    frame[11]=*(dir+6);
    frame[12]=*(dir+7);
    frame[13]=0xFF;		//addr16 will be FFFE when it's unknown
    frame[14]=0xFE;
    frame[15]=0x00;
    frame[16]=0x00;
    //writing the message
    for(i=0;i<lenght;i++){
	frame[17+i]=*(data+i);
    }
    //Calculating the checksum
    for(i=0;i<(14+lenght);i++){
    	frame[17+lenght]+=frame[i+3];
    }
    frame[17+lenght]=0xFF-frame[17+lenght];

    for(i=0;i<strlen(frame);i++){
	DEBUG_PRINT(" %3d: 0x%02X %c\n", i, frame[i],((frame[i] >= ' ' && frame[i] <= '~') ? frame[i] : '.'));
    }

    //Sending the frame
    if(xbee_write_port(frame,(18+lenght))==WRITE_PORT_ERROR)
	return WRITE_FRAME10_ERROR;

    return TRUE;
}

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
int xbee_explicitAddressingZigBee(unsigned char *dir, unsigned char *data, unsigned char *cluster, unsigned char *profile, unsigned char source_endpoint, unsigned char destination_endpoint){
    unsigned char *frame;
    int lenght,i;

    lenght=strlen(data);
    DEBUG_PRINT("longi-->%d\tdata-->%c\n",lenght,data);
    //we keep the memory we will use
    frame=(char *)calloc((24+lenght),sizeof(char));
    //Creating the frame
    frame[0]=delimiter;
    //checking how many bytes we will need to
    if((lenght-(255-0x14))>0){
	frame[1]=lenght-(255-0x14);
	frame[2]=0xFF;
    }else{
    	frame[1]=ZERO;
    	frame[2]=(0x14)+lenght;
    }
    frame[3]=explicitAddressingZigBee;
    frame[4]=ACK;
    frame[5]=*dir;
    frame[6]=*(dir+1);
    frame[7]=*(dir+2);
    frame[8]=*(dir+3);
    frame[9]=*(dir+4);
    frame[10]=*(dir+5);
    frame[11]=*(dir+6);
    frame[12]=*(dir+7);
    frame[13]=0xFF;
    frame[14]=0xFE;
    frame[15]=source_endpoint; //0xEB;	//Source endpoint
    frame[16]=destination_endpoint; //0xEA;	//Destination endpoint
    frame[17]=*cluster;
    frame[18]=*(cluster+1);
    frame[19]=*profile; //0xC1;		//Profile id C105
    frame[20]=*(profile+1); //0x05;
    frame[21]=0x00;		//BroadcastRaius
    frame[22]=0x00;		//Enable APS encryption (if EE=1)
    //frame[23]=data;		
    //writing the message
    for(i=0;i<lenght;i++){
	frame[23+i]=*(data+i);
    }
    
    //Checkcum
    for(i=0;i<(20+lenght);i++){
    	frame[23+lenght]+=frame[i+3];
    }
    frame[23+lenght]=0xFF-frame[23+lenght];

    for(i=0;i<strlen(frame);i++){
	DEBUG_PRINT(" %3d: 0x%02X %c\n", i, frame[i],((frame[i] >= ' ' && frame[i] <= '~') ? frame[i] : '.'));
    }

    /*printf("Frame-->");
    for(i=0;i<(24+lenght);i++){
	printf("%02x",frame[i]);
    }
    printf("\n");*/

    //Sending the frame
    if(xbee_write_port(frame,(24+lenght))==WRITE_PORT_ERROR)
	return WRITE_FRAME11_ERROR;

    return TRUE;
}

/*********************************************************************************
** We use this method to set the module XBee in default mode. If we want to    	**
** change the configuration we will need to call 'xbee_writeATCommand' function.**
**										**
** param: pan_id, is the pan identify we want to use.				**
** param: indexBaudrate, is the index which represent the baudrate.  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_config_module(char *pan_id, unsigned int baudrate){
    unsigned char *indexBaudrate;

    strcpy(indexBaudrate,"0");
    xbee_getIndexBaudrate(baudrate,indexBaudrate);

    if(xbee_writeATCommand("ID",pan_id,strlen(pan_id))==WRITE_FRAME08_ERROR)
	return CONFIG_MODULE_ERROR;

    if(xbee_writeATCommand("BD",indexBaudrate,1)==WRITE_FRAME08_ERROR)
	return CONFIG_MODULE_ERROR;

    if(xbee_writeATCommand("WR",NULL,0)==WRITE_FRAME08_ERROR)
	return CONFIG_MODULE_ERROR;

    return TRUE;
}

/*********************************************************************************
** This method prints the status of the AT command frame that we have sent.	**
**										**
** param: status, is the status of the AT command frame.			**
*********************************************************************************/
void xbee_statusframeResponseATCommand(unsigned char status){
    switch(status){
	case 0x00:
		printf("Status frame--> Ok\n");
		break;
	case 0x01:
		printf("Status frame--> ERROR\n");
		break;
	case 0x02:
		printf("Status frame--> Invalid command\n");
		break;
	case 0x03:
		printf("Status frame--> Invalid parameter\n");
		break;
	case 0x04:
		printf("Status frame--> Tx failure\n");
		break;
	default:
		printf("Status frame--> Error unknown");
		break;
    }
}

/*********************************************************************************
** This method is called when a Response AT Command frame (0x88) is received. 	**
** The method checks the frame and gets the information.			**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_frameResponseATCommand(unsigned char *frame,int lenght){
    unsigned char commando[2];
    unsigned char statu;
    unsigned char *dat;
    int i,data_lenght;
    
    //Getting the lenght of the data
    data_lenght=lenght-8;
    //keeping the memory we will use
    dat=(char *)calloc(data_lenght,sizeof(char));

    //Getting the data
    frame--;
    for(i=(data_lenght-1);i>=0;i--){
	*(dat+i)=*frame;
	frame--;
    }

    //Getting the status of the frame
    statu=*frame;
    frame--;

    //Getting the command of the frame
    for(i=1;i>=0;i--){
	*(commando+i)=*frame;
	frame--;
    }

    //Printing the status on the console
    xbee_statusframeResponseATCommand(statu);

    DEBUG_PRINT("command--> %s\n",commando);
    DEBUG_PRINT("status--> %02x\n",statu);
    printf("(%c%c)--> ",commando[0],commando[1]);
    if(data_lenght>0){
    	for(i=0;i<data_lenght;i++){
    	    DEBUG_PRINT("%02x",*(dat+i));
	    printf("%02x",*(dat+i));
    	}
    	DEBUG_PRINT("\n");
	printf("\n");
    }
    //We free the space of the memory we kept before
    free(dat);

    return TRUE;
}
/*********************************************************************************
** This method is called when a Receive Packet frame (0x90) is received.	**
** The method checks the frame and gets the information.			**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** param: data, is where we will copy the message tha we have received.		**
** param: data_lenght, is where we will copy the lenght of the message.		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_frameReceivePacket(unsigned char *frame,int lenght,unsigned char *data, unsigned int *data_lenght){
    unsigned char dire[8];
    unsigned char dire16[2];
    int i=0;

    //Getting the lenght of the message
    *data_lenght=lenght-15;   

    //Getting the message
    frame--;
    for(i=(*data_lenght-1);i>=0;i--){
	*(data+i)=*frame;
	frame--;
    }

    frame--;

    //Getting the 16 bits address
    for(i=1;i>=0;i--){
	dire16[i]=*frame;
	frame--;
    }

    //Getting the 64bits address
    for(i=7;i>=0;i--){
	dire[i]=*frame;
	frame--;
    }

    //Notifying we have already received the data
    setReceived(TRUE);

    DEBUG_PRINT("Sender address-->");
    for(i=0;i<8;i++){
	DEBUG_PRINT("%02x",dire[i]);
    }

    DEBUG_PRINT("\nSender address16-->");
    for(i=0;i<2;i++){
	DEBUG_PRINT("%02x",dire16[i]);
    }

    DEBUG_PRINT("\nReceived Data-->");
    printf("msg--> ");
    for(i=0;i<*data_lenght;i++){
	DEBUG_PRINT("%c",data[i]);
	printf("%c",data[i]);
    }
    DEBUG_PRINT("\n");
    printf("\n");

    return TRUE;
}

/*********************************************************************************
** This method prints the modem status.						**
**										**
** param: status, is the status of the modem.					**
*********************************************************************************/
void xbee_printStatusFrameModemStatus(unsigned char status){
    switch(status){
	case 0x00:
		printf("Hardware reset\n");
		break;
	case 0x01:
		printf("Watchdog timer reset\n");
		break;
	case 0x02:
		printf("Joined network\n");
		break;
	case 0x03:
		printf("Disassociated\n");
		break;
	case 0x06:
		printf("Coordinator started\n");
		break;
	case 0x07:
		printf("Network security key was updated\n");
		break;
	case 0x0D:
		printf("Voltage supply limit exceeded\n");
		break;
	case 0x11:
		printf("Modem configuration changed while join in progress\n");
		break;
	case 0x80:
		printf("Stack error\n");
		break;
	default:
		printf("Error unknown\n");
		break;
    }
}

/*********************************************************************************
** This method is called when a Modem Status frame (0x8A) is received.		**
** The method checks the frame and gets the information.			**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_frameModemStatus(unsigned char *frame, int lenght){
    unsigned char statu;

    frame--;
    //Getting the status of the frame
    statu=*frame;

    //printing the status 
    xbee_printStatusFrameModemStatus(statu);
    DEBUG_PRINT("Status--> %02x\n",statu);

    return TRUE;
}

/*********************************************************************************
** This method prints the delivery status of the transmition.			**
**										**
** param: status, is the status of the modem.					**
*********************************************************************************/
void xbee_deliveryFrameTransmitStatus(unsigned char status){
    switch(status){
	case 0x00:
		printf("Delivery status--> Success\n");
		break;
	case 0x01:
		printf("Delivery status--> MAC ACK failure\n");
		break;
	case 0x02:
		printf("Delivery status--> CCA failure\n");
		break;
	case 0x15:
		printf("Delivery status--> Invalid destination endpoint\n");
		break;
	case 0x21:
		printf("Delivery status--> Network ACK failure\n");
		break;
	case 0x22:
		printf("Delivery status--> Not joined to network\n");
		break;
	case 0x23:
		printf("Delivery status--> Self-addressed\n");
		break;
	case 0x24:
		printf("Delivery status--> Address not found\n");
		break;
	case 0x25:
		printf("Delivery status--> Router not found\n");
		break;
	case 0x26:
		printf("Delivery status--> Boadcast source failed to hear a neighbor relay the message\n");
		break;
	case 0x2B:
		printf("Delivery status--> Invalid binding tabled index\n");
		break;
	case 0x2C:
		printf("Delivery status--> Resource error lack of free buffers, timers, etc\n");
		break;
	case 0x2D:
		printf("Delivery status--> Attempted broadcast with APS transmission\n");
		break;
	case 0x2E:
		printf("Delivery status--> Attempted unicast with APS transformassion, but EE=0\n");
		break;
	case 0x32:
		printf("Delivery status--> Resource error lack of free buffers, timers, etc\n");
		break;
	case 0x74:
		printf("Delivery status--> Data payload too large\n");
		break;
	case 0x75:
		printf("Delivery status--> Indirect message unrequested\n");
		break;
	default:
		printf("Error unknown\n");
		break;
    }
}

/*********************************************************************************
** This method prints the discovery status of the transmition.			**
**										**
** param: status, is the status of the modem.					**
*********************************************************************************/
void xbee_discoveryFrameTransmitStatus(unsigned char status){
    switch(status){
	case 0x00:
		printf("Discovery status--> Not discovery overhead\n");
		break;
	case 0x01:
		printf("Discovery status--> Address discovery\n");
		break;
	case 0x02:
		printf("Discovery status--> Route discovery\n");
		break;
	case 0x03:
		printf("Discovery status--> Address and route\n");
		break;
	case 0x40:
		printf("Discovery status--> Extended timeout discovery\n");
		break;
	default:
		printf("Error unknown");
		break;
    }
}

/*********************************************************************************
** This method is called when a Transmit Status frame (0x8B) is received.	**
** The method checks the frame and gets the information.			**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_frameTransmitStatus(unsigned char *frame, int lenght){
    unsigned char dire16[2];
    unsigned char deliveryStatu;
    unsigned char discoveryStatu;
    int i=0;

    frame--;
    //Getting the discovery status
    discoveryStatu=*frame;
    frame--;
    //Getting the delivery status
    deliveryStatu=*frame;

    frame=frame-2;
    //Getting the 16bits address
    for(i=1;i>=0;i--){
	dire16[i]=*frame;
	frame--;
    }

    //printing both statuses
    xbee_deliveryFrameTransmitStatus(deliveryStatu);
    xbee_discoveryFrameTransmitStatus(discoveryStatu);

    DEBUG_PRINT("\nSender address16-->");
    for(i=0;i<strlen(dire16);i++){
	DEBUG_PRINT("%02x",dire16[i]);
    }

    return TRUE;
}

/*********************************************************************************
** This method is called when a Node Identification Indicator frame (0x95) 	**
** is received.	The method checks the frame and gets the information.		**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_frameNodeIdentificationIndicator(unsigned char *frame, int lenght){
    unsigned char dire[8];
    unsigned char dire16[2];
    unsigned char *data;
    unsigned int data_lenght;
    int i=0;

    data_lenght=lenght-35;
    data=(char *)calloc(data_lenght,sizeof(char));
        
    frame=frame-10;
    //Getting the data
    for(i=(data_lenght-1);i>=0;i--){
	*(data+i)=*frame;
	frame--;
    }
    frame=frame-12;
    //Getting the 16bits address
    for(i=1;i>=0;i--){
	dire16[i]=*frame;
	frame--;
    }
    //Getting the 64bits address
    for(i=7;i>=0;i--){
	dire[i]=*frame;
	frame--;
    }

    DEBUG_PRINT("Sender address-->");
    for(i=0;i<8;i++){
	DEBUG_PRINT("%02x",dire[i]);
    }

    DEBUG_PRINT("\nSender address16-->");
    for(i=0;i<2;i++){
	DEBUG_PRINT("%02x",dire16[i]);
    }

    DEBUG_PRINT("\nData-->");
    for(i=0;i<data_lenght;i++){
	DEBUG_PRINT("%c",data[i]);
    }
    DEBUG_PRINT("\n");

    free(data);

    return TRUE;
}

/*********************************************************************************
** This method prints the status of the remote command response.		**
**										**
** param: status, is the status of the frame.					**
*********************************************************************************/
void xbee_statusframeRemoteCommandResponse(unsigned char status){
    switch(status){
	case 0x00:
		printf("Status frame remote--> Ok\n");
		break;
	case 0x01:
		printf("Status frame remote--> ERROR\n");
		break;
	case 0x02:
		printf("Status frame remote--> Invalid command\n");
		break;
	case 0x03:
		printf("Status frame remote--> Invalid parameter\n");
		break;
	case 0x04:
		printf("Status frame remote--> Remote command transmission failed\n");
		break;
	default:
		printf("Status frame remote--> Error unknown");
		break;
    }
}

/*********************************************************************************
** This method is called when a Remote Command Response frame (0x97)	 	**
** is received.	The method checks the frame and gets the information.		**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_frameRemoteCommandResponse(unsigned char *frame, int lenght){
    unsigned char addr[8];
    unsigned char addr16[2];
    unsigned char *data;
    unsigned char command[2];
    unsigned char status;
    unsigned int data_lenght;
    int i=0;

    data_lenght=lenght-18;
    data=(char *)calloc(data_lenght,sizeof(char));

    frame--;
    //Getting the data
    for(i=(data_lenght-1);i>=0;i--){
	*(data+i)=*frame;
	frame--;
    }

    //Getting the status
    status=*frame;

    frame--;
    for(i=1;i>=0;i--){
	command[i]=*frame;
	frame--;
    }
    //Getting the 16bits address
    for(i=1;i>=0;i--){
	addr16[i]=*frame;
	frame--;
    }
    //Getting the 64bits address
    for(i=7;i>=0;i--){		//Obtenemos la direccion de 64bits
	addr[i]=*frame;
	frame--;
    }
    //printing the status
    xbee_statusframeRemoteCommandResponse(status);

    DEBUG_PRINT("Sender address-->");
    for(i=0;i<8;i++){
	DEBUG_PRINT("%02x",addr[i]);
    }

    DEBUG_PRINT("\nSender address16-->");
    for(i=0;i<2;i++){
	DEBUG_PRINT("%02x",addr16[i]);
    }

    DEBUG_PRINT("\ncommand--> %s\n",command);
    DEBUG_PRINT("status--> %02x",status);

    DEBUG_PRINT("\nData-->");
    printf("(%c%c)--> ",command[0],command[1]);
    for(i=0;i<data_lenght;i++){
	DEBUG_PRINT("%02x",data[i]);
	printf("%02x",data[i]);
    }
    DEBUG_PRINT("\n");
    printf("\n");

    return TRUE;
}

/*********************************************************************************
** This method is called when an IO Data Sample Rx frame (0x92)	is received.	**
** The method checks the frame and gets the information.			**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_frameIODataSampleRx(unsigned char *frame, int lenght){
    unsigned char addr[8];
    unsigned char addr16[2];
    unsigned char analogSample[2];
    unsigned char digitalSample[2];
    int numSamples=0, i;

    frame--;
    //Getting the analog sample
    for(i=1;i>=0;i--){
	analogSample[i]=*frame;
	frame--;
    }

    //Getting the digital sample
    for(i=1;i>=0;i--){
	digitalSample[i]=*frame;
	frame--;
    }
    
    frame=frame-3;
    //Getting the number of samples
    numSamples=(int)*frame;
    
    frame=frame-2;
    //Getting the 16 bits address
    for(i=1;i>=0;i--){
	addr16[i]=*frame;
	frame--;
    }
    //Getting the 64 bits address
    for(i=7;i>=0;i--){
	addr[i]=*frame;
	frame--;
    }

    DEBUG_PRINT("Sender address-->");
    for(i=0;i<8;i++){
	DEBUG_PRINT("%02x",addr[i]);
    }

    DEBUG_PRINT("\nSender address16-->");
    for(i=0;i<2;i++){
	DEBUG_PRINT("%02x",addr16[i]);
    }

    DEBUG_PRINT("\nNumber of samples--> %d\n", numSamples);

    DEBUG_PRINT("Digital samples-->");
    for(i=0;i<2;i++){
	DEBUG_PRINT("%02x",digitalSample[i]);
    }

    DEBUG_PRINT("\nAnalog samples-->");
    for(i=0;i<2;i++){
	DEBUG_PRINT("%02x",analogSample[i]);
    }
    DEBUG_PRINT("\n");

    return TRUE;
}

/*********************************************************************************
** This method is called when an Explicit Rx Indicator frame (0x91) is received.**
** The method checks the frame and gets the information.			**
**										**
** param: frame, is the frame received on the serial port.			**
** param: lenght, is the lenght of the frame.			  		**
** param: data, is where we will copy the message tha we have received.		**
** param: data_lenght, is where we will copy the lenght of the message.		**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_ExplicitRxIndicator(unsigned char *frame, int lenght,unsigned char *data, unsigned int *data_lenght){
    unsigned char dire[8];
    unsigned char dire16[2];
    int i=0;

    *data_lenght=lenght-21;   

    frame--;
    //Getting the data
    for(i=(*data_lenght-1);i>=0;i--){
	*(data+i)=*frame;
	frame--;
    }

    frame=frame-7;
    //Getting the 16 bits address
    for(i=1;i>=0;i--){
	dire16[i]=*frame;
	frame--;
    }
    //Getting the 64 bits address
    for(i=7;i>=0;i--){
	dire[i]=*frame;
	frame--;
    }
    //Notifying we have already received the data
    setReceived(TRUE);

    DEBUG_PRINT("\nReceived Data-->");
    printf("msg--> ");
    for(i=0;i<*data_lenght;i++){
	DEBUG_PRINT("%c",data[i]);
	printf("%c",data[i]);
    }
    DEBUG_PRINT("\n");
    printf("\n");

    return TRUE;
}

/*********************************************************************************
** This method is used to knowing how big the frame received is.		**
**										**
** param: tyoeFrame, is type of frame we have received.				**
** param: lenght1, is the first byte where we found the lenght of the frame.	**
** param: lenght2, is the second byte where we found the lenght of the frame.	**
** param: total, is lenght1 + lenght2 - (minimum lenght of a type of frame).	**
** return: result, is TRUE if everything work okay.				**
*********************************************************************************/
int xbee_getLenght(unsigned char typeFrame, unsigned int lenght1, unsigned int lenght2, unsigned int *total){

    *total=lenght1+lenght2;

    switch(typeFrame){
	case 0x88://Response AT command (0x88)
		*total=*total-8;
		break;
	case 0x90://TRAMA DATOS (0X90)
		*total=*total-12;
		break;
	case 0x97://REMOTE COMMAND RESPONSE (0x97)
		*total=*total-18;
		break;
	case 0x91://Explicit Rx Indicator (0x91)
		*total=*total-21;
		break;
	default:
		*total=0;
		break;
    }

    return TRUE;
}

/*********************************************************************************
** Depending of the type of frame we have received, this method will call one	**
** function or another.								**
**										**
** param: num, is the lenght of the frame.					**
** param: frame, is the pointer of the frame received.				**
** param: typeFrame, is the type of the frame we have received.			**
** param: data, is a pointer where we will save the datas.			**
** param: data_lenght, is a pointer where we'll save the lenght of the data.	**
*********************************************************************************/
int xbee_readApiFrame(int *num, unsigned char *frame, unsigned char typeFrame, unsigned char *data, unsigned int *data_lenght){

    switch(typeFrame){
	case 0x88://TRAMA RESPUESTA COMMANDO AT (0x88)
		xbee_frameResponseATCommand(frame,*num);
		*num=0;
		break;
	case 0x90://TRAMA DATOS (0X90)
		xbee_frameReceivePacket(frame,*num,data,data_lenght);
		*num=0;
		break;
	case 0x8A://TRAMA STATUS (0X8A)
		xbee_frameModemStatus(frame,*num);
		*num=0;
		break;
	case 0x95://TRAMA NODO IDENTIFICACION (0x95)
		xbee_frameNodeIdentificationIndicator(frame,*num);
		*num=0;
		break;
	case 0x8B://TRAMA TRANSMIT STATUS (0X8B)
		xbee_frameTransmitStatus(frame,*num);
		*num=0;
		break;
	case 0x92://TRAMA IO DATA SAMPLE RX (0X92)			
		xbee_frameIODataSampleRx(frame,*num);
		*num=0;
		break;
	case 0x97://REMOTE COMMAND RESPONSE (0x97)
		xbee_frameRemoteCommandResponse(frame,*num);
		*num=0;
		break;
	case 0x91://Explicit Rx Indicator (0x91)
		xbee_ExplicitRxIndicator(frame,*num,data,data_lenght);
		*num=0;
		break;
	default:
		*num=0;
		DEBUG_PRINT("Tipo de trama no tratada\n");
		break;
    }

    return TRUE;
}

/*********************************************************************************
** This method is used to reading the serial port when a character is received.	**
**										**
** param: hPort, is the index of the serial port.				**
*********************************************************************************/
int numero=0;
unsigned char *frame,*p,*data; 
unsigned int lenght=0;
void SERIAL_PORT_EVENT(int * hPort)
{
    unsigned char c;
    read(fd,&c,1);

    //Very useful print if we want to check the frame received through the serial port
    //printf(" %3d: 0x%02X %c\n", numero, c,((c >= ' ' && c <= '~') ? c : '.'));

    //Checking if the first character is the delimiter 0x7E
    if(numero==0 && c==0x7E){
	frame=(char *)calloc(FRAME_LENGHT, sizeof(char));
	//saving where the frame starts
	p=frame;
	//saving the first character
	*frame=c;
    }
    //saving the second character
    if(numero==1){
	frame++;
	*frame=c;
    }
    //saving the third character
    if(numero==2){
	frame++;
	*frame=c;
    }
    //If is the number four
    if(numero==3){
	frame++;
	//saving the character
	*frame=c;
	//Getting the lenght of the datas
	xbee_getLenght(c,(int)*(p+1),(int)*(p+2),&lenght);
	//keeping memory
	if(lenght>0)
	    data=(char *)calloc(lenght, sizeof(char));
    }
    //Saving the rest of the characters
    if(numero>3){
	frame++;
	*frame=c;
	//If is the end of the frame
	if(numero==(((int)*(p+2))+3)){
	    //Sending the frame to get the information
	    xbee_readApiFrame(&numero,frame,*(p+3),data,&lenght);
	    //making sure we reset all the variables we need for the next new frame
	    if(numero==0){
	    	frame=p;
		lenght=0;
	    	numero=-1;
	    }
	}
    }

    numero++;
}

/*********************************************************************************
** This method is used to closing the serial port.				**
*********************************************************************************/
int xbee_end_module(){

    //Set_Configure_Port(fd,OldConf);     // Restituyo la antigua configuración 
                                        // del puerto.
    #ifdef MaRTE_OS
    	ioctl(fd,SERIAL_SETATTR,((void *)&OldConf));
    #else
    	if(tcsetattr(fd,TCSANOW,&OldConf)!=0){
	    printf("ERROR: No se pudo poner configuración del puerto serie\n" );
       	    return SETATTR_ERROR;
    	}
    #endif

    if (close(fd)==-1) {
      DEBUG_PERROR("Error al cerrar el fichero");
      return CLOSE_PORT_ERROR;
    }
    DEBUG_PRINT("Fichero cerrado\n");

    return TRUE;
}

//Creating the conditional variable and the mutex
int received=FALSE;
pthread_mutex_t mutex;
pthread_cond_t condition;

/*********************************************************************************
** This method initializes the mutex and the conditional variable.		**
*********************************************************************************/
int create (void){
    // Initialize the mutex
    if (pthread_mutex_init(&mutex,NULL)!=0) {
	return CREATION_ERROR;
    }
    // Initialize the condition variable with default attributes
    if (pthread_cond_init(&condition,NULL)!=0) {
	return CREATION_ERROR;
    }
    return (0);
}

/*********************************************************************************
** This method is used to locking the mutex and to change the variable.		**
*********************************************************************************/
int setReceived(void){
    if (pthread_mutex_lock(&mutex)!=0){
	printf("setReceived--> Error al bloquear el mutex\n");
	return SYNCH_ERROR;
    }
    if(received==FALSE){
    	received=TRUE;
	if (pthread_cond_signal(&condition)!=0)
	    return SYNCH_ERROR;
    }
    if (pthread_mutex_unlock(&mutex)!=0){
	printf("setReceived--> Error al liberar el mutex\n");
	return SYNCH_ERROR;
    }

    return TRUE;
}

/*********************************************************************************
** This method is used to getting the data we have received through 		**
** the serial port.								**
*********************************************************************************/
int getReceived(unsigned char **mi_data){
    if (pthread_mutex_lock(&mutex)!=0){
	printf("getReceived--> Error al bloquear el mutex\n");
	return SYNCH_ERROR;
    }

    while(received==FALSE){
	printf("Espero datos\n");
	if (pthread_cond_wait(&condition,&mutex)!=0) {
	    return SYNCH_ERROR;
	}
    }

    if(received==TRUE){
	received=FALSE;
	*mi_data=data;
    }

    if (pthread_mutex_unlock(&mutex)!=0) {
	printf("getReceived--> Error al liberar el mutex\n");
	return SYNCH_ERROR;
    }

    return TRUE;
}

/*********************************************************************************
** This method is used to reading the variable where we have the data 		**
** we have received.								**
*********************************************************************************/
int xbee_readData(unsigned char **mi_data){
    
    if(getReceived(mi_data)!=TRUE){
	printf("Error al recibir datos\n");
	return FALSE;
    }

    return TRUE;
}
