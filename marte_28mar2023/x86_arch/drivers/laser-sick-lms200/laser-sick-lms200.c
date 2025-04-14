/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                             'laser-sick-lms200.c'
 *
 *                                      C
 *
 *  File 'laser-sick-lms200.c'                              by F.J.Feijoo
 *                                            University of Zaragoza (UNIZAR)
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2007, Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
 *   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                      Michael Gonzalez Harbour      mgh@unican.es
 *
 *  MaRTE OS  is free software; you can  redistribute it and/or  modify it
 *  under the terms of the GNU General Public License  as published by the
 *  Free Software Foundation;  either  version 2, or (at  your option) any
 *  later version.
 *
 *  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 *  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 *  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 *  General Public License for more details.
 *
 *  You should have received  a  copy of  the  GNU General Public  License
 *  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 *  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 *  02111-1307, USA.
 *
 *  As a  special exception, if you  link this  unit  with other  files to
 *  produce an   executable,   this unit  does  not  by  itself cause  the
 *  resulting executable to be covered by the  GNU General Public License.
 *  This exception does  not however invalidate  any other reasons why the
 *  executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/

#include <drivers/laser-sick-lms200.h>
#include <drivers/serial_port_driver.h>
#include <pthread.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

/*the cmd and ack packets for the 5 different range/resolution modes*/
const uchar PCLMS_RES1[]={0x02,0x00,0x05,0x00,0x3b,0x64,0x00,0x64,0x00};
const uchar PCLMS_RES2[]={0x02,0x00,0x05,0x00,0x3b,0x64,0x00,0x32,0x00};
const uchar PCLMS_RES3[]={0x02,0x00,0x05,0x00,0x3b,0x64,0x00,0x19,0x00};
const uchar PCLMS_RES4[]={0x02,0x00,0x05,0x00,0x3b,0xb4,0x00,0x64,0x00};
const uchar PCLMS_RES5[]={0x02,0x00,0x05,0x00,0x3b,0xb4,0x00,0x32,0x00};
const uchar LMSPC_RES1_ACK[]=
    {0x06,0x02,0x80,0x07,0x00,0xbb,0x01,0x64,0x00,0x64,0x00,0x10};
const uchar LMSPC_RES2_ACK[]=
    {0x06,0x02,0x80,0x07,0x00,0xbb,0x01,0x64,0x00,0x32,0x00,0x10};
const uchar LMSPC_RES3_ACK[]=
    {0x06,0x02,0x80,0x07,0x00,0xbb,0x01,0x64,0x00,0x19,0x00,0x10};
const uchar LMSPC_RES4_ACK[]=
    {0x06,0x02,0x80,0x07,0x00,0xbb,0x01,0xb4,0x00,0x64,0x00,0x10};
const uchar LMSPC_RES5_ACK[]=
    {0x06,0x02,0x80,0x07,0x00,0xbb,0x01,0xb4,0x00,0x32,0x00,0x10};

/*the cmd and ack packets different measurement unit modes*/
const uchar PCLMS_SETMODE[]=
    {0x02,0x00,0x0a,0x00,0x20,0x00,0x53,0x49,0x43,0x4b,0x5f,0x4c,0x4d,0x53};
const uchar PCLMS_MM[]=
   {0x02,0x00,0x23,0x00,0x77,
    0x00,0x00, // Block A
    0x46,0x00, // Block B
    0x01, // Block C [9]
    0x00, // Block D (was 0xd)
    0x01, // Block E - 0x00 = cm, 0x01 = mm
    0x00, // Block F
    0x00, // Block G
    0x02, // Block H
    0x02, // Block I
    0x02, // Block J
    0x00, // Block K
    0x00, // Block L
    0x0a, // Block M
    0x0a, // Block N
    0x50, // Block O
    0x64, // Block P
    0x00, // Block Q
    0x0a, // Block R
    0x0a, // Block S
    0x50, // Block T
    0x80, // Block U
    0x00, // Block V
    0x0a, // Block W
    0x0a, // Block X
    0x50, // Block Y
    0x64, // Block Z
    0x00, // Block A1
    0x00, // Block A2
    0x00,0x00, // Block A3
    0x02,0x00};// Block A4

const uchar LMSPC_MM_ACK[]=
    {0x06,0x02,0x80,0x25,0x00,0xf7,0x01,0x00,0x00,0x46,0x00,0x01,0x0d,0x01,\
    0x00,0x00,0x02,0x02,0x02,0x00,0x00,0x0a,0x0a,0x50,0x64,0x00,0x0a,0x0a,\
    0x50,0x80,0x00,0x0a,0x0a,0x50,0x64,0x00,0x00,0x00,0x00,0x02,0x00,0x10};

/* the cmd packets for setting transfer speed and  controlling the start
    and stop of measurement*/
const uchar PCLMS_STATUS[] = {0x02,0x00,0x01,0x00,0x31};
const uchar PCLMS_START[]  = {0x02,0x00,0x02,0x00,0x20,0x24};
const uchar PCLMS_STOP[]   = {0x02,0x00,0x02,0x00,0x20,0x25};
const uchar PCLMS_B9600[]  = {0x02,0x00,0x02,0x00,0x20,0x42};
const uchar PCLMS_B19200[] = {0x02,0x00,0x02,0x00,0x20,0x41};
const uchar PCLMS_B38400[] = {0x02,0x00,0x02,0x00,0x20,0x40};
const uchar PCLMS_B500000[]= {0x02,0x00,0x02,0x00,0x20,0x48};
/*the ack packet for the above*/
const uchar LMSPC_CMD_ACK[]={0x06,0x02,0x80,0x03,0x00,0xa0,0x00,0x10};

typedef struct LMS
{
    int LMS_fd;
    //variables globales para añadir funciones estilo player:
    double min_angle, max_angle, scan_res, range_res, scan_count;
    //puntero a un vector de valores doubles que debe definir el usuario
    //double* valores;

    //mutex para bloquear la lectura del laser de forma correcta
    pthread_mutexattr_t mutexattr;
    pthread_mutex_t mutex;

    //vector donde se guardan las medidas del laser
    int valores0[MAXVALORES];
    int valores1[MAXVALORES];
    int numVector; // tomara el valor 0 o 1 dependiendo del vector que
                   // se este escribiendo en ese momento
                   // se tendra que leer el vector con indice contrario
    int estado;
} LMS;

// variable global para controlarlo todo evitando al maximo
// los parametros (conversion ADA)
LMS laser;
//variable para dejar el puerto serie como estaba
static termios_t OLDTERMIOS;

// Calculates the CRC for packets sent to/from the LMS
static unsigned short LMSCRC(unsigned char* theBytes, int lenBytes)
{
    unsigned char xyz1 = 0;
    unsigned char xyz2 = 0;
    unsigned short uCrc16 = 0;
    int i;
    for (i = 0; i < lenBytes; i++) {
        xyz2 = xyz1;
        xyz1 = theBytes[i];

        if (uCrc16 & 0x8000) {
            uCrc16 = ((uCrc16 & 0x7fff) << 1);
            uCrc16 = (uCrc16 ^ 0x8005);
        }
        else {
            uCrc16 = uCrc16 << 1;
        }
        uCrc16 = (uCrc16 ^ (xyz1 | (xyz2 << 8)));
    }
    return uCrc16;
}

static void printError(const char * msg)
{
    fprintf(stderr, "%s", msg);
}

static void printError1(const char * msg, int val1)
{
    printf("%s 0x%x\n", msg, val1);
}

static void printError2(const char * msg, int val1, int val2)
{
    printf("%s 0x%x 0x%x\n", msg, val1, val2);
}

// Compares two messages
static uchar msgcmp(int len1, const uchar *s1, int len2, const uchar *s2)
{
    int i;
    unsigned short crcval;

    if (len1 != len2) {
        printError2("msgcmp - message lengths didn't match", len1, len2);
        return FALSE;
    }

    for(i=0; i<len1; i++) {
        if (s1[i] != s2[i]) {
            printError2("msgcmp - character didn't match", s1[i], s2[i]);
            printf("posicion %d\n",i);
            return FALSE;
        }
    }

    crcval = LMSCRC((unsigned char*)s1, len1);

    if ((crcval & 0xff) != s2[len2]) {
        printf("primero s1 = %02x, s2 = %02x\n",(crcval & 0xff),s2[len2]);
        printError2("msgcmp - CRC didn't match1", crcval & 0xff, s2[len2-2]);
        return FALSE;
    }

    if (((crcval >> 8) & 0xff) != s2[len2+1]) {
        printf("s1 = %02x, s2 = %02x\n",((crcval >> 8) & 0xff),s2[len2+1]);
        printError2("msgcmp - CRC didn't match2",
                    (crcval >> 8) & 0xff, s2[len2-1]);
        return FALSE;
    }

    return TRUE;
}

// Writes a message to the LMS
static void wtLMSmsg(int len, const uchar *msg)
{
    unsigned short crcval = LMSCRC((unsigned char*)msg, len);
    unsigned char sendchar;
    int bytes=0, cnt=0;
    unsigned char idx=0;

    while(bytes<len) {
        if ((cnt=write(laser.LMS_fd ,(char *)(msg+idx),1)) > 0){
            bytes+=cnt;
            idx++;
        }
    }

    sendchar = crcval & 0xff;

    write(laser.LMS_fd , (const void *) &sendchar, 1);

    sendchar = (crcval >> 8) & 0xff;

    cnt=write(laser.LMS_fd , (const void *) &sendchar, 1);

#ifdef DEBUG
    int i;
    printf("write msg: ");
    for(i=0;i<len; i++) printf("%02x ",msg[i]);
    printf("%02x %02x\n", crcval & 0xff, (crcval >> 8) & 0xff);
#endif
}

//esta es la misma funcion que rdLMSmsg pero rellena el vector de valores
//del laser esto es para mejorar el rendimiento y evitar leer el buffer y
//luego rellenar el vector del laser
int rdLMSmsgValues (int len, const uchar *buf)
{
    int i,j=0,paso=0;
    int bytes_read=0,nread=0;
    uchar primer_valor, segundo_valor;

    pthread_mutex_lock(&laser.mutex);

    //cambio del vector que tenemos que actualizar
    laser.numVector = !laser.numVector;
    pthread_mutex_unlock(&laser.mutex);

    i=0;
    while(nread < len){
        i++;
        if ((bytes_read=read(laser.LMS_fd ,(void *)(buf+nread),1))>0) {
            if(nread % 2 == 0){
                primer_valor = buf[nread];paso=1;
            } else {
                segundo_valor = buf[nread]; paso++;
            }
            nread += bytes_read;
        }

        //cada 2 byes leidos hay que fusionarlos para formar el valor del laser
        if (paso == 2){
            if (laser.numVector)
                laser.valores1[j]=( (segundo_valor & 0x1f) <<8 | primer_valor);
            else
                laser.valores0[j]=( (segundo_valor & 0x1f) <<8 | primer_valor);
            j++;
            paso = 0;
        }
    }

    return (nread);
}

static int rdLMSmsg(int len, const uchar *buf)
{
    int i;
    int bytes_read=0,nread=0;

    i=0;
    while(nread < len){
        i++;
        if ((bytes_read=read(laser.LMS_fd ,(void *)(buf+nread),1))>0) {
            //printf("rdLMSmsg lee: %02x \n",buf[nread]);
            nread += bytes_read;
        }
    }

#ifdef DEBUG
    for(i=0;i<=nread; i++) printf("%d : %02x \n",i,buf[i]);
#endif

    return (nread);
}

static uchar rdLMSbyte()
{
    uchar buf[50];
    int nread=0;

    while(1){
        if (read(laser.LMS_fd ,&buf[0],1)>0){
            nread++;
#ifdef DEBUG
            printf("read byte: %02x\n", buf[0]);
#endif
        }
        if (nread==1) break;
    }

    return buf[0];
}

void showdata(int len, char *buf)
{
    struct timeval now;
    int i,hr,min,sec;
    double val;

    gettimeofday(&now,NULL);
    hr=(now.tv_sec/3600 % 24 ) % 24;
    min=(now.tv_sec % 3600)/60;
    sec=now.tv_sec % 60;

    /*each value are represented by 16 bits*/
    /*only the lower 12 bits are significant*/
    for(i=0; i<len; i=i+2) {//len
        if((i % 40) ==0) printf("\n%d:",i/2);
        val=(double)( (buf[i+1] & 0x1f) <<8  |buf[i]);
        printf("%5.0f ", val);
    }
    printf("\n");
}

void showLaser(int len, char *buf) {
    int i;
    int j;
    double buf_data[MAXNDATA];

    j=7;
    len = len /2;

    for(i=0; i<len; i++) {//len
        buf_data[i]=(double)( ((uchar)buf[j+1] & 0x1f) <<8  |(uchar)buf[j]);
        j=j+2;
    }

    for(i=0; i<len; i=i+1) {//len
        if((i % 40) ==0) printf("\n%d:",i/2);
        printf("%5.0f ", buf_data[i]);
    }
    printf("\n");
}

/*return true if the ACK packet is as expected*/
static uchar chkAck(int ackmsglen, const uchar *ackmsg)
{
    int i,buflen;
    uchar buf[MAXNDATA];

    /*the following is to deal with a possibly timing issue*/
    /*the communication occasionally breaks without this*/
    for(i=0;i<MAXRETRY;i++) {
        if(rdLMSbyte()==ackmsg[0]) break;
    }
    memset(buf,0,MAXNDATA);

    buflen=rdLMSmsg(ackmsglen+1,buf);

    return msgcmp(ackmsglen-1,ackmsg+1,buflen-2,buf);
}

/*set the communication speed and terminal properties*/
int initLMS(const char *serialdev, int baud_sel)
{
    const uchar *msg;

    termios_t new_termios;
    struct timespec ts;
    static struct timespec tv0;
    tv0.tv_sec = 0;
    tv0.tv_nsec = 500000000;

    printf("initLMS empieza\n");
    laser.LMS_fd  = open(serialdev, O_RDWR | O_NOCTTY);
    printf("open\n");

    pthread_mutexattr_init (&laser.mutexattr);
    pthread_mutexattr_setprotocol(&laser.mutexattr,PTHREAD_PRIO_INHERIT);
    pthread_mutex_init (&laser.mutex, &laser.mutexattr);
    laser.numVector = 0;

    if (laser.LMS_fd  <0) {
        printError1("initLMS - Error opening 1", laser.LMS_fd);
        perror(serialdev);
        exit(-1);
    }
    ioctl(laser.LMS_fd ,SERIAL_GETATTR, ((void *)&OLDTERMIOS));

    ioctl(laser.LMS_fd ,SERIAL_FLUSH,NULL);
    ts.tv_sec = 0;
    ts.tv_nsec = 1000*1000;
    nanosleep (&ts, NULL);

    ioctl(laser.LMS_fd ,SERIAL_GETATTR, ((void *)&new_termios));
    /*after power up, the laser scanner will reset to 9600bps*/

    new_termios.cflag = B9600 | CS8 | CLOCAL | CREAD ;
    new_termios.iflag = IGNPAR;
    new_termios.oflag = 0;

    /* set input mode (non-canonical, no echo,...) */
    new_termios.lflag = 0;
    new_termios.cc[VTIME]    = 10;   /* inter-character timer unused */
    new_termios.cc[VMIN]     = 255;  /* blocking read until 1 chars received */

    new_termios.ospeed = B9600;
    new_termios.ispeed = B9600;

    ioctl(laser.LMS_fd ,SERIAL_SETATTR,((void *)&new_termios));

    if (baud_sel == BAUD_38400) { // step to the 38400bps mode
        msg = PCLMS_B38400;
        new_termios.cflag = B38400 | CS8 | CLOCAL | CREAD;
        new_termios.ospeed = B38400;
        new_termios.ispeed = B38400;

    } else if (baud_sel == BAUD_19200) { // step to the 19200bps mode
        msg = PCLMS_B19200;
        new_termios.cflag = B19200 | CS8 | CLOCAL | CREAD;
        new_termios.ospeed = B19200;
        new_termios.ispeed = B19200;

    } else {
    //Do nothing - goal is 9600 and already at 9600
        return laser.LMS_fd ;
    }

    printf("initLMS empieza2\n");

    //This comand is always sent at 9600
    wtLMSmsg(sizeof(PCLMS_B500000)/sizeof(uchar),msg);
    printf("initLMS empieza3\n");
    if(!chkAck(sizeof(LMSPC_CMD_ACK)/sizeof(uchar),LMSPC_CMD_ACK))
        printError("initLMS - Baud rate changes failure\n");

    // set the PC side as well
    ioctl(laser.LMS_fd ,SERIAL_FLUSH,NULL);

    close(laser.LMS_fd );
    ts.tv_sec = 0;
    ts.tv_nsec = 400000000;
    nanosleep (&ts, NULL);

    laser.LMS_fd  = open(serialdev, O_RDWR | O_NOCTTY);

    if (laser.LMS_fd  <0) {
        printError1("initLMS - Error opening 2", laser.LMS_fd);
        perror(serialdev);
        exit(-1);
    }

    ioctl(laser.LMS_fd, SERIAL_FLUSH,NULL);

    ioctl(laser.LMS_fd, SERIAL_SETATTR,((void *)&new_termios));

    ioctl(laser.LMS_fd, SERIAL_FLUSH,NULL);
    return laser.LMS_fd ;
}

// Sets the sweep width and resolution
int setRangeRes(int res)
{
    const uchar *msg, *ackmsg;
    int retval = 0;
    //Tambien guardamos en las variables globales los valores iniciales

    /*change the resolution*/
    switch(res){
        case (RES_1_DEG | RANGE_100):
            msg=PCLMS_RES1;
            ackmsg=LMSPC_RES1_ACK;
            laser.scan_res = 1.0; laser.range_res = 100.0;
            laser.scan_count = 101;
            break;
        case (RES_0_5_DEG | RANGE_100):
            msg=PCLMS_RES2;
            ackmsg=LMSPC_RES2_ACK;
            laser.scan_res = 0.5; laser.range_res = 100.0;
            laser.scan_count = 201;
            break;
        case (RES_0_25_DEG | RANGE_100):
            msg=PCLMS_RES3;
            ackmsg=LMSPC_RES3_ACK;
            laser.scan_res = 0.25; laser.range_res = 100.0;
            laser.scan_count = 401;
            break;
        case (RES_1_DEG | RANGE_180):
            msg=PCLMS_RES4;
            ackmsg=LMSPC_RES4_ACK;
            laser.scan_res = 1.0; laser.range_res = 180.0;
            laser.scan_count = 181;
            break;
        case (RES_0_5_DEG | RANGE_180):
            msg=PCLMS_RES5;
            ackmsg=LMSPC_RES5_ACK;
            laser.scan_res = 0.5; laser.range_res = 180.0;
            laser.scan_count = 361;
            break;
        default:
            printError("Invalid resolution selected. Drop back to default\n");
            msg=PCLMS_RES1;
            ackmsg=LMSPC_RES1_ACK;
            break;
    }

    // the following two line works only because msg & ackmsg are
    // const uchar str
    wtLMSmsg(sizeof(PCLMS_RES1)/sizeof(uchar),msg);

    if(!chkAck(sizeof(LMSPC_RES1_ACK)/sizeof(uchar),ackmsg)) {
        printError("setRangeRes - Resolution mode setting failure\n");
        retval = 1;
    }
    return retval;
}

// Sends the password to enable changing the mode
int sendPassword()
{
    /*invoking setting mode*/
    int retval = 0;
    wtLMSmsg(sizeof(PCLMS_SETMODE)/sizeof(uchar),PCLMS_SETMODE);
    if(!chkAck(sizeof(LMSPC_CMD_ACK)/sizeof(uchar),LMSPC_CMD_ACK)) {
        printError("sendPassword - Measurement mode setting failure\n");
        retval = 1;
    }
    return retval;
}

// Selects mm/cm
// This message causes the red signal to light on an LMS291 - reason unknown
int setUnits(int unit)
{
    int retval = 0;
    uchar msg[100], ackmsg[100];
    memcpy(msg, PCLMS_MM, sizeof(PCLMS_MM));
    memcpy(ackmsg, LMSPC_MM_ACK, sizeof(LMSPC_MM_ACK));

    /*change the measurement unit*/
    /*may need to increase the timeout to 7sec here*/
    if (unit == MMMODE) {
    // No change is required since MM is the default
    } else if (unit == CMMODE) {
        msg[11] = 0x00;
    } else {
        printError("setUnits - Invalid units specified\n");
    }

    // The ACK contains the original message data
    memcpy(ackmsg+7, msg+5, sizeof(PCLMS_MM) - 5);

    // the following two line works only because msg & ackmsg are
    // const uchar str
    wtLMSmsg(sizeof(PCLMS_MM)/sizeof(uchar),msg);
    if(!chkAck(sizeof(LMSPC_MM_ACK)/sizeof(uchar),ackmsg)) {
        printError("setUnits - Measurement mode setting failure2\n");
        retval = 1;
    }
    return retval;
}

/*tell the scanner to enter the continuous measurement mode*/
int startLMS()
{
    int retval = 0;
    wtLMSmsg(sizeof(PCLMS_START)/sizeof(uchar),PCLMS_START);
    if(!chkAck(sizeof(LMSPC_CMD_ACK)/sizeof(uchar),LMSPC_CMD_ACK)) {
        printError("startLMS - LMS fails to start\n");
        retval = 1;
    }
    return retval;
}

/*stop the continuous measurement mode*/
void stopLMS()
{
    wtLMSmsg(sizeof(PCLMS_STOP)/sizeof(uchar),PCLMS_STOP);
    if(!chkAck(sizeof(LMSPC_CMD_ACK)/sizeof(uchar),LMSPC_CMD_ACK))
        printError("stopLMS - LMS fails to stop\n");
}

/*check the status bit of the measurement*/
void chkstatus(char byte)
{
    uchar c;
    c = (uchar) byte;
    switch(c & 0x07){
        case 0: printError("chkstatus - no error\n"); break;
        case 1: printError("chkstatus - info\n"); break;
        case 2: printError("chkstatus - warning\n"); break;
        case 3: printError("chkstatus - error\n"); break;
        case 4: printError("chkstatus - fatal error\n"); break;
        default: printError1("chkstatus - unknown error", c); break;
    }
    if(c & 0x40)
        printError("chkstatus - implausible measured value\n");
    if(c & 0x80)
        printError("chkstatus - pollution\n");
}

/*reset terminal and transfer speed of laser scanner before quitting*/
void resetLMS()
{
    wtLMSmsg(sizeof(PCLMS_B9600)/sizeof(uchar),PCLMS_B9600);
    if(!chkAck(sizeof(LMSPC_CMD_ACK)/sizeof(uchar),LMSPC_CMD_ACK))
        printError("resetLMS - Baud rate changes failure\n");

    ioctl(laser.LMS_fd,SERIAL_FLUSH,NULL);

    ioctl(laser.LMS_fd,SERIAL_SETATTR,((void *)&OLDTERMIOS));
    close(laser.LMS_fd);
}

void connectToLMS(int range_mode, int res_mode, int unit_mode,
                  char * port, int baud_sel)
{
    //static int fd = -1;
    laser.LMS_fd = -1;
    int retval = 0;
    int numTrys = 1;

    /*initialisation*/
    for (numTrys = 1; numTrys < 26; numTrys++) {
        retval = 0;

#ifdef DEBUG
        printf("trying to connect %d\n", numTrys);
#endif

        // Set the baud rate of the PC and the LMS
        laser.LMS_fd = initLMS(port, baud_sel);

        // mode setting
        retval |= setRangeRes(range_mode | res_mode);
        retval |= sendPassword(); // this enables the following command
        retval |= setUnits(unit_mode);
        retval |= startLMS();

        if (retval == 0) {
            // LMS is working
            printf("connected to LMS\n");
            return ;
        } else {
            // Tell the LMS to stop sending and set baud to 9600
            // Reset the LADAR to put it into a known state
            stopLMS(laser.LMS_fd);
            resetLMS(laser.LMS_fd);
            printError2("connectToLMS - Failed to connect", retval, numTrys);
        }
    }

    printError1("Failed to connect to ladar after many tries!", numTrys);
    exit(0);
}

int readLMSdata(uchar* buf)
{
    unsigned short realCRC = 0;
    int datalen;
    buf[0] = 0;

    while (buf[0] != 0x02) {
        buf[0] = rdLMSbyte();
    }

    buf[1] = rdLMSbyte(); // should be the ADR byte, ADR=0X80 here

    // LEN refers to the packet length in bytes, datalen
    // refers to the number of measurements, each of them are 16bits long
    buf[2] = rdLMSbyte(); // should be the LEN low byte
    buf[3] = rdLMSbyte(); // should be the LEN high byte
    buf[4] = rdLMSbyte(); // should be the CMD byte, CMD=0xb0 in this mode
    buf[5] = rdLMSbyte(); // samples low byte
    buf[6] = rdLMSbyte(); // samples high byte

    // only lower 12 bits of high byte are significant
    datalen = buf[5] | ((buf[6] & 0x1f) << 8);
    datalen = datalen * 2; /*each reading is 2 bytes*/

    // Check that we have a valid ADR byte, valid data length and
    // valid CMD byte
    if ((datalen > MAXNDATA) ||
         (0x80 != buf[1]) ||
         (0xb0 != buf[4])) {
        printf("ok");
        return 0;
    }

    rdLMSmsgValues(datalen,buf+7);

    buf[datalen + 7] = rdLMSbyte(); // Status byte

//************************************************
//relleno primero el campo de estado del laser
    uchar c;
    c = (uchar) buf[datalen+7];
    switch(c & 0x07){
        case 0: laser.estado = 0; break;
        case 1: laser.estado = 1; break;
        case 2: laser.estado = 2; break;
        case 3: laser.estado = 3; break;
        case 4: laser.estado = 4; break;
        default: laser.estado = 5; break;
    }
    if(c & 0x40)
        laser.estado = 6;
    if(c & 0x80)
        laser.estado = 7;
//************************************************

    buf[datalen + 8] = rdLMSbyte(); // should be CRC low byte*/
    buf[datalen + 9] = rdLMSbyte(); // should be CRC high byte*/
    realCRC = buf[datalen+8] | (buf[datalen+9] << 8);


    int lenBytes = datalen+8;
    unsigned short CRCcalculated;
    CRCcalculated = LMSCRC(buf, lenBytes);


    if (CRCcalculated != realCRC) {
        printError2("readLMSdata - CRC Error ", CRCcalculated, realCRC);
        return 0;
    }

    return datalen; // return 0 on error (0 valid samples)
}


// Con esta función buscamos el poder leer el laser cuando nos convenga.
// En ese caso habra que vaciar el buffer del puerto serie antes de ponernos
// a leer. Primero vacia el buffer con un flush de Marte y luego hace una
// lectura normal.

int readLMSdataDemand(uchar* buf){
    ioctl(laser.LMS_fd,SERIAL_FLUSH,NULL);
    //printf("LECTURA DEL LASER AHORA ...:\n");
    return readLMSdata(buf);
}

//Funciones de mas alto nivel.
//Estas funciones pretenden ofrecer al programador de marte un API similar
//al de player abstrayendo al
// maximo la parte de bajo nivel.

int GetCountLaser(){
    return laser.scan_count;
}

double GetScanResLaser() { return laser.scan_res; }

double GetRangeResLaser() { return laser.range_res; }

int readLMSValues(){
    uchar buf[2000];
    int len;
    len = readLMSdata(&buf[0]);
    return len/2;
}

//lo mismo bajo demanda
int readLMSValuesDemand (){
//  ioctl(laser.LMS_fd,SERIAL_FLUSH,NULL);
//  return  readLMSData(laser.valores);
    printf("No implementada");
    return 0;
}

void lockLaser(){
    pthread_mutex_lock(&laser.mutex);
}

void unlockLaser(){
    pthread_mutex_unlock(&laser.mutex);
}

//devuelve el valor del punto i del laser despues de una lectura ...
int laserazo (int i){
    if (laser.numVector)
        return laser.valores0[i];
    else
        return laser.valores1[i];
}

int getStatus (){
    return laser.estado;
}

void vaciarLaser(){
    ioctl(laser.LMS_fd,SERIAL_FLUSH,NULL);
}
//otra funcion que acceda al vector directamente
//(o eso que lo haga el usuario??)
