/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.51  Oct 2005
 *
 *                          t e s t _ c m p s 0 3
 *
 *                                    C
 *
 * File 'test_cmps03.c'                                        By Sangorrin
 *
 *
 * This file show how to use the cmps03 I2c driver.
 *
 * to compile: mgcc test_cmps03_c.c
 *
 *---------------------------------------------------------------------------*/
#include <stdio.h>  /*printf..*/
#include <fcntl.h>  /*open*/
#include <unistd.h> /*close read ...*/
#include <stdint.h> /* uint8_t ... */
#include <stdlib.h> /* exit */
#include <pthread.h>

#include <drivers/cmps03.h>

#define ERROR(s) {perror (s); exit (-1);}

//--------------------------------------------------------------------------
void pause(){
  char pulsacion;
  printf("press...");
  pulsacion = getchar();
}

void msg(char *s){
  printf("\n------------------------------------------------------------\n");
  printf(s);
  printf("\n------------------------------------------------------------\n");
  pause();
}
//--------------------------------------------------------------------------

int main()
{
  int fd, ret;
  ssize_t bytes;

  bearing_byte byte;
  bearing_word word;

  cmps03_ioctl_cmd cmd;
  cmps03_ioctl_arg arg;


  ret = pthread_setschedprio ( pthread_self(), 4);

  msg("This is the C Test program for the CMPS03 I2C Driver. ");
  if ((fd = open ("/dev/cmps03", O_RDONLY)) == -1)
    ERROR ("open");
  while(1){
    msg("Read Compass Bearing as a Byte (no blocking): ");
    // 1- Start conversion
    cmd = START_CONVERSION;
    arg.mode = BEARING_MODE_BYTE;
    if (ioctl(fd,cmd,&arg) == -1)
      ERROR ("ioctl");
    // 2- Now check the Status of the conversion
    cmd = GET_STATUS;
    do{
      if (ioctl(fd,cmd,&arg) == -1)
         ERROR ("ioctl");
      sleep(1);
      switch(arg.status){
      case CONVERSION_IN_PROGRESS:
         printf("CONVERSION_IN_PROGRESS: Mr I2C-Daemon is busy\n");
         break;
      case NO_CONVERSION_STARTED:
         printf("NOT_IN_USE: You better send a command\n");
         exit(-1);
      case CMPS03_ERROR:
         printf("CMPS03_ERROR: oooooops\n");
         exit(-1);
      }
    }while(arg.status != CONVERSION_DONE);
    printf("CONVERSION_DONE: Good work Mr I2C-Daemon\n");
    // 3- Finally read the data
    if ((bytes = read(fd,&byte,1)) == -1)
      ERROR ("read");
    printf("Compass Bearing as a Byte: %u (360:%d)\n",byte,(int)byte*360/255);

    //--------------------------------------------------------

    msg("Read Compass Bearing as a Word (no blocking): ");
    // 1- Start conversion
    cmd = START_CONVERSION;
    arg.mode = BEARING_MODE_WORD;
    if (ioctl(fd,cmd,&arg) == -1)
      ERROR ("ioctl");
    // 2- Now check the Status of the conversion
    cmd = GET_STATUS;
    do{
      if (ioctl(fd,cmd,&arg) == -1)
         ERROR ("ioctl");
      sleep(1);
      switch(arg.status){
      case CONVERSION_IN_PROGRESS:
         printf("CONVERSION_IN_PROGRESS: Mr I2C-Daemon is busy\n");
         break;
      case NO_CONVERSION_STARTED:
         printf("NOT_IN_USE: You better send a command\n");
         exit(-1);
      case CMPS03_ERROR:
         printf("CMPS03_ERROR: oooooops\n");
         exit(-1);
      }
    }while(arg.status != CONVERSION_DONE);
    printf("CONVERSION_DONE: Good work Mr I2C-Daemon\n");
    // 3- Finally read the data
    if ((bytes = read(fd,&word,2)) == -1)
      ERROR ("read");
    printf("Compass Bearing as a Word: %u \n",word);
    pause();
  }
  close(fd);
  exit (0);
}
