#include <stdio.h>  // perror
#include <fcntl.h>  // open, O_RDWR
#include <stdlib.h> // exit
#include <unistd.h>   // sleep

#define ERROR(s) {perror (s); exit (-1);}

int main ()
{
        int fd0, fd1;

        fd0 = open ("/dev/can0", O_RDWR);
        if (fd0 == -1) ERROR ("could not open can0");

        fd1 = open ("/dev/can1", O_RDWR);
        if (fd1 == -1) ERROR ("could not open can1");

        while (1) {
                sleep(1);
        }

        return 0;
}


