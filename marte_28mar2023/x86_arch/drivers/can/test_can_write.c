#include <stdio.h>  // perror
#include <fcntl.h>  // open, O_RDWR
#include <stdlib.h> // exit
#include <unistd.h> // write
#include <drivers/can.h> // struct can_frame_t

#define ERROR(s) {perror (s); exit (-1);}

int main ()
{
        int fd0;
        ssize_t wret;
        struct can_frame_t frame;

        fd0 = open ("/dev/can0", O_RDWR);
        if (fd0 == -1) ERROR ("could not open can0\n");

        frame.is_extended_format = true;
        frame.is_rtr  = false;
        frame.dlc     = 8;
        frame.data[0] = 0;
        frame.data[1] = 1;
        frame.data[2] = 2;
        frame.data[3] = 3;
        frame.data[4] = 4;
        frame.data[5] = 5;
        frame.data[6] = 6;
        frame.data[7] = 7;

        while(1) {
                printf("Enter ID: ");
                scanf("%X", &frame.id);

                wret = write(fd0, (void *)&frame, sizeof(frame));
                if (wret != sizeof(frame)) ERROR ("could not send frame\n");
        }

        return 0;
}


