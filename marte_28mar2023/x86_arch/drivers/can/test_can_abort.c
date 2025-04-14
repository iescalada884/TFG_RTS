#include <stdio.h>  // perror
#include <fcntl.h>  // open, O_RDWR
#include <stdlib.h> // exit
#include <unistd.h> // write
#include <drivers/can.h> // struct can_frame_t

#define ERROR(s) {perror (s); exit (-1);}

int main ()
{
        int fd0;
        int ret;
        struct can_frame_t frame;

        fd0 = open ("/dev/can0", O_RDWR);
        if (fd0 == -1) ERROR ("could not open can0\n");

        frame.id = 0xAAAA;
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
                write(fd0, (void *)&frame, sizeof(frame));
                write(fd0, (void *)&frame, sizeof(frame));
                write(fd0, (void *)&frame, sizeof(frame));
                write(fd0, (void *)&frame, sizeof(frame));
                write(fd0, (void *)&frame, sizeof(frame));
                write(fd0, (void *)&frame, sizeof(frame));

                ret = ioctl(fd0, CAN_IOCTL_ABORT_FRAME, NULL);
                if (ret == -1) ERROR ("ioctl abort failed");

                sleep(3);
        }

        return 0;
}
