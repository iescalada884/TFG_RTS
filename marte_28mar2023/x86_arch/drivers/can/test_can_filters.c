#define TEST_CAN_FILTERS_SENDER

#ifdef TEST_CAN_FILTERS_SENDER

#include <stdio.h>  // perror
#include <fcntl.h>  // open, O_RDWR
#include <stdlib.h> // exit
#include <unistd.h> // write, sleep
#include <drivers/can.h> // struct can_frame_t

#define ERROR(s) {perror (s); exit (-1);}

static void pause(){
        char key;
        printf(" press Enter to send...");
        key = getchar();
}

int main ()
{
        int fd0;
        ssize_t wret;
        struct can_frame_t frame;

        fd0 = open ("/dev/can0", O_RDWR);
        if (fd0 == -1) ERROR ("could not open can0\n");

        frame.is_extended_format = true;
        frame.is_rtr  = false;
        frame.id      = 0x12345678;
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
                pause();
                wret = write(fd0, (void *)&frame, sizeof(frame));
                if (wret != sizeof(frame)) ERROR ("could not send frame\n");
        }

        return 0;
}

//----------------------------------------------------------------------------
#else
//----------------------------------------------------------------------------

#include <stdio.h>  // perror, printf,
#include <fcntl.h>  // open, O_RDWR
#include <stdlib.h> // exit
#include <unistd.h> // sleep
#include <drivers/can.h> // can ioctls

#define ERROR(s) {perror (s); exit (-1);}

int main ()
{
        int fd, i, ret;
        struct ioctl_filters_t ioctl_filters;
        struct can_filter_t filters[2];

        fd = open ("/dev/can0", O_RDWR);
        if (fd == -1) ERROR ("could not open can0");

        ioctl_filters.filters = filters;

        while (1) {
                printf("Enter number of filters: ");
                scanf("%u",&ioctl_filters.len);

                for(i=0; i<ioctl_filters.len; i++) {
                        printf("Enter hex mask for filter[%d]: ", i);
                        scanf("%x",&filters[i].mask);
                        printf("Enter hex code for filter[%d]: ", i);
                        scanf("%x",&filters[i].code);
                }

                ret = ioctl(fd, CAN_IOCTL_SET_FILTERS, &ioctl_filters);
                if (ret == -1) ERROR ("ioctl failed");
        }

        return 0;
}

#endif // TEST_CAN_FILTERS_SENDER


