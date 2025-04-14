// mgcc test_serial_port_driver_c.c
// mv slave.img disk.img &&
// ./mkvirtualdisk &&
// mv disk.img slave.img &&
// qemu -hda slave.img -serial pty &
//
// mgcc -DSENDER test_serial_port_driver_c.c
// mv master.img disk.img &&
// ./mkvirtualdisk &&
// mv disk.img master.img &&
// qemu -hda master.img -serial /dev/pts/3 &

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <drivers/serial_port_driver.h>
#include <sys/types.h>
#include <time.h>

void sender_job(int fd);
void receiver_job(int fd);

int main()
{
    int fd;
    termios_t new_termios;

    if ((fd= open("/dev/ttyS0", O_RDWR)) < 0 ) {
        perror("open():");
        return -1;
    }

    ioctl(fd,SERIAL_GETATTR,((void *)&new_termios));
    new_termios.ospeed = B9600;
    new_termios.ispeed = B9600;
    ioctl(fd,SERIAL_SETATTR,((void *)&new_termios));

#ifdef SENDER
    sender_job(fd);
#else
    receiver_job(fd);
#endif

    printf ("end\n");
    return 0;
}

void sender_job(int fd)
{
    unsigned char buffer[] = "123456789abcdefgab123456789abcdefgab112233445566";
    struct timespec time = {0, 1000};

    while (1) {
        write(fd,buffer,sizeof(buffer));
        nanosleep (&time, NULL);
    }
}

void receiver_job(int fd)
{
    char c;

    while (1) {
        if (read(fd,&c,1) > 0) {
            printf("%c",c);
        }
    }
}
