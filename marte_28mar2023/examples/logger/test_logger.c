/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                test_logger
 *
 *                                    c
 *
 * File 'test_logger.c'                                   By Sangorrin
 *
 *
 * Test for the loggers. It first creates the logger thread and then writes
 * some data to the buffer with a certain period. This data should be read
 * by the logger and sent to the appropiate device (console, ..)
 *
 *---------------------------------------------------------------------------*/
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <drivers/console_switcher.h>
#include <misc/logger.h>

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define LOG_DEVICE LOG_ETHERNET // LOG_CONSOLE

int main()
{
        int err, fd, count;
        struct timespec logger_period = {4, 0};
        char write_data[] = "0123456789";

        // SERIAL_CONSOLE_INIT(); // uncomment for QEMU with LOG_CONSOLE

        DEBUG("opening membuff device file to write\n");
        fd = open(MEMBUFFER_PATH, O_WRONLY);
        if (fd == -1) return -1;

        err = logger_init(LOG_DEVICE);
        assert(err == 0);

        err = logger_thread_create(&logger_period);
        assert(err == 0);

        while(1) {
                DEBUG("writing data to buffer\n");
                count = write(fd, write_data, sizeof(write_data));
                if (count < 0) return -1;
                DEBUG("%d bytes written\n", count);

                sleep(1);
        }

        return 0;
}
