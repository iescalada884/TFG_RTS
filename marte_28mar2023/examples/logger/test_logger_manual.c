/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                test_logger_manual
 *
 *                                    c
 *
 * File 'test_logger_manual.c'                                   By Sangorrin
 *
 *
 * Test for the logger. Instead of creating a logging thread here we
 * will use a manual trigger for the logging. That is, we will store messages
 * in the membuff device and then wait for the keyboard to read it and send
 * the log.
 *
 *---------------------------------------------------------------------------*/
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <misc/logger.h>

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define DEVICE_PATH "/dev/membuff"
#define LOG_DEVICE LOG_DISK // LOG_ETHERNET // LOG_CONSOLE // LOG_DISK

int main()
{
        int err, i, fd, count;
        char write_data[] = "0123456789";
        char key;

        DEBUG("opening buffer device file\n");
        fd = open(DEVICE_PATH, O_RDWR);
        assert(fd != -1);

        err = logger_init(LOG_DEVICE);
        assert(err == 0);

        while(1) {
                for(i=0; i<4; i++) {
                        DEBUG("writing data to buffer\n");
                        count = write(fd, write_data, sizeof(write_data)-1);
                        assert(count >= 0);
                        DEBUG("%d bytes written\n", count);
                }

                printf(">> execute a manual log, press ENTER! >");
                key = getchar();
                err = logger_manual_call();
                assert(err >= 0);
        }

        return 0;
}
