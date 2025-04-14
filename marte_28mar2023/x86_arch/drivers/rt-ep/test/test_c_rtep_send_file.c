/**
 * test_c_rtep_send_file.c
 *
 * Description: node 1 reads a file from the filesystem and sends it to node 2
 * Installation:
 *    - Install the FAT driver in marte-kernel-devices_table.ads
 *    - Create a FAT partition in the Compact Flash and put the file to read
 *      there (ie: file3)
 *    - make test_c_rtep_send_file.exe
 *    - run 'test_c_rtep_send_file.exe' in the rtep nodes
 */
#include <pthread.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <misc/error_checks.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include "rtep.h"

#if 0
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

void job1 (void);
void job2 (void);

int main ()
{
        rtep_station_id_t me;

        // initialization
        rtep_adainit();
        rtep_init_comm();

        // call the appropiate procedure
        me = rtep_get_station_id();
        switch (me) {
                case 1:
                        job1 ();
                        break;
                case 2:
                        job2 ();
                        break;
                default:
                        sleep(10000);
        };

         // finalization
        rtep_adafinal();
        return 0;
}

void job1 (void)
{
        int fd, ret;
        uint8_t buffer[1200];
        ssize_t read_bytes;
        struct timespec ts = {0, 3000000};
        rtep_priority_t   prio = 6;
        rtep_station_id_t dest = 2;
        rtep_channel_t    chan = 4;

        printf ("Job1\n");

        printf ("Opening file\n");

        fd = open ("/fat/file3", O_RDONLY);
        assert (fd >= 0);

        while( (read_bytes = read(fd, (void *)buffer, sizeof(buffer))) > 0) {
                DEBUG ("Sending %u bytes, dest=%u prio=%u chan=%u\n",
                        read_bytes, dest, prio, chan);
                rtep_send_info (dest, chan, buffer, read_bytes, prio);
                nanosleep (&ts, NULL);
        }

        printf ("Closing file\n");

        ret = close(fd);
        assert (ret == 0);
}

void job2 (void)
{
        uint8_t buffer[1200];
        rtep_priority_t   prio;
        rtep_station_id_t source;
        rtep_channel_t    chan = 4;
        size_t last;

        printf ("job2\n");

        while (1) {
                rtep_recv_info (&source, chan, buffer, sizeof(buffer), &last, &prio);
                DEBUG ("received data from: %u  with prio: %u last: %d\n",
                        source, prio, last);
        }
}
