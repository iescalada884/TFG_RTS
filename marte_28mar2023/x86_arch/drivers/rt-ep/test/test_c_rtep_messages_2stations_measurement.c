#include <pthread.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <misc/error_checks.h>
#include <unistd.h>
#include "rtep.h"
#include <assert.h>
#include <misc/time_measurement_hwtime.h>
#include <misc/logger.h>

void job1 (void);
void job2 (void);

static const trace_point_id_t BEGIN = 0;
static const trace_point_id_t END = 1;

int main ()
{
        int err;
        rtep_station_id_t me;

        err = logger_init(LOG_ETHERNET);
        assert(err == 0);

        err = time_measure_hwtime_init(BEGIN, "rtep_begin");
        assert(err == 0);

        err = time_measure_hwtime_init(END, "rtep_end");
        assert(err == 0);

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
                        sleep (10000);
        };

        // finalization
        rtep_adafinal();
        return 0;
}

void job1 (void)
{
        rtep_station_id_t station;
        uint8_t buffer[40];
        size_t last;
        rtep_priority_t prio;

        printf ("job1\n");

        while (1) {
                rtep_recv_info(&station, 4, buffer, sizeof(buffer),&last,&prio);
                rtep_send_info(station, 4, buffer, last, prio);
        }
}

void job2 (void)
{
        int err;
        rtep_station_id_t station;
        uint8_t buffer[40];
        size_t last;
        rtep_priority_t prio;
        rtep_station_id_t dest_station = 1;
        char msg_to_3[40];
        int size, i;

        printf ("job2\n");

        for (i=0; i<50; i++) {
                size = sprintf(msg_to_3, "msg %d", i);
                printf ("Sending msg %d, size: %d\n", i, size);

                time_measure_hwtime_set_timestamp(BEGIN);
                rtep_send_info(dest_station, 4, (uint8_t *)msg_to_3, size+1, 9);
                rtep_recv_info(&station, 4, buffer, sizeof(buffer),&last,&prio);
                time_measure_hwtime_set_timestamp(END);

                printf ("received answer from: %u with prio: %u last: %d data:",
                        station, prio, last);
                printf (" %s\n", buffer);
        }

        err = time_measure_hwtime_write_membuffer(BEGIN);
        assert(err == 0);

        err = time_measure_hwtime_write_membuffer(END);
        assert(err == 0);

        while (logger_manual_call() > 0);
}
