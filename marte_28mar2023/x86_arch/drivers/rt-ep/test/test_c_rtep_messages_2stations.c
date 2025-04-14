#include <pthread.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <misc/error_checks.h>
#include <unistd.h>
#include "rtep.h"

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
                        printf ("me: %d\n", me);
                        sleep(10000);
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
                rtep_recv_info (&station, 4, buffer, sizeof(buffer), &last, &prio);
                printf ("received data from: %u  with prio: %u last: %d data: ",
                        station, prio, last);
                printf ("%s\n", buffer);
                rtep_send_info (station, 4, buffer, last, prio);
        }
}

void job2 (void)
{
        rtep_station_id_t station;
        uint8_t buffer[40];
        size_t last;
        rtep_priority_t prio;
        rtep_station_id_t dest_station = 1;
        char msg_to_3[40];
        int size, i;
        //    struct timespec delay = {0, 500000000};

        printf ("job2\n");

        for (i=0; 1; i++) {
                size = sprintf(msg_to_3, "msg %d", i);
                printf ("Sending msg %d, size: %d\n", i, size);
                rtep_send_info (dest_station, 4, (uint8_t *)msg_to_3, size+1, 9);
                /*      nanosleep (&delay, NULL);*/
                rtep_recv_info (&station, 4, buffer, sizeof(buffer), &last, &prio);
                printf ("received answer from: %u  with prio: %u last: %d data: ",
                        station, prio, last);
                printf ("%s\n", buffer);   }
}
