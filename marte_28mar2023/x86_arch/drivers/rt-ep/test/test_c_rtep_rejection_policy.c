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
                        sleep(10000);
        };

         // finalization
        rtep_adafinal();
        return 0;
}

/**
 * TEST 1
 *
 * Set rejection of the RX QUEUES. JOB1 will send messages quickly and JOB2
 * will receive in slowly so in the end, the buffer will have an overflow.
 * Change the policy constant to see what happens in both cases.
 *
 **/

void job1 (void)
{
        uint8_t buffer[40];
        printf ("job1\n");
        struct timespec delay = {0, 500000000};

        buffer[0] = 0;
        while (1) {
                nanosleep (&delay, NULL);
                rtep_send_info
                      (2,  // station
                       4,  // channel
                       buffer,
                       sizeof(buffer),
                       7); // priority
                buffer[0] = buffer[0] + 1;
        }
}

void job2 (void)
{
        rtep_station_id_t station;
        uint8_t buffer[100];
        size_t last;
        rtep_priority_t prio;
        struct timespec delay = {2, 0};

        printf ("job2\n");

        rtep_rx_queues_set_rejection_policy
              (4, // channel
               DISCARD_OLDEST);  // DISCARD_NEWEST

        while (1) {
                nanosleep (&delay, NULL);
                rtep_recv_info (&station,
                                 4, //  channel
                                 buffer,
                                 sizeof(buffer),
                                 &last,
                                 &prio);
                printf ("received %u from: %u with prio: %u last: %d \n",
                        buffer[0], station, prio, last);
        }
}
