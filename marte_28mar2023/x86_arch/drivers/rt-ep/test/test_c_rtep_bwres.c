#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdint.h>
#include <misc/error_checks.h>
#include <unistd.h>
#include <assert.h>
#include <misc/time_measurement_hwtime.h>
#include <misc/logger.h>

#include "rtep.h"
#include "rtep_bandwith_reservation.h"

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

static const trace_point_id_t BEGIN = 0;
static const trace_point_id_t END   = 1;

#define NO_CONTENTION
// #undef NO_CONTENTION

void *receiver_task (void *arg);

int main ()
{
        rtep_station_id_t me;
        rtep_station_id_t dest_station;
        char dest_station_name[] = "broadcast";
        char msg_broadcast[] = "Hi All!";
        rtep_bwres_contract_t contract;
        rtep_budget_t budget_min = 5;
        struct timespec period_max = {10, 0};
        struct timespec deadline = period_max;
        rtep_bwres_vres_t vres;
        rtep_server_id_t server;
        rtep_priority_t prio = 8;
        rtep_channel_t channel = 3;
        int accepted;
        pthread_t th;
        pthread_attr_t attr;
        int err;
        char trace_name[MX_TRACE_POINT_CHARS];

        /* initialization */
        rtep_adainit();
        err = rtep_bwres_init();
        assert(err == 0);

        me = rtep_get_station_id();
        DEBUG("I am: %u\n", me);

        err = pthread_attr_init(&attr);
        assert(err == 0);

        err = pthread_create (&th, &attr, receiver_task, NULL);
        assert(err == 0);

#ifdef NO_CONTENTION
        if (me != 1) {
                sleep(1000);
        }
#endif

        err = logger_init(LOG_ETHERNET);
        assert(err == 0);

        snprintf(trace_name, MX_TRACE_POINT_CHARS, "station%d_neg_begin", me);

        err = time_measure_hwtime_init(BEGIN, trace_name);
        assert(err == 0);

        snprintf(trace_name, MX_TRACE_POINT_CHARS, "station%d_neg_end", me);

        err = time_measure_hwtime_init(END, trace_name);
        assert(err == 0);

        /* negotiation */

        contract.budget_min = 69;

        err = rtep_bwres_contract_set_basic_params
                        (&contract, budget_min, &period_max, &deadline);
        assert(err == 0);

        err = rtep_bwres_set_priority (&contract, prio);
        assert(err == 0);

        time_measure_hwtime_set_timestamp(BEGIN);

        accepted = rtep_bwres_contract_negotiate (&contract, &vres);

        time_measure_hwtime_set_timestamp(END);

        err = time_measure_hwtime_write_membuffer(BEGIN);
        assert(err == 0);

        err = time_measure_hwtime_write_membuffer(END);
        assert(err == 0);

        while (logger_manual_call() > 0);

        if (accepted != 0) {
                DEBUG("contract not accepted\n");
                return -1;
        }
        DEBUG("contract accepted\n");

        DEBUG("server_id: %u\n", vres.server_id);

        err = rtep_bwres_get_server_id (&vres, &server);

        DEBUG("server: %u\n", server);

        /* sending messages */

        dest_station = rtep_get_station_id_by_name
                        ((uint8_t *)dest_station_name,
                         sizeof(dest_station_name)-1);

        while (1) {
                DEBUG("sending a message\n");
                rtep_server_send_info
                                (dest_station, channel,
                                 (uint8_t *)msg_broadcast,
                                 sizeof(msg_broadcast),
                                 server,
                                 0);
                sleep (2);
        }

        // finalization
        rtep_adafinal();
        return 0;
}

void *receiver_task (void *arg)
{
        rtep_station_id_t station;
        uint8_t buffer[20];
        size_t last;
        rtep_priority_t prio;

        DEBUG("receiver_task\n");
        while (1) {
                rtep_recv_info (&station, 3, buffer, sizeof(buffer), &last, &prio);
                DEBUG("received data from: %u  with prio: %u last: %d data: ",
                        station, prio, last);
                DEBUG("%s\n", buffer);
        }
}
