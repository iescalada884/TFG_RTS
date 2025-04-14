/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                      test_time_measurement_hwtime_ethernet
 *
 *                                    c
 *
 * File 'test_time_measurement_hwtime_ethernet.c'                    By Sangorrin
 *
 *
 * Test for the time_measurement_hwtime module and ethernet.
 * We get 100 measures of a packet go-return trip and then we
 * log them manually
 *
 *---------------------------------------------------------------------------*/
#include <assert.h>
#include <drivers/if_ether.h>
#include <drivers/eth_ioctl.h>
#include <drivers/osdep.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <misc/time_measurement_hwtime.h>
#include <misc/logger.h>

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

typedef struct {
        struct ethhdr header;
        unsigned char data[ETH_DATA_LEN];
} eth_frame_t;

#define ETH_PROTOCOL 0xA000

#define STATION_1  "00:30:64:04:EA:AB" // "00:0E:0C:5B:1E:28"
#define STATION_2 "00:30:64:05:74:73"

#define MX_MEASURES 10

static const trace_point_id_t BEGIN = 0;
static const trace_point_id_t END = 1;

int main()
{
        int err, i, fd, count;
        eth_frame_t eth_frame, read_frame;
        struct ether_addr src_mac, dest_mac;
        bool is_station1;
        char mac_str[18];
        char msg[] = "hello world";
        unsigned short protocol = ETH_PROTOCOL;

        err = logger_init(LOG_ETHERNET);
        assert(err == 0);

        err = time_measure_hwtime_init(BEGIN, "eth_begin");
        assert(err == 0);

        err = time_measure_hwtime_init(END, "eth_end");
        assert(err == 0);

        fd = open("/dev/eth0", O_RDWR);
        assert(fd != -1);

        err = ioctl(fd, ETH_HWADDR, src_mac.ether_addr_octet);
        assert(err == 0);

        err = ioctl(fd, SET_PROTOCOL_FILTER, (void *)&protocol);
        assert(err == 0);

        ether_ntoa (&src_mac, mac_str);
        DEBUG("I am %s\n", mac_str);

        err = ether_aton(STATION_1, &dest_mac);
        assert(err == 0);

        is_station1 = (memcmp(&dest_mac, &src_mac, sizeof(src_mac)) == 0);

        if (is_station1) {
                err = ether_aton(STATION_2, &dest_mac);
                assert(err == 0);

                *(struct ether_addr * )eth_frame.header.h_dest = dest_mac;
                *(struct ether_addr * )eth_frame.header.h_source = src_mac;
                eth_frame.header.h_proto= htons(ETH_PROTOCOL);
                strncpy((char *)eth_frame.data, msg, sizeof(msg));

                ether_ntoa (&dest_mac, mac_str);

                // Sometims the card didnt get the first packet, but be careful
                // because if it gets it, then all the measures will be wrong!!
                // DEBUG("sending bytes to %s\n", mac_str);
                // count = write(fd, &eth_frame,
                //                 sizeof(struct ethhdr) + sizeof(msg));
                // assert (count > 0);

                for(i=0; i<MX_MEASURES; i++) {
                        DEBUG("sending bytes to %s\n", mac_str);

                        time_measure_hwtime_set_timestamp(BEGIN);

                        count = write(fd, &eth_frame,
                                      sizeof(struct ethhdr) + sizeof(msg));
                        count = read(fd, &read_frame, ETH_FRAME_LEN);

                        time_measure_hwtime_set_timestamp(END);

                        ether_ntoa ((struct ether_addr *)
                                        &read_frame.header.h_source, mac_str);
                        DEBUG("received %d bytes from %s: %s\n",
                              count, mac_str, read_frame.data);
                        sleep(1);
                }

                for(i=0; i<6; i++) {
                        err = time_measure_hwtime_write_membuffer(i);
                        assert(err == 0);
                }

                while (logger_manual_call() > 0);

        } else {
                *(struct ether_addr * )eth_frame.header.h_dest = dest_mac;
                *(struct ether_addr * )eth_frame.header.h_source = src_mac;
                eth_frame.header.h_proto= htons(ETH_PROTOCOL);
                strncpy((char *)eth_frame.data, msg, sizeof(msg));

                while(1) {
                        count = read(fd, &read_frame, ETH_FRAME_LEN);
                        count = write(fd, &eth_frame,
                                      sizeof(struct ethhdr) + sizeof(msg));
                }
        }

        return 0;
}
