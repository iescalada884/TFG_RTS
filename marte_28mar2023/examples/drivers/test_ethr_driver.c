/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                test_ethr_driver
 *
 *                                    c
 *
 * File 'test_ethr_driver.c'                                   By Sangorrin
 *
 *
 * Simple test that sends and receives data through the ethernet drivers
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

#define LOGGER_PROTOCOL 0x1010

#define STATION_1 "00:0E:0C:5B:1E:28"
#define STATION_2 "FF:FF:FF:FF:FF:FF" // "00:30:64:05:74:73"

int main()
{
        int err, fd, count;
        eth_frame_t eth_frame, read_frame;
        struct ether_addr src_mac, dest_mac;
        bool is_station1;
        char mac_str[18];
        char msg[] = "hello world";
        unsigned short protocol = LOGGER_PROTOCOL;

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
                eth_frame.header.h_proto= htons(LOGGER_PROTOCOL);
                strncpy(eth_frame.data, msg, sizeof(msg));

                ether_ntoa (&dest_mac, mac_str);

                while(1) {
                        DEBUG("sending bytes to %s\n", mac_str);
                        count = write(fd, &eth_frame, sizeof(eth_frame));
                        assert (count > 0);
                        sleep(1);
                }
        } else {
                while(1) {
                        DEBUG("receiving bytes\n");
                        count = read(fd, &read_frame, ETH_FRAME_LEN);
                        assert (count > 0);
                        ether_ntoa (&read_frame.header.h_source, mac_str);
                        DEBUG("received %d bytes from %s: %s\n",
                              count, mac_str, read_frame.data);
                }
        }

        return 0;
}
