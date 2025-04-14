/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                           linux_eth_receive_log
 *
 *                                    c
 *
 * File 'linux_eth_receive_log.c'                                By Sangorrin
 *
 *
 * This module implements a program waiting to receive data from ethernet
 * from a specific protocol type and writes it to a file.
 *
 * Must be executed in Linux as ROOT
 *
 *---------------------------------------------------------------------------*/
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/ether.h>
#include <sys/utsname.h>
#include <net/if.h>
#include <fcntl.h>
#include <linux/if_packet.h>
#include <linux/if_ether.h>
#include <time.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>

#include "linux_eth_receive_log.h"

#if 1
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

/**
 * ETH_MIN_PAYLOAD is the minimum payload in an ethernet frame, if one sends
 *  less bytes the rest of the frame will be padded with zeros. To know when this
 *  situation happens we have added a flag in the first byte of each frame:
 *       - 0: if there is no padding (we sent data >= the 45)
 *       - 1-44: if there is padding (we sent data < 45)
 **/
#define ETH_MIN_PAYLOAD 46

typedef struct {
        struct ethhdr header;
        unsigned char less_than_min;
        unsigned char data[ETH_DATA_LEN - 1];
} __attribute__((__packed__)) eth_frame_t;

static FILE *fd;
static int sock;

void finish_handler(int x) {
        switch(x) {
                case SIGINT:
                        fprintf(stdout,"Finishing the logger ");
                        close(sock);
                        fclose(fd);
                        fprintf(stdout,"OK!\n");
                        exit(0);
                        break;
                default:
                        break;
        }
        return;
}

int main() {
        int err;
        char device[] = NIC_INTERFACE;
        struct sockaddr_ll host_addr;
        struct ifreq ifr;
        ssize_t nbytes;
        size_t written_bytes;
        eth_frame_t receive_buffer;

        DEBUG("setting up the socket\n");
        sock=socket(PF_PACKET,SOCK_RAW,htons(LOGGER_PROTOCOL));
        if (sock < 0) return -1;

        DEBUG("getting our MAC\n");
        memset(&ifr, 0, sizeof(struct ifreq));
        strcpy(ifr.ifr_name, device);

        err = ioctl(sock, SIOCGIFHWADDR, &ifr);
        if (err < 0) return -1;

        memcpy(host_addr.sll_addr, ifr.ifr_addr.sa_data, ETH_ALEN);

        DEBUG("getting our ethernet interface index\n");
        err = ioctl(sock, SIOGIFINDEX, &ifr);
        if (err < 0) return -1;

        host_addr.sll_ifindex = ifr.ifr_ifindex;

        DEBUG("initialize other fields from host_addr\n");
        host_addr.sll_family = AF_PACKET;
        host_addr.sll_protocol = htons(LOGGER_PROTOCOL);
        host_addr.sll_halen = ETH_ALEN;

        DEBUG("bind the socket to the host_addr\n");
        bind(sock, (struct sockaddr *)&host_addr, sizeof(host_addr));

        DEBUG("open the log file to write\n");
        fd = fopen(OUTPUT_FILE, "w+");

        DEBUG("install the signal handler for Ctrl + C\n");
        signal(SIGINT,finish_handler);

        DEBUG(">> Initialization done ;)\n");

        while(1) {
                DEBUG("receiving, ethernet protocol: 0x%X \n", LOGGER_PROTOCOL);
                nbytes = recvfrom(sock, &receive_buffer, sizeof(receive_buffer),
                                  0, NULL, 0);
                DEBUG("received %d bytes\n", nbytes);

                if (receive_buffer.less_than_min != 0) {
                        DEBUG("stripping %d header and %d padding bytes\n",
                              ETH_HLEN, ETH_MIN_PAYLOAD - 1 -
                                              receive_buffer.less_than_min);
                        nbytes = receive_buffer.less_than_min;
                } else {
                        DEBUG("stripping %d header\n", ETH_HLEN);
                        nbytes = nbytes - ETH_HLEN - 1;
                }

                DEBUG("writting %d bytes\n", nbytes);
                written_bytes = fwrite(&receive_buffer.data,
                                        sizeof(char),
                                        (size_t)nbytes,
                                        fd);
                if (written_bytes != nbytes) return -1;
        }

        return 0;
}
