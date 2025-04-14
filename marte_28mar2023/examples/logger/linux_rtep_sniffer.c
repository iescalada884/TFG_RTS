/**
 * linux_rtep_sniffer.c
 *
 * Sniffer for RTEP aimed at extracting certain packets from the network
 * and writting into a FIFO. The packets that will be extracted are
 * defined by using the constants:
 *
 *      SNIFFER_TYPE, SNIFFER_DEST, SNIFFER_CHAN
 *
 * The sniffed data can be either output to a file (USE_OUTPUT_FILE == 1)
 * or written to a FIFO (that has to be created $ mkfifo /tmp/movie)
 */
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
#include <stdint.h>
#include <assert.h>

#define RTEP_PROTOCOL 0xA000
#define OUTPUT_FILE "output_file.log"
#define NIC_INTERFACE "eth0"

#define RTEP_TYPE_INFO                  0x49  /* 'I'=0x49 */
#define RTEP_TYPE_REGULAR_TOKEN         0x54  /* 'T'=0x54 */
#define RTEP_TYPE_TRANSMIT_TOKEN        0x50  /* 'P'=0x50 */
#define RTEP_TYPE_ACK                   0x41  /* 'A'=0x41 */
#define RTEP_TYPE_REQUEST               0x52  /* 'R'=0x52 */

#define SNIFFER_TYPE RTEP_TYPE_INFO
#define SNIFFER_DEST 2
#define SNIFFER_CHAN 4

#if 0
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define USE_OUTPUT_FILE 0

/**
 * ETH_MIN_PAYLOAD is the minimum payload in an ethernet frame, if one sends
 *  less bytes the rest of the frame will be padded with zeros.
 */
#define ETH_MIN_PAYLOAD 46

typedef struct {
        struct ethhdr header;
        uint16_t dest;
        uint8_t  type;
        uint8_t  prio;
        uint16_t packet_number;
        uint16_t chan;
        uint16_t len;
        unsigned char data[ETH_DATA_LEN - 10];
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
        int err, fifo;
        char device[] = NIC_INTERFACE;
        struct sockaddr_ll host_addr;
        struct ifreq ifr;
        ssize_t nbytes;
        size_t written_bytes;
        eth_frame_t receive_buffer;

        DEBUG("setting up the socket\n");
        sock=socket(PF_PACKET,SOCK_RAW,htons(RTEP_PROTOCOL));
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
        host_addr.sll_protocol = htons(RTEP_PROTOCOL);
        host_addr.sll_halen = ETH_ALEN;

        DEBUG("bind the socket to the host_addr\n");
        bind(sock, (struct sockaddr *)&host_addr, sizeof(host_addr));

        DEBUG("open the file to write\n");

        // comment if the FIFO is created from the console with mkfifo
        // err = mkfifo("/tmp/movie", S_IWUSR | S_IRUSR | S_IRGRP | S_IROTH);
        // assert (err == 0);

#if USE_OUTPUT_FILE
        fd = fopen(OUTPUT_FILE, "w+");
        assert (fd >= 0);
#else
        fifo = open("/tmp/movie", O_WRONLY);
        assert (fifo >= 0);
#endif

        DEBUG("install the signal handler for Ctrl + C\n");
        signal(SIGINT,finish_handler);

        DEBUG(">> Initialization done ;)\n");

        while(1) {
                DEBUG("receiving, ethernet protocol: 0x%X \n", RTEP_PROTOCOL);
                nbytes = recvfrom(sock, &receive_buffer, sizeof(receive_buffer),
                                  0, NULL, 0);
                DEBUG("received %d bytes, dest:%u, type:%X\n",
                      nbytes, receive_buffer.dest, receive_buffer.type);

                if (receive_buffer.dest != SNIFFER_DEST) continue;
                if (receive_buffer.type != SNIFFER_TYPE) continue;
                if (receive_buffer.chan != SNIFFER_CHAN) continue;

                DEBUG("prio: %u packet: %u chan: %u len: %u\n",
                      receive_buffer.prio,
                      receive_buffer.packet_number,
                      receive_buffer.chan,
                      receive_buffer.len);

                DEBUG("writting %d bytes\n", receive_buffer.len);

#if USE_OUTPUT_FILE
                written_bytes = fwrite(&receive_buffer.data,
                                        sizeof(char),
                                        (size_t)receive_buffer.len,
                                        fd);
                if (written_bytes != receive_buffer.len) {
                        DEBUG("could not write all the bytes\n");
                        return -1;
                }
#else
                written_bytes = write(fifo,
                                      (void *)receive_buffer.data,
                                      (size_t)receive_buffer.len);
#endif
        }

        return 0;
}
