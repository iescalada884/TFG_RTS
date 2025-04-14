/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                logger
 *
 *                                    c
 *
 * File 'logger.c'                                         By Sangorrin
 *
 *
 * This module implements a task logger that reads bytes from the driver
 * membuffer and writes them on a certain device with a given period.
 *
 * This device can be the console or ethernet and more devices could be
 * added in the future.
 *
 * You have to install the membuffer driver.
 *
 * We use a table to store the requisites for each logging device (it's
 * similar to do polimorphism in object oriented terminology)
 *
 *---------------------------------------------------------------------------*/
#include <pthread.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <misc/timespec_operations.h>
#include <misc/logger.h>

#if 0
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

/**
 * Function prototypes
 *
 */

static void *logger_thread_code (void *arg);

static int logger_console_init(void);
static int logger_console_log_data(char *logger_buffer, int nbytes);

static int logger_eth_init(void);
static int logger_eth_log_data(char *logger_buffer, int nbytes);

static int logger_disk_init(void);
static int logger_disk_log_data(char *logger_buffer, int nbytes);

/**
 * Static Variables
 *
 */
static bool is_initialized = false;            // initialization flag
static int mem_fd;                             // membuffer file desc
static struct timespec logger_period;          // the period of the thread
static char global_buffer[MAX_BYTES_TO_READ];  // buffer to read logging data
static enum log_device_id_t logger_device = 0; // the current log device

struct log_device_t {
        void *data;
        bool is_initialized;
        int (*init)(void);
        int (*log_data)(char *logger_buffer, int nbytes);
};

static struct log_device_t log_devices[3] = {
        // LOG_CONSOLE
        {.data = NULL,
         .is_initialized = false,
         .init = logger_console_init,
         .log_data = logger_console_log_data},
        // LOG_ETHERNET
        {.data = NULL,
         .is_initialized = false,
         .init = logger_eth_init,
         .log_data = logger_eth_log_data},
        // LOG_DISK
        {.data = NULL,
         .is_initialized = false,
         .init = logger_disk_init,
         .log_data = logger_disk_log_data}
};

/**
 * logger_init()
 *
 * sets the logging device and initializes internal data
 *
 */

int logger_init(enum log_device_id_t dev)
{
        int err = 0;

        if (is_initialized == false) {
                if (dev != LOG_CONSOLE) {
                        DEBUG("opening the membuffer device file to read\n");
                        mem_fd = open(MEMBUFFER_PATH, O_RDONLY);
                        if (mem_fd < 0) return -1;
                }
                is_initialized = true;
        }

        logger_device = dev;
        if (log_devices[logger_device].is_initialized == false) {
                DEBUG("initializing log device %d\n", logger_device);
                err = log_devices[logger_device].init();
                if (err == 0) {
                        log_devices[logger_device].is_initialized = true;
                }
        }

        return err;
}

/**
 * logger_thread_create()
 *
 * Creates a new thread that will read a maximum of 'MAX_BYTES_TO_READ' from
 * the file /dev/membuff and writes them on the logging device
 *
 */

int logger_thread_create(struct timespec *period)
{
        int err;
        pthread_t id;

        logger_period = *period;

        err = logger_init(logger_device);
        if (err != 0) return err;

        DEBUG("Creating logger\n");
        err = pthread_create(&id, NULL, logger_thread_code, NULL);
        if (err != 0) return err;

        return 0;
}

/**
 * logger_manual_call()
 *
 * Instead of having a periodic thread (through the previous operation)
 * collecting data, we could prefer to do it manually. That is what this
 * operation is intended for. When called it reads a maximum of
 * 'MAX_BYTES_TO_READ' from the file /dev/membuff and writes them on the
 * logging device
 *
 */

int logger_manual_call(void)
{
        int err;
        int nbytes;

        err = logger_init(logger_device);
        if (err != 0) return err;

        DEBUG("reading data\n");
        nbytes = read(mem_fd, global_buffer, MAX_BYTES_TO_READ);
        if (nbytes < 0) return -1;

        DEBUG("logging %d data bytes\n", nbytes);
        err = log_devices[logger_device].log_data(global_buffer, nbytes);
        if (err != 0) return err;

        return nbytes;
}

/**
 * logger_direct_call()
 *
 * Send the buffer contents directly to the log device without passing
 * through the membuffer.
 *
 */

int logger_direct_call(void *logger_buffer, int nbytes)
{
        int err;

        err = logger_init(logger_device);
        if (err != 0) return err;

        DEBUG("logging %d data bytes\n", nbytes);
        err = log_devices[logger_device].log_data((char *)logger_buffer,
                                                  nbytes);
        if (err != 0) return err;

        return nbytes;
}

/**
 * logger_thread_code()
 *
 * A loop to read a maximum of 'MAX_BYTES_TO_READ' from the file
 * /dev/membuff and writes them to the current logging device with
 * a given period.
 *
 */

static void *logger_thread_code (void *arg)
{
        int err;
        struct timespec next_activation;

        DEBUG("logger thread starts\n");

        err = clock_gettime(CLOCK_MONOTONIC, &next_activation);
        assert(err == 0);

        while (1) {
                incr_timespec (&next_activation, &logger_period);
                DEBUG("Sleeping until %d sec %d nsec\n",
                      next_activation.tv_sec, next_activation.tv_nsec);

                err = clock_nanosleep(CLOCK_MONOTONIC,
                                      TIMER_ABSTIME,
                                      &next_activation,
                                      NULL);
                assert(err == 0);

                err = logger_manual_call();
                assert(err >= 0);
        }

        return NULL;
}

/**
 * LOG_CONSOLE functions
 *
 * This are the functions specific for logging on the console:
 *
 *      - logger_console_init: no initialization needs
 *      - logger_console_log_data: prints logged data
 *
 */

static int logger_console_init(void) {
        return 0;
}

static int logger_console_log_data(char *logger_buffer, int nbytes)
{
        int i;

        for(i=0; i<nbytes; i++) {
                putchar(logger_buffer[i]);
        }
        putchar('\n');

        return 0;
}

/**
 * LOG_ETHERNET functions
 *
 * This are the functions specific for logging on the ethernet:
 *
 *      - logger_eth_init: opens the ethernet device and initializes the frame
 *                         with the MAC addresses and the protocol number
 *      - logger_eth_log_data: sends data through ethernet (there is a trick
 *                             here for the case when data bellow the minimum
 *                             payload is sent)
 *
 */

#include <drivers/if_ether.h>
#include <drivers/eth_ioctl.h>

#define LOG_ETHERNET_PATH "/dev/eth0"
#define LOG_ETHERNET_MAC  "FF:FF:FF:FF:FF:FF" // destination mac
#define LOGGER_PROTOCOL 0x1010

/* ETH_MIN_PAYLOAD is the minimum payload in an ethernet frame, if one sends
   less bytes the rest of the frame will be padded with zeros. To know when this
   situation happens we have added a flag in the first byte of each frame:
        - 0: if there is no padding (we sent data >= the 45)
        - 1-44: if there is padding (we sent data < 45)
*/
#define ETH_MIN_PAYLOAD 46

typedef struct {
        struct ethhdr header;
        unsigned char less_than_min;
        unsigned char data[ETH_DATA_LEN - 1];
} __attribute__((__packed__)) eth_frame_t;

struct log_eth_data_t {
        int fd;
        eth_frame_t eth_frame;
};

static struct log_eth_data_t log_eth_data;

static int logger_eth_init(void)
{
        int err, fd;
        struct ether_addr src_mac, dest_mac;

        fd = open(LOG_ETHERNET_PATH, O_WRONLY);
        if (fd < 0) return -1;

        err = ioctl(fd, ETH_HWADDR, src_mac.ether_addr_octet);
        if (err != 0) return -1;

        err = ether_aton(LOG_ETHERNET_MAC, &dest_mac);
        assert(err == 0);

        log_eth_data.fd = fd;
        *(struct ether_addr *)log_eth_data.eth_frame.header.h_dest = dest_mac;
        *(struct ether_addr *)log_eth_data.eth_frame.header.h_source = src_mac;
        log_eth_data.eth_frame.header.h_proto= LOGGER_PROTOCOL;

        log_devices[LOG_ETHERNET].data = (void *)&log_eth_data;

        return 0;
}

static int logger_eth_log_data(char *logger_buffer, int nbytes)
{
        int count;
        int tmp_nbytes;

        struct log_eth_data_t *data =
                (struct log_eth_data_t *)log_devices[LOG_ETHERNET].data;

        while (nbytes > 0) {

                if (nbytes > ETH_DATA_LEN - 1) {
                        tmp_nbytes = ETH_DATA_LEN - 1;
                } else {
                        tmp_nbytes = nbytes;
                }

                nbytes = nbytes - tmp_nbytes;

                memcpy(data->eth_frame.data, logger_buffer, tmp_nbytes);
                logger_buffer = logger_buffer + tmp_nbytes;

                if (tmp_nbytes < (ETH_MIN_PAYLOAD - 1)) {
                        data->eth_frame.less_than_min =
                                                (unsigned char)tmp_nbytes;
                } else {
                        data->eth_frame.less_than_min = 0;
                }

                DEBUG("sending %d bytes to %s\n", tmp_nbytes, LOG_ETHERNET_MAC);
                count = write(data->fd,
                              &data->eth_frame,
                              sizeof(data->eth_frame.header) + tmp_nbytes + 1);
                if (count != sizeof(data->eth_frame.header) + tmp_nbytes + 1)
                        return -1;
        }

        return 0;
}

/**
 * LOG_DISK functions
 *
 * This are the functions specific for logging on a file in a disk :
 *
 *      - logger_disk_init: open or create the file to log
 *      - logger_disk_log_data: write the logged data on disk
 *
 */

static int logger_disk_init(void)
{
        int fd;

        DEBUG("open or create the log file\n");

        fd = open(LOG_DISK_FILE, O_WRONLY | O_CREAT);
        if (fd == -1) {
                DEBUG("could not open or create log file\n");
        }

        log_devices[LOG_DISK].data = (void *)fd;

        return 0;
}

static int logger_disk_log_data(char *logger_buffer, int nbytes)
{
        int fd, count;

        DEBUG("loggin data to disk\n");

        fd = (int)log_devices[LOG_DISK].data;

        count = write(fd, logger_buffer, nbytes);
        if (count == -1) {
                DEBUG("could not write on log file\n");
        }

        return 0;
}
