// Test for:

// Not possible to execute automatically the test, it requires to
// install the driver membuffer_driver in kernel-devices_table.ads

/*!
 * @file test_membuffer_driver.c
 *
 * @brief Test for the Circular Memory Buffer driver
 *
 * @version 0.01
 *
 * @date 30 -Nov-2007
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * This module contains a test similar to the one for circular_memory_buffer but
 * for the driver wrapper. You have to install the driver membuffer_driver
 * in kernel-devices_table.ads
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <sys/marte_configuration_parameters.h>
#include <drivers/membuffer_driver.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#if 1
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define DEVICE_PATH "/dev/membuff"
#define BUFFER_LENGTH 10 /* bytes */

int main()
{
        int fd, err;
        int count;
        char write_data[BUFFER_LENGTH + 1] = "0123456789";
        char read_data[BUFFER_LENGTH + 1]  = "0000000000";

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        DEBUG("opening buffer device file\n");
        fd = open(DEVICE_PATH, O_RDWR);
        if (fd == -1) return -1;

        DEBUG("writing data to buffer\n");
        count = write(fd, write_data, BUFFER_LENGTH);
        if (count != BUFFER_LENGTH) return -1;

        DEBUG("reading the written data\n");
        count = read(fd, read_data, BUFFER_LENGTH);
        if (count != BUFFER_LENGTH) return -1;

        DEBUG("comparing the written data with the read data\n");
        err = strncmp(write_data, read_data, BUFFER_LENGTH);
        if (err != 0) return -1;

        DEBUG("closing file\n");
        err = close(fd);
        if (err == -1) return -1;

        printf("Test OK\n");
        return 0;
}
