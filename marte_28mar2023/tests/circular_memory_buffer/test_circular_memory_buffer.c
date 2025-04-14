// Test for all architectures
/*!
 * @file test_circular_memory_buffer.c
 *
 * @brief Test for Circular Memory Buffer
 *
 * @version 0.01
 *
 * @date 26-Nov-2007
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * This module contains a test for the implementation of the circular memory
 * buffer. It tests several cases:
 *
 *      1.- Normal operation (write, read, write, read ...)
 *      2.- Overwrite situations
 *      3.- Concurrency
 *
 * mgcc -c circular_memory_buffer.c
 * mgcc test_circular_memory_buffer.c circular_memory_buffer.o
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <misc/circular_memory_buffer.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#if 1
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define BUFFER_CAPACITY 10
#define BUFFER_CEILING  20

int main()
{
        int err, count;
        membuffer_t mbuff;
        char write_data[BUFFER_CAPACITY + 1] = "0123456789";
        char read_data [BUFFER_CAPACITY + 1] = "0000000000";
        char overflow_data[2 * BUFFER_CAPACITY + 1] = "00112233445566778899";

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        DEBUG("changing main prio to %d\n", BUFFER_CEILING - 1);
        err = pthread_setschedprio(pthread_self(), BUFFER_CEILING - 1);
        if (err != 0) return err;

        DEBUG("initialize membuffer\n");
        err = membuffer_init(&mbuff, BUFFER_CAPACITY, BUFFER_CEILING);
        if (err != 0) return err;

        DEBUG("write 5 bytes in membuffer\n");
        count = membuffer_write(&mbuff, write_data, 5);
        if (count != 5) return -1;
        DEBUG("written first %d bytes from %s\n", count, write_data);

        DEBUG("read from membuffer\n");
        count = membuffer_read(&mbuff, read_data, sizeof(read_data));
        if (count != 5) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("overflow the membuffer with %d bytes\n", 2 * BUFFER_CAPACITY);
        count = membuffer_write(&mbuff, overflow_data, 2 * BUFFER_CAPACITY);
        if (count != 2 * BUFFER_CAPACITY) return -1;
        DEBUG("overwritten bytes (warning must appear)\n");

        DEBUG("read from membuffer\n");
        count = membuffer_read(&mbuff, read_data, sizeof(read_data));
        if (count != BUFFER_CAPACITY) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("destroy the membuffer\n");
        err = membuffer_destroy(&mbuff);
        if (err != 0) return err;

        DEBUG("initialize again membuffer\n");
        err = membuffer_init(&mbuff, BUFFER_CAPACITY, BUFFER_CEILING);
        if (err != 0) return err;

        DEBUG("write %d bytes in membuffer\n", BUFFER_CAPACITY);
        count = membuffer_write(&mbuff, write_data, BUFFER_CAPACITY);
        if (count != BUFFER_CAPACITY) return -1;
        DEBUG("written first %d bytes from %s\n", count, write_data);

        DEBUG("read 5 bytes from membuffer\n");
        count = membuffer_read(&mbuff, read_data, 5);
        if (count != 5) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("read rest of bytes from membuffer\n");
        count = membuffer_read(&mbuff, read_data, BUFFER_CAPACITY);
        if (count != 5) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("read rest of bytes from membuffer\n");
        count = membuffer_read(&mbuff, read_data, BUFFER_CAPACITY);
        if (count != 0) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("destroy the membuffer\n");
        err = membuffer_destroy(&mbuff);
        if (err != 0) return err;


        DEBUG("initialize again membuffer\n");
        err = membuffer_init(&mbuff, BUFFER_CAPACITY, BUFFER_CEILING);
        if (err != 0) return err;

        DEBUG("write %d bytes in membuffer\n", 8);
        count = membuffer_write(&mbuff, write_data, 8);
        if (count != 8) return -1;
        DEBUG("written first %d bytes from %s\n", count, write_data);

        DEBUG("read 3 bytes from membuffer\n");
        count = membuffer_read(&mbuff, read_data, 3);
        if (count != 3) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("read 1 of bytes from membuffer\n");
        count = membuffer_read(&mbuff, read_data, 1);
        if (count != 1) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("write %d bytes in membuffer\n", 8);
        count = membuffer_write(&mbuff, write_data, 8);
        if (count != 8) return -1;
        DEBUG("written first %d bytes from %s\n", count, write_data);

        DEBUG("read rest of bytes from membuffer\n");
        count = membuffer_read(&mbuff, read_data, BUFFER_CAPACITY);
        if (count != BUFFER_CAPACITY) return -1;
        read_data[count] = '\0';
        DEBUG("read %d bytes: %s\n", count, read_data);

        DEBUG("destroy the membuffer\n");
        err = membuffer_destroy(&mbuff);
        if (err != 0) return err;

        DEBUG("TODO: TEST CONCURRENCY!!!\n");

        printf("Test OK\n\n");
        return 0;
}
