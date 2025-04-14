/*!
 * @file circular_memory_buffer.c
 *
 * @brief Circular Memory Buffer
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
 * The implementation is very simple and consists in moving the read and write
 * pointers correctly. See the operations write and read.
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include "circular_memory_buffer.h"
#include <stdlib.h>  /* for malloc, free*/
#include <stdbool.h> /* for bool */

#if 0
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

/**
 * membuffer_init()
 *
 * Allocate the buffer with the capacity indicated as a parameter and initialize
 * the values for the read and write pointers, the mutex (using the ceiling) and
 * the rest of values. Check that the capacity is larger than the WARNING
 * message.
 *
 */

int membuffer_init(membuffer_t *mbuff, int capacity, int ceiling)
{
        int err;
        pthread_mutexattr_t attr;

        DEBUG("allocating %d bytes for the buffer\n", capacity);
        mbuff->buffer = (uint8_t *)malloc(sizeof(uint8_t) * capacity);
        if (mbuff->buffer == NULL) return -1;

        DEBUG("initialize pointers and variables of the mbuffer\n");
        mbuff->write_index = 0;
        mbuff->read_index  = 0;
        mbuff->capacity    = capacity;
        mbuff->count       = 0;

        DEBUG("initialize the mutex for the mbuffer\n");

        err = pthread_mutexattr_init(&attr);
        if (err != 0) return err;

        err = pthread_mutexattr_setprotocol(&attr, PTHREAD_PRIO_PROTECT);
        if (err != 0) goto destroy_attr;

        err = pthread_mutexattr_setprioceiling(&attr, ceiling);
        if (err != 0) goto destroy_attr;

        err = pthread_mutex_init(&(mbuff->mutex), &attr);

destroy_attr:
        pthread_mutexattr_destroy(&attr);
        return err;
}

/**
 * membuffer_write()
 *
 * Write the bytes in the buffer:
 *
 *      1.- LOCK the mutex
 *      2.- Write the bytes in the buffer
 *      3.- In case of overflow adjust read_index pointer and count
 *      4.- UNLOCK the mutex
 *
 */

int membuffer_write(membuffer_t *mbuff, const void *data, int len)
{
        int err, i;
        bool overflow;

        err = pthread_mutex_lock(&mbuff->mutex);
        if (err != 0) return err;

        overflow = len > (mbuff->capacity - mbuff->count);

        // TODO: if overflow we don't need to write all bytes
        for (i=0; i<len; i++) {
                mbuff->buffer[mbuff->write_index] = ((uint8_t *)data)[i];
                mbuff->write_index = (mbuff->write_index + 1) % mbuff->capacity;
        }

        if (overflow) {
                DEBUG("BUFFER OVERFLOW: overwriting data\n");
                mbuff->read_index = mbuff->write_index;
                mbuff->count = mbuff->capacity;
        } else {
                mbuff->count = mbuff->count + len;
        }

        err = pthread_mutex_unlock(&mbuff->mutex);
        if (err != 0) return err;

        return len;
}

/**
 * membuffer_read()
 *
 * Read bytes from the buffer:
 *
 *      1.- LOCK the mutex
 *      2.- set number of bytes to read
 *      3.- read the bytes
 *      4.- adjust count
 *      5.- UNLOCK the mutex
 *
 */

int membuffer_read(membuffer_t *mbuff, void *data, int len)
{
        int err, i;
        int bytes_to_read;
        uint8_t *buff = (uint8_t *)data;

        err = pthread_mutex_lock(&mbuff->mutex);
        if (err != 0) return err;

        if (len > mbuff->count) {
                bytes_to_read = mbuff->count;
        } else {
                bytes_to_read = len;
        }

        for(i=0; i<bytes_to_read; i++) {
                buff[i] = mbuff->buffer[mbuff->read_index];
                mbuff->read_index = (mbuff->read_index + 1) % mbuff->capacity;
        }

        mbuff->count = mbuff->count - bytes_to_read;

        err = pthread_mutex_unlock(&mbuff->mutex);
        if (err != 0) return err;

        return bytes_to_read;
}

/**
 * membuffer_destroy()
 *
 * Free the mutex and the memory allocated for the buffer.
 *
 */

int membuffer_destroy(membuffer_t *mbuff)
{
        int err;

        DEBUG("destroying mbuffer mutex\n");
        err = pthread_mutex_destroy(&mbuff->mutex);
        if (err != 0) return err;

        DEBUG("freeing mbuffer memory\n");
        free(mbuff->buffer);

        DEBUG("reseting internal values\n");
        mbuff->write_index = 0;
        mbuff->read_index  = 0;
        mbuff->capacity    = 0;
        mbuff->count       = 0;

        return 0;
}
