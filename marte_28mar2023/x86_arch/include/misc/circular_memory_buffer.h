/*!
 * @file circular_memory_buffer.h
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
 * This module contains the implementation of a circular memory buffer to
 * write and read bytes. It is typically aimed at logging applications that
 * need to write and read information with a certain period.
 *
 * If the writing pace is faster than the reading it may happen (because of a
 * bad real-time design) that the limit of the buffer is achieved. In this
 * situation the module overwrites the contents.
 *
 * The module provides mutual exclusion through the use of mutexes.
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#ifndef CIRCULAR_MEMORY_BUFFER_H
#define CIRCULAR_MEMORY_BUFFER_H

#include <stdint.h>  /* for uint8_t */
#include <pthread.h> /* for pthread_mutex_t */

typedef struct {
        uint8_t *buffer;
        int write_index;
        int read_index;
        int capacity;
        int count;
        pthread_mutex_t mutex;
} membuffer_t;

int membuffer_init(membuffer_t *mbuff, int capacity, int ceiling);
int membuffer_write(membuffer_t *mbuff, const void *data, int len);
int membuffer_read(membuffer_t *mbuff, void *data, int len);
int membuffer_destroy(membuffer_t *mbuff);

#endif /* CIRCULAR_MEMORY_BUFFER_H */
