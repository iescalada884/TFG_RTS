/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                  logger
 *
 *                                    h
 *
 * File 'logger.h'                                         By Sangorrin
 *
 *
 * This module implements a task logger that reads bytes from the driver
 * membuffer and writes them on a certain device with a given period.
 *
 * This device can be the console, ethernet, a disk, ...
 *
 * You have to install the membuffer driver.
 *
 *---------------------------------------------------------------------------*/
#ifndef __MARTE_OS_LOGGER__
#define __MARTE_OS_LOGGER__

#include <time.h>

#define MAX_BYTES_TO_READ 1000 // amount of bytes to read each time
#define MEMBUFFER_PATH    "/dev/membuff"
#define LOG_DISK_FILE     "/fat/log"

enum log_device_id_t {
        LOG_CONSOLE  = 0,
        LOG_ETHERNET = 1,
        LOG_DISK     = 2
};

/**
 * logger_init()
 *
 * sets the logging device and initializes internal data
 *
 */

extern int logger_init(enum log_device_id_t dev);

/**
 * logger_thread_create()
 *
 * Creates a new thread that will read a maximum of 'MAX_BYTES_TO_READ' from
 * the file /dev/membuff and writes them on the logging device
 *
 */

extern int logger_thread_create(struct timespec *period);

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

extern int logger_manual_call(void);

/**
 * logger_direct_call()
 *
 * Send the buffer contents directly to the log device without passing
 * through the membuffer.
 *
 */

extern int logger_direct_call(void *logger_buffer, int nbytes);

#endif  // __MARTE_OS_LOGGER__
