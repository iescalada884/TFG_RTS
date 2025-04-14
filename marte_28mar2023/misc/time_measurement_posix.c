/*!
 * @file time_measurement_posix.c
 *
 * @brief Library to take time measurements using posix clocks
 *
 * @version 0.01
 *
 * @date 3-Dic-2007
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * This module contains the implementation of small library to perform simple
 * time measures. Internally, the module writes the measures in the Circular
 * Memory Buffer driver so for example a logger (see dir examples/) can take
 * those measures and write them into the appropiate device (a disk, put them
 * on the console, send them trough ethernet to a linux host, etc.). It uses
 * the posix function clock_gettime to obtain the time measurements.
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <time.h>
#include <stdbool.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include "time_measurement_posix.h"

#if 0
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define MEMBUFFER_PATH    "/dev/membuff"

/**
 * Static variables
 *
 */

static bool is_initialized = false;
static int fd; // memory buffer file descriptor

struct time_measure_t {
        clockid_t clock_id;
        char name[MX_TIME_MEASURE_NAME + 1];
        struct timespec begin, end;
        bool is_used;
};

static struct time_measure_t time_measurements_pool[MX_TIME_MEASURES];

/**
 * The format for the measurement messages is described bellow. First we put
 * the name of the measurement and then we put the begin and end of the
 * measurement in a timespec format (seconds, nanoseconds).
 *
 *"measurement_name:(0123456789,111222333)->(0123456789,111222333)\n"
 * MX_TIME_MEASURE_NAME + 1 + 10 + 1 + 9 + 4 + 10 + 9 + 2
 **/

#define TIME_MEASURE_MESSAGE_SIZE MX_TIME_MEASURE_MSG + MX_TIME_MEASURE_NAME + 46
static char time_measure_message[TIME_MEASURE_MESSAGE_SIZE + 1];

/**
 * time_measurement_init()
 *
 * Internal function to initialize internal structures.
 *
 */

static inline int time_measurement_init()
{
        time_measure_id_t id;

        if (is_initialized == false) {
                for (id=0; id<MX_TIME_MEASURES; id++) {
                        time_measurements_pool[id].is_used = false;
                }

                fd = open(MEMBUFFER_PATH, O_WRONLY);
                if (fd == -1) return -1;

                is_initialized = true;
        }
        return 0;
}

/**
 * time_measurement_get_new_id()
 *
 * Internal function to get a new free id from the pool. We could use a freelist
 * to get O(1) timings but this way is simpler and it does not affect the
 * measures.
 *
 * TODO: possible race condition when there are two concurrent calls.
 *
 */

static int time_measurement_get_new_id(time_measure_id_t *id)
{
        time_measure_id_t tmp;

        for (tmp=0; tmp<MX_TIME_MEASURES; tmp++) {
                if (time_measurements_pool[tmp].is_used == false) {
                        *id = tmp;
                        time_measurements_pool[tmp].is_used = true;
                        return 0;
                }
        }
        return -1;
}

/**
 * time_measure_posix_create()
 *
 * Initializes the fields of a new time_measure
 *
 */

int time_measure_posix_create(const char *name,
                              clockid_t clock_id,
                              time_measure_id_t *id)
{
        int err;
        struct time_measure_t *tm;

        err = time_measurement_init();
        if (err != 0) return err;

        err = time_measurement_get_new_id(id);
        if (err != 0) return err;

        tm = &(time_measurements_pool[*id]);

        tm->clock_id = clock_id;
        strncpy(tm->name, name, MX_TIME_MEASURE_NAME);
        tm->name[MX_TIME_MEASURE_NAME] = '\0';

        return 0;
}

/**
 * time_measure_posix_begin()
 *
 * Takes a time stamp for the begin of the time measurement
 *
 */

int time_measure_posix_begin(time_measure_id_t id)
{
        int err;
        struct time_measure_t *tm = &(time_measurements_pool[id]);

        err = clock_gettime(tm->clock_id, &tm->begin);
        if (err != 0) return err;

        return 0;
}

/**
 * time_measure_posix_end()
 *
 * Takes a time stamp for the end of the time measurement and puts it in
 * the circular memory buffer driver.
 *
 */

int time_measure_posix_end(time_measure_id_t id, char *msg)
{
        int err, count;
        struct time_measure_t *tm = &(time_measurements_pool[id]);

        err = clock_gettime(tm->clock_id, &tm->end);
        if (err != 0) return err;

        DEBUG("writing measure to buffer\n");

        if (msg == NULL) {
                count = snprintf(time_measure_message,
                                TIME_MEASURE_MESSAGE_SIZE,
                                "%s:(%d,%d)->(%d,%d)\n",
                                tm->name,
                                tm->begin.tv_sec, tm->begin.tv_nsec,
                                tm->end.tv_sec, tm->end.tv_nsec);
        } else {
                count = snprintf(time_measure_message,
                                 TIME_MEASURE_MESSAGE_SIZE,
                                 "%s_%s:(%d,%d)->(%d,%d)\n",
                                 tm->name,
                                 msg,
                                 tm->begin.tv_sec, tm->begin.tv_nsec,
                                 tm->end.tv_sec, tm->end.tv_nsec);
        }

        count = write(fd, time_measure_message, count);
        if (count < 0) return -1;

        return 0;
}
