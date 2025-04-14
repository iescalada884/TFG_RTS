/*!
 * @file time_measurement_hwtime.c
 *
 * @brief Library to take time measurements using more direct functions
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
 * This module contains the implementation of a small library to perform
 * time measures. I have tried to implemment them with the minimum overhead.
 * The interface is different from the time_measurement_posix. The model
 * here is:
 *
 * - We store timestamps in "points of code" identified by an id
 * -
 * The IDs of the "points of code" must be configured by the user (for example,
 * use define flags).
 *
 * Once the measures are taken there is a function to write them in the
 * Circular Memory Buffer driver so for example a logger (see dir examples/)
 * can take those measures and write them into the appropiate device
 * (a disk, put them on the console, send them trough ethernet to a linux
 * host, etc.).
 *
 * TODO: Add multiple tasks support
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

#include "time_measurement_hwtime.h"

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

typedef long long unsigned hwtime_t;
typedef unsigned int timestamp_index_t;

struct trace_point_t {
        char name[MX_TRACE_POINT_CHARS + 1];
        hwtime_t timestamps[MX_TIMESTAMPS_PER_ID];
        timestamp_index_t index;
};

static struct trace_point_t trace_points[MX_TRACE_POINTS];

/**
 * The format for the measurement messages is described bellow. First we put
 * the name of the trace point with a [i] indicating the subid of the timestamp.
 * Then we put the timestamp in second - nanoseconds
 *
 *"measurement_name[i]: 0123456789s 123456789ns\n"
 * MX_TRACE_POINT_CHARS + 1 + 10 + 3 + 10 + 2 + 9 + 2 + 1
 **/

#define MX_MESSAGE_SIZE  MX_TRACE_POINT_CHARS + 38
static char time_measure_message[MX_MESSAGE_SIZE];

/**
 * initialization()
 *
 * Internal function to initialize internal structures.
 *
 */

static int initialization()
{
        if (is_initialized == false) {
                DEBUG("opening membuff\n");
                fd = open(MEMBUFFER_PATH, O_WRONLY);
                if (fd == -1) return -1;

                is_initialized = true;
        }
        return 0;
}

/**
 * time_measure_hwtime_init()
 *
 * Initializes the fields of a new trace_point
 *
 */

int time_measure_hwtime_init(trace_point_id_t id,
                              const char *name)
{
        int err;
        struct trace_point_t *tp;
        timestamp_index_t ts;

        err = initialization();
        if (err != 0) return err;

        DEBUG("initializing trace point %u\n", id);

        tp = &trace_points[id];

        strncpy(tp->name, name, MX_TRACE_POINT_CHARS);
        tp->name[MX_TRACE_POINT_CHARS] = '\0';

        tp->index = 0;

        for(ts=0; ts<MX_TIMESTAMPS_PER_ID; ts++) {
                tp->timestamps[ts] = 0;
        }

        return 0;
}

/**
 * time_measure_hwtime_set_timestamp()
 *
 * Sets a timestamp for a trace_point
 *
 */

#define rdtscll(val) \
     __asm__ __volatile__("rdtsc" : "=A" (val))

void inline time_measure_hwtime_set_timestamp(trace_point_id_t id)
{
        struct trace_point_t *tp = &trace_points[id];

        DEBUG("setting timestamp %u for %s\n", tp->index, tp->name);
        rdtscll(tp->timestamps[tp->index]);
        tp->index++;
}

/**
 * time_measure_hwtime_write_membuffer()
 *
 * dumps the timestamps of a trace_point in the buffer so it can be logged
 *
 */

#define NS_PER_S 1000000000
extern hwtime_t marte__hal__get_hwclock_frequency ();
#define HWT_HZ marte__hal__get_hwclock_frequency()

int time_measure_hwtime_write_membuffer(trace_point_id_t id)
{
        int count;
        timestamp_index_t ts;
        hwtime_t s, ns, hwt;
        struct trace_point_t *tp = &trace_points[id];

        DEBUG("writing measures for %s to buffer\n", tp->name);

        for (ts = 0; ts < tp->index; ts++) {
                hwt = tp->timestamps[ts];
                s = hwt / HWT_HZ;
                ns = ((hwt%HWT_HZ) * NS_PER_S) / HWT_HZ;

                count = snprintf(time_measure_message,
                                 MX_MESSAGE_SIZE,
                                 "%s[%2u]: %lus %9luns\n",
                                 tp->name,
                                 ts,
                                 (unsigned long)s,
                                 (unsigned long)ns);

                count = write(fd, time_measure_message, count);
                if (count < 0) return -1;
        }

        tp->index = 0; // reset index

        return 0;
}
