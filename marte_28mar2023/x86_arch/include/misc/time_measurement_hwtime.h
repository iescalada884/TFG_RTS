/*!
 * @file time_measurement_hwtime.h
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
#ifndef __MaRTE_OS_TIME_MEASUREMENT_HWTIME_H__
#define __MaRTE_OS_TIME_MEASUREMENT_HWTIME_H__

#define MX_TRACE_POINTS               20
#define MX_TRACE_POINT_CHARS          50
#define MX_TIMESTAMPS_PER_ID        1000

typedef unsigned int trace_point_id_t;     // 0 .. MX_TRACE_POINTS - 1

extern int time_measure_hwtime_init(trace_point_id_t id,
                                    const char *name);

extern void inline time_measure_hwtime_set_timestamp(trace_point_id_t id);

extern int time_measure_hwtime_write_membuffer(trace_point_id_t id);

#endif /* __MaRTE_OS_TIME_MEASUREMENT_HWTIME_H__ */
