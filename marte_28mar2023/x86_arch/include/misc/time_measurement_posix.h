/*!
 * @file time_measurement_posix.h
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
#ifndef __MaRTE_OS_TIME_MEASUREMENT_POSIX_H__
#define __MaRTE_OS_TIME_MEASUREMENT_POSIX_H__

#include <time.h>

#define MX_TIME_MEASURE_NAME    40
#define MX_TIME_MEASURES        20
#define MX_TIME_MEASURE_MSG     40

typedef unsigned int time_measure_id_t;

int time_measure_posix_create(const char *name,
                              clockid_t clock_id,
                              time_measure_id_t *id);

int time_measure_posix_begin(time_measure_id_t id);

int time_measure_posix_end(time_measure_id_t id, char *msg);

#endif /* __MaRTE_OS_TIME_MEASUREMENT_POSIX_H__ */
