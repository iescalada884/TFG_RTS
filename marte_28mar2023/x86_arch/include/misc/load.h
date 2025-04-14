/*
 *
 * Functions to "eat" CPU-time.
 * The amount of time is measured using CPU-time clocks.
 *
 */

#ifndef __MARTE_MISC_LOAD_H__
#define __MARTE_MISC_LOAD_H__

#include <time.h>
#include <misc/timespec_operations.h>

/*
 * eat_ts
 *
 * Executes during the interval of time 'cpu_time' 
 */
static inline void eat_ts(const struct timespec *cpu_time)
{
    struct timespec current_time, time_to_go;

    (void)clock_gettime(CLOCK_THREAD_CPUTIME_ID, &current_time);
    add_timespec(&time_to_go, &current_time, cpu_time);

    while (smaller_timespec(&current_time, &time_to_go)) {
        (void)clock_gettime(CLOCK_THREAD_CPUTIME_ID, &current_time);
    }
}

/*
 * eat
 *
 * Executes during the interval of time 'For_Seconds' 
 */
static inline void eat (float for_seconds)
{
  struct timespec time;
  double_to_timespec(for_seconds, &time);
  eat_ts(&time);
}

/*
 * adjust
 * 
 * Only for compatibility with load_loop.c
 */
static inline void adjust (void)
{
}

#endif // __MARTE_MISC_LOAD_H__
