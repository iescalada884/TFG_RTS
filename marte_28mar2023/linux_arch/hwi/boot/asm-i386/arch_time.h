#ifndef ARCH_TIME_H_
#define ARCH_TIME_H
#include <time.h>
typedef long long hrtime_t;

#define NSECS_PER_USEC 1000
#define NSECS_PER_MSEC NSECS_PER_USEC * 1000
#define NSECS_PER_SEC  NSECS_PER_MSEC * 1000
#define USECS_PER_SEC  NSECS_PER_SEC/NSECS_PER_USEC

extern inline hrtime_t timespec_to_ns (const struct timespec *ts)
{
        long long t;

        __asm__("imull %%edx\n"
                "add %%ebx, %%eax\n\t"
                "adc $0, %%edx\n\t"
                :"=A" (t)
                : "a" (ts->tv_sec), "d" (NSECS_PER_SEC), "b" (ts->tv_nsec)
                );
        return t;
}

extern inline struct timespec timespec_from_ns (hrtime_t t)
{
        struct timespec ts;
        __asm__("idivl %%ecx\n\t"
                :"=a" (ts.tv_sec), "=d" (ts.tv_nsec)
                : "A" (t), "c" (NSECS_PER_SEC)
                );
        if (ts.tv_nsec < 0) {
                ts.tv_nsec += NSECS_PER_SEC;
                ts.tv_sec --;
        }
        return ts;
}

#endif
