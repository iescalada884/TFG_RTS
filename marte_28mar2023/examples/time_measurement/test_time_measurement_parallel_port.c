/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                      test_time_measurement_parallel_port
 *
 *                                    c
 *
 * File 'test_time_measurement_parallel_port.c'                 By Sangorrin
 *
 *
 * Test for the use of the Parallel to take time measurements. You need to
 * connect an oscilloscope or logic analyzer to the one of the data lines
 * of the parallel port and measure the corresponding pulse's widths.
 *
 *---------------------------------------------------------------------------*/
#include <drivers/printer_port.h>
#include <sys/pio.h>
#include <time.h>
#include <misc/timespec_operations.h>

// Printer Port Registers
#define PP_BASE_REG    0x378
#define PP_DATA_REG    0     // Data port offset

int main()
{
    struct timespec my_period, next_activation;

    my_period.tv_sec  = 0;
    my_period.tv_nsec = 10000;

    clock_gettime(CLOCK_MONOTONIC, &next_activation);

    while(1) {
        outb_p (PP_BASE_REG + PP_DATA_REG, 0xFF);
        incr_timespec (&next_activation, &my_period);
        clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &next_activation, NULL);
        outb_p (PP_BASE_REG + PP_DATA_REG, 0);
        incr_timespec (&next_activation, &my_period);
        clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &next_activation, NULL);
    }

    return 0;
}
