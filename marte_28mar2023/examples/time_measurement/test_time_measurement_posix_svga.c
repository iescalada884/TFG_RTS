/*!
 * @file test_time_measurement_posix_svga.c
 *
 * @brief simple test to measure the svga driver performance
 *
 * This file contains a basic test for the svga module
 *
 * @version 0.01
 *
 * @date 28-Oct-2008
 *
 * @author Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 */

#include <time.h>   // timespec, nanosleep
#include <assert.h> // assert
#include "vga.h"
#include <misc/time_measurement_posix.h>
#include <misc/load.h>
#include <misc/logger.h>

#define PCI_DEVICE_ID_S3_TRIO64V2 35073
#define BLACK         0
#define LIGHT_GRAY    7

#define ENABLE_MEASURES

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

int main()
{
        int ret, i;
        struct timespec period = {0, 30000000};
#ifdef ENABLE_MEASURES
        time_measure_id_t measure_id;

        DEBUG ("init the logger\n");
        ret = logger_init(LOG_ETHERNET);
        assert(ret == 0);

        DEBUG ("init the measures\n");
        ret = time_measure_posix_create("measure",
                                        CLOCK_MONOTONIC,
                                        &measure_id);
        assert(ret == 0);
#endif

        DEBUG ("init the svga driver and the background\n");
        ret = init_vga(G640x480x16, VGA, PCI_DEVICE_ID_S3_TRIO64V2);
        assert (ret == 0);

        vga_rectangle_fill(conv_to_point(0,0),
                           conv_to_point(639,479), LIGHT_GRAY);

        DEBUG ("starting animation\n");

        for (i=0; 1; i = (i + 1) % 600) {
#ifdef ENABLE_MEASURES
                ret = time_measure_posix_begin(measure_id);
                assert(ret == 0);
#endif
                if (i == 0) {
                        vga_rectangle_fill(conv_to_point(599,200),
                                           conv_to_point(639, 250),
                                           LIGHT_GRAY);
                } else {
                        vga_rectangle_fill(conv_to_point(i-1,200),
                                           conv_to_point(i-1+40, 250),
                                           LIGHT_GRAY);
                }

                vga_rectangle_fill(conv_to_point(i,200),
                                   conv_to_point(i+40, 250), BLACK);
#ifdef ENABLE_MEASURES
                ret = time_measure_posix_end(measure_id, NULL);
                assert(ret == 0);
                while(logger_manual_call() > 0);
#endif
                nanosleep(&period, NULL);
        }

        return 0;
}
