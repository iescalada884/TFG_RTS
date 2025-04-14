/*!
 * @file test_oscilloscope.c
 *
 * @brief simple test for the oscilloscope
 *
 * This file contains a basic test for the oscilloscope module
 *
 * NOTE: the svga driver must have been already initialized before with
 *
 *     #define PCI_DEVICE_ID_S3_TRIO64V2 35073
 *     error = init_vga(G640x480x16, VGA, PCI_DEVICE_ID_S3_TRIO64V2);
 *     assert (error == 0);
 *
 * @version 0.01
 *
 * @date 10-Jun-2008
 *
 * @author Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 */

#include <string.h> // for strncpy
#include <time.h>   // timespec, nanosleep
#include <assert.h> // assert
#include <math.h>   // sin

#include <misc/timespec_operations.h>
#include "oscilloscope.h"
#include "vga.h"

#define PCI_DEVICE_ID_S3_TRIO64V2 35073

#define   BLACK         0
#define   BLUE          1
#define   GREEN         2
#define   CYAN          3
#define   RED           4
#define   MAGENTA       5
#define   BROWN         6
#define   LIGHT_GRAY    7
#define   GRAY          8
#define   ICE_BLUE      9
#define   LIGHT_GREEN   10
#define   LIGHT_BLUE    11
#define   LIGHT_RED     12
#define   PINK          13
#define   YELLOW        14
#define   WHITE         15

int main()
{
        int ret, i;
        oscilloscope_t osc;
        int value;
        double wave_period        = 5.0;
        double samples_per_period = 40.0;
        double sampling_period    = wave_period / samples_per_period;
        double number_of_periods  = 2.0;
        struct timespec sampling_period_ts;

        double_to_timespec(sampling_period, &sampling_period_ts);

        // init the vga driver
        ret = init_vga(G640x480x16, VGA, PCI_DEVICE_ID_S3_TRIO64V2);
        assert (ret == 0);

        // set scenary
        vga_rectangle_fill(conv_to_point(0,0), conv_to_point(639,79), LIGHT_GRAY);
        vga_text("OSCILLOSCOPE",
                 conv_to_point(319 - 8*strlen("OSCILLOSCOPE")/2,40),
                 BLACK,
                 LIGHT_GRAY);
        vga_rectangle_fill(conv_to_point(0,80), conv_to_point(639,479), GRAY);

        // give values to the oscilloscope instance
        osc.position   = conv_to_point(19,130);
        osc.height     = 250;
        osc.width      = 600;
        osc.min_value  = 0;
        osc.max_value  = 100;
        osc.resolution = number_of_periods * wave_period;
        osc.color_back    = BLACK;
        osc.color_wave    = YELLOW;
        osc.color_border  = WHITE;
        osc.interpolation = NONE; // NONE; CONSTANT; LINEAR;
        strncpy(osc.label_x, "Time", sizeof(osc.label_x));
        strncpy(osc.label_y, "Values", sizeof(osc.label_y));
        strncpy(osc.label_title, "Test_Oscilloscope", sizeof(osc.label_title));

        ret = oscilloscope_init(&osc);
        assert (ret == 0);

        for(i=0; 1; i=i+1) {
                // value = ((i % 50) <  25) ? 2 : 8;
                value = 50 + (int)(40.0 * sin(2*M_PI * (float)i/samples_per_period));

                ret = oscilloscope_put(&osc, value);
                assert (ret == 0);

                nanosleep(&sampling_period_ts, NULL);
        }

        ret = oscilloscope_destroy(&osc);
        assert (ret == 0);

        return 0;
}
