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
#include <time.h>   // timespec, nanosleep
#include <fcntl.h>
#include <unistd.h>
#include <drivers/pcm3718.h>

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

static float sample2volt(uint16_t the_sample, float range_p, int bipolar)
{
        float range_n = 0.0;
        if(bipolar){
                range_n = range_p;
        }
        return (range_p-(-range_n))*(float)the_sample/4096.0-range_n;
}

int main()
{
        int ret, i;
        oscilloscope_t osc;
        int value;
        struct timespec next_activation, my_period;
        int fd;
        ai_ioctl_cmd ai_command;
        ai_ioctl_arg ai_arg;
        analog_data_type analog_data;
        float voltage;

        fd = open ("/dev/daq", O_RDWR);
        assert(fd != -1);

        ai_command         = SET_RANGE_OF_CHANNEL;
        ai_arg.start_ch    = 0;
        ai_arg.input_range = UNIPOLAR_5;

        ret = ioctl(fd, ai_command, &ai_arg);
        assert(ret != -1);

        ai_command      = SET_PARAMETERS;
        ai_arg.start_ch = 0;
        ai_arg.stop_ch  = 0;
        ai_arg.trigger  = SOFTWARE;
        ai_arg.count    = 1;

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

        vga_text("-10%", conv_to_point(480, 410), BLACK, GRAY);
        vga_text("+10%", conv_to_point(590, 410), BLACK, GRAY);
        vga_text("Tdist", conv_to_point(530, 408), BLACK, GRAY);
        vga_line(conv_to_point(550, 418), conv_to_point(550, 420), BLACK);
        vga_rectangle(conv_to_point(500, 420), conv_to_point(600, 430), BLACK);
        vga_rectangle_fill(conv_to_point(501, 421), conv_to_point(599, 429), WHITE);

        // give values to the oscilloscope instance
        osc.position   = conv_to_point(19,130);
        osc.height     = 250;
        osc.width      = 600;
        osc.min_value  = 0;
        osc.max_value  = 500;
        osc.resolution = 30.0;
        osc.color_back    = BLACK;
        osc.color_wave    = YELLOW;
        osc.color_border  = WHITE;
        osc.interpolation = NONE; // NONE; CONSTANT; LINEAR;
        strncpy(osc.label_x, "Time", sizeof(osc.label_x));
        strncpy(osc.label_y, "Values", sizeof(osc.label_y));
        strncpy(osc.label_title, "Test_Oscilloscope", sizeof(osc.label_title));

        ret = oscilloscope_init(&osc);
        assert (ret == 0);

        my_period.tv_sec = 0;
        my_period.tv_nsec = 400000000;

        if (clock_gettime(CLOCK_MONOTONIC, &next_activation))
                printf ("Error in clock_realtime\n");

        for(i=0; 1; i=i+1) {
                incr_timespec (&next_activation, &my_period);

                ret = ioctl(fd,ai_command,&ai_arg);
                assert(ret != -1);

                ret = read(fd, (void *)&analog_data, sizeof(analog_data_type));
                assert(ret != -1);

                voltage = sample2volt(analog_data.the_sample, 5.0, 0);

                value = (int)rintf(voltage*100.0);

                ret = oscilloscope_put(&osc, value);
                assert (ret == 0);

                if (clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &next_activation, NULL))
                        printf("Error in clock_nanosleep");

                if (i == 30) my_period.tv_nsec = 80000000;
        }

        ret = oscilloscope_destroy(&osc);
        assert (ret == 0);

        return 0;
}
