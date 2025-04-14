/*!
 * @file oscilloscope.h
 *
 * @brief simple oscilloscope
 *
 * This file contains a very basic oscilloscope used to draw a stream of data
 * values on the screen using the MaRTE OS svga driver. The lapse of time is
 * calculated internally by the oscilloscope so the user just have to puts
 * the data with the same pace as it should be displayed.
 *
 * NOTE: the svga driver must have been already initialized before with
 *
 *     #define PCI_DEVICE_ID_S3_TRIO64V2 35073
 *     error = init_vga(G640x480x16, VGA, PCI_DEVICE_ID_S3_TRIO64V2);
 *     assert (error == 0);
 *
 * @version 0.02
 *
 * @date 17-Jan-2009
 *
 * @author Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 */

#ifndef _OSCILLOSCOPE_
#define _OSCILLOSCOPE_

#include "vga.h"  // for point_t

/**
 * interpolation_t - type of interpolation between the points in the wave
 *
 * @NONE: do not interpolate, only draw the points
 * @CONSTANT: use constant interpolation
 * @LINEAR: use linear interpolation
 * @POLYNOMIAL: use polynomial interpolation (not implemented)
 */

typedef enum {
        NONE,
        CONSTANT,
        LINEAR,
        POLYNOMIAL
} interpolation_t;

/**
 * oscilloscope_t - oscilloscope data
 *
 * @position: position of the upper left corner of the oscilloscope (in pixels)
 * @height: height of the oscilloscope (in pixels)
 * @width: width of the oscilloscope (in pixels)
 * @min_value: minimum value that can be drawed on the y axis (any integer)
 * @max_value: maximum value that can be drawed on the y axis (any integer)
 * @resolution: the amount of time represented on the x axis by width
 * @color_back: color of the background of the oscilloscope
 * @color_wave: color of the wave
 * @color_border: color of the border and labels
 * @label_x: label for the x axis
 * @label_y: label for the y axis
 * @label_title: label for the title
 * @interpolation: the type of interpolation to use
 * @private_data: private data managed by the oscilloscope implementation
 */

typedef struct {
        point_t position;
        int height;
        int width;
        int min_value;
        int max_value;
        double resolution;
        int color_back;
        int color_wave;
        int color_border;
        char label_x[20];
        char label_y[20];
        char label_title[20];
        interpolation_t interpolation;
        void *private_data;
} oscilloscope_t;

/**
 * oscilloscope_init()
 *
 * Creates and draws an oscilloscope instance. Returns 0 on success and
 * another value in case of error.
 *
 */

int oscilloscope_init(oscilloscope_t *osc);

/**
 * oscilloscope_put()
 *
 * Puts a new value, in the range (osc->min_value .. osc->max_value) on the
 * oscilloscope. Returns 0 on success. and another value in case of error.
 *
 */

int oscilloscope_put(oscilloscope_t *osc, int value);

/**
 * oscilloscope_destroy()
 *
 * Destructor for the oscilloscope. Internally it release any acquired
 * resources.
 *
 */

int oscilloscope_destroy(oscilloscope_t *osc);

#endif // _OSCILLOSCOPE_
