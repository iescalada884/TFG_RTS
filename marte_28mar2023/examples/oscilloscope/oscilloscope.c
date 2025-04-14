/*!
 * @file oscilloscope.c
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

#include <string.h>  // for strlen
#include <stdlib.h>  // for malloc
#include <math.h>    // for ceil
#include <time.h>    // for clock_gettime
#include <assert.h>
#include <misc/timespec_operations.h>
#include "oscilloscope.h"  // for point_t

#define LABEL_WIDTH      30 // pixels
#define CLEAN_BAND_WIDTH 20 // pixels

/**
 * oscilloscope_private_data_t - oscilloscope private data
 *
 * @canvas_position: upper left corner of the canvas (the drawing rectangle)
 * @canvas_width: canvas width (in pixels)
 * @canvas_height: canvas height (in pixels)
 * @canvas_pixels: array to remember which pixel have been drawn in each column
 * @last_point: the last point of the wave drawed
 * @last_point_time: the absolute time when the point was drawed
 * @double time_per_pixel: time represented by each pixel in the x axis
 *
 */

typedef signed long long s64;

typedef struct {
        point_t canvas_position;
        int     canvas_width;
        int     canvas_height;
        int     *canvas_pixels;
        point_t last_point;
        s64 start_time_ns;
        s64 time_per_pixel_ns;
} oscilloscope_private_data_t;

#define NSEC_PER_SEC 1000000000L

static inline s64 timespec_to_ns(const struct timespec *ts)
{
        return ((s64) ts->tv_sec * NSEC_PER_SEC) + ts->tv_nsec;
}

/**
 * oscilloscope_init()
 */

int oscilloscope_init(oscilloscope_t *osc)
{
        int i, ret;
        char tmp[2];
        struct timespec now;
        oscilloscope_private_data_t *priv;

        // allocate and fill private data
        osc->private_data = malloc(sizeof(oscilloscope_private_data_t));
        assert(osc->private_data != NULL);
        priv = (oscilloscope_private_data_t *)osc->private_data;

        priv->canvas_width    = osc->width  - 2*LABEL_WIDTH;
        priv->canvas_height   = osc->height - 2*LABEL_WIDTH;
        priv->canvas_position = conv_to_point(osc->position.x + LABEL_WIDTH,
                                              osc->position.y + LABEL_WIDTH);
        priv->canvas_pixels = (int *)malloc(sizeof(int) * priv->canvas_width);
        assert(priv->canvas_pixels != NULL);

        for (i=0; i<priv->canvas_width; i++) {
            priv->canvas_pixels[i] = -1; // '-1' means no pixel drawn
        }

        priv->last_point = conv_to_point(0, priv->canvas_height/2);

        ret = clock_gettime(CLOCK_MONOTONIC, &now);
        assert(ret == 0);

        priv->start_time_ns = timespec_to_ns(&now);
        priv->time_per_pixel_ns  = 50000000;

        // fill background
        vga_rectangle_fill(osc->position,
                           conv_to_point(osc->position.x + osc->width,
                                         osc->position.y + osc->height),
                           osc->color_back);

        // draw border of the canvas
        vga_rectangle(conv_to_point(priv->canvas_position.x - 1,
                                    priv->canvas_position.y - 1),
                      conv_to_point(priv->canvas_position.x +
                                    priv->canvas_width + 1,
                                    priv->canvas_position.y +
                                    priv->canvas_height + 1),
                      osc->color_border);

        // labels
        vga_text(osc->label_x,
                 conv_to_point(osc->position.x +
                               osc->width/2 - 8*strlen(osc->label_x)/2,
                               osc->position.y +
                               priv->canvas_height + 3*LABEL_WIDTH/2),
                 osc->color_border,
                 osc->color_back);

        vga_text(osc->label_title,
                 conv_to_point(osc->position.x +
                               osc->width/2 - 4*strlen(osc->label_title),
                               osc->position.y + LABEL_WIDTH/2),
                 osc->color_border,
                 osc->color_back);

        tmp[1] = '\0';
        for (i=0; i<strlen(osc->label_y); i++) {
                tmp[0] = osc->label_y[i];
                vga_text(tmp,
                         conv_to_point(osc->position.x + LABEL_WIDTH/2,
                                       osc->position.y +
                                       LABEL_WIDTH + priv->canvas_height/2 -
                                       4*strlen(osc->label_y) + i*8),
                         osc->color_border,
                         osc->color_back);
        }

        return 0;
}

/**
 * value_to_pixels()
 *
 * auxiliar function to convert a value into the corresponding pixels in
 * the y axis (the oscilloscope y offset must be added)
 *
 *               y  (pixels)
 *
 *               |
 *               +--+
 *               | /|
 *               |/ |
 *               /  |
 *              /|  |
 *             / |  |
 *            /  |  |
 *          -+---+--+--  x (values)
 *               0
 *
 *  The line is (min_value, 0) -> (max_value, height - 2*LABEL_WIDTH)
 *
 *  y = m*(x - x1) + y1 = m*(x - min_value)
 *
 *  m = (height - 2*LABEL_WIDTH) / (max_value - min_value)
 *
 */

static int value_to_pixels(oscilloscope_t *osc, int value)
{
        double m;
        oscilloscope_private_data_t *priv;

        priv = (oscilloscope_private_data_t *)osc->private_data;
        assert(priv != NULL);

        if (value > osc->max_value) {
                value = osc->max_value;
        } else if (value < osc->min_value) {
                value = osc->min_value;
        }

        m = (double)(priv->canvas_height) /
            (double)(osc->max_value - osc->min_value);

        return (int)(m*(double)(value - osc->min_value));
}

/**
 * modified_vga_drawline()
 *
 * This is a copy of vga_drawline in vgaline.c modified so when we draw a
 * new pixel we record it in canvas_pixels.
 */

#define ABS(a) (((a)<0) ? -(a) : (a))

static int modified_vga_drawline(oscilloscope_private_data_t *priv,
                                 int x1, int y1, int x2, int y2)
{
    int dx = x2 - x1;
    int dy = y2 - y1;
    int ax = ABS(dx) << 1;
    int ay = ABS(dy) << 1;
    int sx = (dx >= 0) ? 1 : -1;
    int sy = (dy >= 0) ? 1 : -1;
    int x = x1;
    int y = y1;
    int canvas_x;

    if (ax > ay) {
        int d = ay - (ax >> 1);
        while (x != x2) {
            canvas_x = x - priv->canvas_position.x;
            if (priv->canvas_pixels[canvas_x] == -1) {
                vga_drawpixel(x, y);
                priv->canvas_pixels[canvas_x] = priv->canvas_position.y +
                                                priv->canvas_height - y;
            }

            if (d > 0 || (d == 0 && sx == 1)) {
                y += sy;
                d -= ax;
            }
            x += sx;
            d += ay;
        }
    } else {
        int d = ax - (ay >> 1);
        while (y != y2) {
            canvas_x = x - priv->canvas_position.x;
            if (priv->canvas_pixels[canvas_x] == -1) {
                vga_drawpixel(x, y);
                priv->canvas_pixels[canvas_x] = priv->canvas_position.y +
                        priv->canvas_height - y;
            }

            if (d > 0 || (d == 0 && sy == 1)) {
                x += sx;
                d -= ay;
            }
            y += sy;
            d += ax;
        }
    }
    canvas_x = x - priv->canvas_position.x;
    if (priv->canvas_pixels[canvas_x] == -1) {
        vga_drawpixel(x, y);
        priv->canvas_pixels[canvas_x] = priv->canvas_position.y +
                priv->canvas_height - y;
    }

    return 0;
}


/**
 * oscilloscope_put()
 */

int oscilloscope_put(oscilloscope_t *osc, int value)
{
        int ret, i;
        int new_x;
        point_t new_point;
        struct timespec now;
        s64 now_ns;
        oscilloscope_private_data_t *priv;

        priv = (oscilloscope_private_data_t *)osc->private_data;
        assert(priv != NULL);

        // find the (x,y) of the new point
        ret = clock_gettime(CLOCK_MONOTONIC, &now);
        assert(ret == 0);

        now_ns = timespec_to_ns(&now);

        new_x = ((now_ns - priv->start_time_ns) / priv->time_per_pixel_ns) % priv->canvas_width;

        if (new_x == priv->last_point.x) return 0;

        new_point.x = new_x;
        new_point.y = value_to_pixels(osc, value);

        // clean the pixels in the middle columns
        vga_setcolor(osc->color_back);

        for (i = (priv->last_point.x + 1) % priv->canvas_width;
             i != ((new_point.x + CLEAN_BAND_WIDTH) % priv->canvas_width);
             i = (i + 1) % priv->canvas_width)
        {
                if (priv->canvas_pixels[i] == -1) continue; // column is clean

                vga_drawpixel(priv->canvas_position.x + i,
                              priv->canvas_position.y +
                              priv->canvas_height - priv->canvas_pixels[i]);
                priv->canvas_pixels[i] = -1;
        }

        // clean the pixels in the new point's column as well
        if (priv->canvas_pixels[new_point.x] != -1) {
                vga_drawpixel(priv->canvas_position.x + new_point.x,
                              priv->canvas_position.y + priv->canvas_height -
                              priv->canvas_pixels[new_point.x]);
                priv->canvas_pixels[new_point.x] = -1;
        }

        // draw the new pixels (interpolation + new point)
        vga_setcolor(osc->color_wave);

        switch(osc->interpolation) {
        case CONSTANT:
                for (i = (priv->last_point.x + 1) % priv->canvas_width;
                     i != (new_point.x % priv->canvas_width);
                     i = (i + 1) % priv->canvas_width)
                {
                        vga_drawpixel(priv->canvas_position.x + i,
                                      priv->canvas_position.y +
                                      priv->canvas_height -
                                      priv->last_point.y);
                        priv->canvas_pixels[i] = priv->last_point.y;
                }
        case NONE:
                 // draw the new point
                vga_drawpixel(priv->canvas_position.x + new_point.x,
                              priv->canvas_position.y +
                              priv->canvas_height - new_point.y);
                priv->canvas_pixels[new_point.x] = new_point.y;
                break; // no need to draw interpolation pixels

        case LINEAR:
                modified_vga_drawline(priv,
                                      priv->canvas_position.x +
                                      priv->last_point.x,
                                      priv->canvas_position.y +
                                      priv->canvas_height - priv->last_point.y,
                                      priv->canvas_position.x + new_point.x,
                                      priv->canvas_position.y +
                                      priv->canvas_height - new_point.y);
                break;

        default:
                return -1;
        }

        priv->last_point = new_point;

        return 0;
}

/**
 * oscilloscope_destroy()
 */

int oscilloscope_destroy(oscilloscope_t *osc)
{
        oscilloscope_private_data_t *priv;

        priv = (oscilloscope_private_data_t *)osc->private_data;

        assert(priv != NULL);
        free(priv->canvas_pixels);

        assert(osc->private_data != NULL);
        free(osc->private_data);

        return 0;
}
