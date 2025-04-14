/*!
 * @file test_vga_rectangle_fill.c
 *
 * @brief very simple test for the function 'vga_rectangle_fill'
 *
 * @version 0.01
 *
 * @date 16-Jun-2008
 *
 * @author Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 */

#include <assert.h> // assert
#include "vga.h"

#define PCI_DEVICE_ID_S3_TRIO64V2 35073
#define GREEN 2

int main()
{
        int ret, i;

        // init the vga driver
        ret = init_vga(G640x480x16, VGA, PCI_DEVICE_ID_S3_TRIO64V2);
        assert (ret == 0);

        // draw rectangle
        vga_rectangle_fill(conv_to_point(200,100), conv_to_point(500,300), GREEN);

        return 0;
}
