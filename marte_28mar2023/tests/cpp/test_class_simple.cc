//  Test for all architectures
/*!
 * @file test_class_simple.c
 *
 * @brief Test for a simple c++ class
 *
 * @version 0.01
 *
 * @date 31-Oct-2008
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * Test for a simple c++ class
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <stdio.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

class CRectangle
{
    int x, y;
    public:
        void set_values (int,int);
        int area () {return (x*y);}
};

void CRectangle::set_values (int a, int b) {
    x = a;
    y = b;
}

int main () {
    CRectangle rect;

#if MARTE_ARCHITECTURE == ARCH_X86
    SERIAL_CONSOLE_INIT();
#endif

    rect.set_values (3,4);
    if (rect.area() == 12) {
        printf("Test OK\n");
        return 0;
    }

    return -1;
}
