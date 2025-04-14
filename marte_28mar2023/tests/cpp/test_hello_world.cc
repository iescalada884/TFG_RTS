//  Test for all architectures
/*!
 * @file test_hello_world.c
 *
 * @brief Test for a hello world
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
 * Test for c++ hello world TODO: use cout
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

int main (void) {

#if MARTE_ARCHITECTURE == ARCH_X86
    SERIAL_CONSOLE_INIT();
#endif
    printf ("Hello World!!!\n");
    printf("Test OK\n");
    return 0;
}
