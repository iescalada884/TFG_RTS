//  Test for all architectures
/*!
 * @file test_exceptions.c
 *
 * @brief Test for exceptions
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
 * Test for exceptions
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

int main () {
  //char myarray[10];

#if MARTE_ARCHITECTURE == ARCH_X86
    SERIAL_CONSOLE_INIT();
#endif

    printf ("enter loop\n");
    try {
        for (int n=0; n<=10; n++) {
            if (n>9) throw 1;
            //myarray[n]='z';
        }
        printf ("didnt catch it\n");
    } catch (int a) {
        printf ("There was an exception!!\n");
        printf("Test OK\n");
        return 0;
    }

    return -1;
}
