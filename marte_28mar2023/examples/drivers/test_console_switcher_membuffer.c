/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                      test_console_switcher_membuffer
 *
 *                                    c
 *
 * File 'test_console_switcher_membuffer.c'                   By Sangorrin
 *
 *
 * Simple test tests the redirection of the printf messages to the membuffer
 * driver. This is useful when you have a remote target (i.e. without a screen)
 * and you want to store debug messages in the membuffer and then send them
 * back to the host with the a logger (i.e through ethernet)
 *
 *---------------------------------------------------------------------------*/
#include <assert.h>
#include <drivers/console_switcher.h>
#include <misc/logger.h>

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define LOG_DEVICE LOG_ETHERNET

int main()
{
        int err, i;

        DEBUG("Initialization of the logger..\n");

        err = logger_init(LOG_DEVICE);
        assert(err == 0);

        DEBUG("Changing to membuffer console\n");

        MEMBUFFER_CONSOLE_INIT();

        for (i=0; i<10; i++) DEBUG("message for membuffer %d\n", i);

        while (logger_manual_call() > 0);

        STANDARD_CONSOLE_INIT();

        for (i=10; i<20; i++) DEBUG("message for standard console %d\n", i);

        MEMBUFFER_CONSOLE_INIT();

        for (i=20; i<30; i++) DEBUG("message for membuffer %d\n", i);

        while (logger_manual_call() > 0);

        return 0;
}
