/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                      test_time_measurement_posix_usleep
 *
 *                                    c
 *
 * File 'test_time_measurement_posix_usleep.c'                  By Sangorrin
 *
 *
 * Test for the time_measurement_posix module with usleep
 *
 *---------------------------------------------------------------------------*/
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <misc/time_measurement_posix.h>
#include <misc/load.h>
#include <misc/logger.h>

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define MX_MEASURES 5

int main()
{
        int err, i;
        time_measure_id_t measure_id;

        adjust ();

        err = logger_init(LOG_ETHERNET);
        assert(err == 0);

        err = time_measure_posix_create("measure",
                                        CLOCK_MONOTONIC,
                                        &measure_id);
        assert(err == 0);

        for (i=0; i<MX_MEASURES; i++) {
                err = time_measure_posix_begin(measure_id);
                assert(err == 0);
                usleep(1437);
                err = time_measure_posix_end(measure_id, NULL);
                assert(err == 0);
        }

        while(logger_manual_call() > 0);

        return 0;
}
