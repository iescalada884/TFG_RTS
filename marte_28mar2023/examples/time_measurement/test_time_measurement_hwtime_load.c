/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                      test_time_measurement_posix_load
 *
 *                                    c
 *
 * File 'test_time_measurement_posix_load.c'                    By Sangorrin
 *
 *
 * Test for the time_measurement_posix module with a specific load
 *
 *---------------------------------------------------------------------------*/
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <misc/time_measurement_hwtime.h>
#include <misc/load.h>
#include <misc/logger.h>

#if 1
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

#define MX_MEASURES 10

static const trace_point_id_t BEGIN = 0;
static const trace_point_id_t END = 1;

int main()
{
        int err, i;

        adjust ();

        err = logger_init(LOG_ETHERNET);
        assert(err == 0);

        err = time_measure_hwtime_init(BEGIN, "eat_begin");
        assert(err == 0);

        err = time_measure_hwtime_init(END, "eat_end");
        assert(err == 0);

        for (i=0; i<MX_MEASURES; i++) {
                time_measure_hwtime_set_timestamp(BEGIN);
                eat(0.003);
                time_measure_hwtime_set_timestamp(END);
        }

        err = time_measure_hwtime_write_membuffer(BEGIN);
        assert(err == 0);

        err = time_measure_hwtime_write_membuffer(END);
        assert(err == 0);

        while (logger_manual_call() > 0);

        return 0;
}
