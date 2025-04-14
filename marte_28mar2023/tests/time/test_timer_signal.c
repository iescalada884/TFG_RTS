//  Test for all architectures
/*!
 * @file test_timer_signal.c
 *
 * @brief Program a timer to send signals every PERIOD_SECS seconds
 *
 * @version 0.01
 *
 * @date 21 - Oct -2008
 *
 * @author
 *      Daniel Sangorrin <daniel.sangorrin@unican.es>
 *
 * @comments
 *
 * Check that we give it time to complete
 *
 * @license
 *
 * See MaRTE OS License
 *
 */
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <pthread.h>
#include <signal.h>
#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define SIGNAL_TO_SEND (SIGRTMIN + 2)
#define VALUE_TO_SEND  69
#define PERIOD_NSECS 500000000

int main ()
{
        int i, err, count;
        struct sigevent timer_sigevent;
        timer_t timerid;
        static const struct timespec timer_interval = {0, PERIOD_NSECS};
        struct itimerspec arm_time;

        sigset_t signal_set;
        siginfo_t siginfo_received;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        printf("Prepare Event\n");
        timer_sigevent.sigev_notify = SIGEV_SIGNAL;
        timer_sigevent.sigev_signo = SIGNAL_TO_SEND;
        timer_sigevent.sigev_value.sival_int = VALUE_TO_SEND;

        printf ("Create timer\n");
        CHK(  timer_create(CLOCK_REALTIME, &timer_sigevent, &timerid)  );
        arm_time.it_value    = timer_interval;
        arm_time.it_interval = timer_interval;
        CHK(  timer_settime(timerid, 0, &arm_time, NULL) );

        printf ("wait for signal\n");
        CHK(  sigemptyset(&signal_set)  );
        CHK(  sigaddset(&signal_set, SIGNAL_TO_SEND)  );
        CHK(  pthread_sigmask(SIG_BLOCK, &signal_set, NULL)  );

        count = 0;
        for (i=0; i<6; i++) {
                err = sigwaitinfo(&signal_set, &siginfo_received);

                printf("signal received=%d value=%d (%d?), err=%d\n",
                       siginfo_received.si_signo, VALUE_TO_SEND,
                       siginfo_received.si_value.sival_int,
                       err);

                assert (err != -1);
                assert(siginfo_received.si_signo == SIGNAL_TO_SEND);
                assert(siginfo_received.si_value.sival_int == VALUE_TO_SEND);

                count++;
        }

        assert (count == 6);

        printf("Test OK\n");
        return 0;
}
