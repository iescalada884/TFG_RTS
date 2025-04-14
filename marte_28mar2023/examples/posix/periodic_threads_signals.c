/*
 * Periodic threads using periodic timers.
 *
 * This is not the common way of doing periodic threads in POSIX. It
 * is simpler and more efficient to use 'clock_nanosleep()' for this
 * purpose. See 'periodic_threads_clock_nanosleep.c'.
 *
 */
#include <stdio.h>
#include <signal.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#include <misc/error_checks.h>

struct periodic_data {
  struct timespec per;
  int sig_num;
};

/* Periodic thread that creates a periodic timer */
void * periodic (void *arg)
{
        struct periodic_data my_data;
        siginfo_t received_sig;
        struct itimerspec timerdata;
        timer_t timer_id;
        struct sigevent event;
        sigset_t set;
        int activation_count = 0;


        my_data = * (struct periodic_data*)arg;

        /* Create timer */
        event.sigev_notify = SIGEV_SIGNAL;
        event.sigev_signo = my_data.sig_num;
        CHKE( timer_create (CLOCK_REALTIME, &event, &timer_id) );

        /* Arm periodic timer */
        timerdata.it_interval = my_data.per;
        timerdata.it_value = my_data.per;
        CHKE( timer_settime (timer_id, 0, &timerdata, NULL) );

        sigemptyset (&set);
        sigaddset (&set, my_data.sig_num);
        /* The signal mask shall have been set by the parent */


        /* Wait timer and do "useful" work */
        while (1) {
                CHKE( sigwaitinfo (&set, &received_sig) );

                printf ("Thread with period %ds%dns activation %d. Received sig:%d\n",
                        my_data.per.tv_sec, my_data.per.tv_nsec, ++activation_count,
                        received_sig.si_signo);
        }
}

/* Main program, that creates two periodic threads */
int main ()
{
        pthread_t t1,t2;
        pthread_attr_t attr;
        sigset_t set;
        struct periodic_data per_params1, per_params2;

        // Set signal mask for main thread and to be inherited by its children
        sigemptyset (&set);
        sigaddset (&set, SIGRTMIN);
        sigaddset (&set, SIGRTMIN+1);
        CHK( pthread_sigmask (SIG_BLOCK, &set, NULL) );

        CHK( pthread_attr_init (&attr) );

        /* Create first thread */
        per_params1.per.tv_sec = 0; per_params1.per.tv_nsec = 500000000;
        per_params1.sig_num = SIGRTMIN;
        CHK( pthread_create (&t1, &attr, periodic, &per_params1) );

        /* Create second thread */
        per_params2.per.tv_sec = 1; per_params2.per.tv_nsec = 500000000;
        per_params2.sig_num = SIGRTMIN+1;
        CHK( pthread_create (&t2, &attr, periodic, &per_params2) );

        /* Allows threads to execute for a while */
        sleep (20);
        exit (0);
}
