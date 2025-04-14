//  Test for all architectures
/*
 * test_timers_sharing_a_signal.c
 *
 * by Miguel marciano
 *
 * The main thread programs 4 timers to expire in a very close time interval
 * trigerring the same signal with different signalinfo value.
 *
 * A waiting thread (with higher priority receives all signals)
 *
 * We verify that all signals are all received and they keep the same
 * order with which they were sent.
 */

#include <unistd.h>
#include <stdio.h>
#include <time.h>

#include <assert.h>
#include <stdlib.h> // for exit in assert
#include <string.h> // for memset
#include <pthread.h>
#include <signal.h>
#include <errno.h>

#include <misc/error_checks.h>
#include <misc/timespec_operations.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif


/*****************************/
/*   D E F I N I T I O N S   */
/*****************************/

#define SIGNAL_TO_SEND (SIGRTMIN + 2)
#define BASE_VALUE_TO_SEND  42
#define INTER_EXPIRATION_INTERVAL_USECS 1

#define GRACE_INTERVAL_USECS 50000
#define GRACE_INTERVAL_SECS 3

/* #define NUMBER_OF_EXPIRATIONS 4 */
#define NUMBER_OF_EXPIRATIONS 4


static struct timespec zero_time = {0, 0};

typedef struct _siginfo_recvd_t
{
    int siginfo_value;
    struct timespec signal_processing_time;
} siginfo_recvd_t;


/***************************/
/*   P R O T O T Y P E S   */
/***************************/
static void * waiter_thread_body(void *thread_arg);


/*************************************/
/*  S T A T I C   V A R I A B L E S  */
/*************************************/

int main ()
{
    pthread_attr_t waiter_thread_attr;
    pthread_t waiter_tid;
    struct sched_param schedparam;
    timer_t timers[NUMBER_OF_EXPIRATIONS];
    static siginfo_recvd_t siginfos_recvd[NUMBER_OF_EXPIRATIONS];

    struct timespec initial_time;
    struct timespec programmed_time;
    struct timespec grace_interval = {GRACE_INTERVAL_SECS, GRACE_INTERVAL_USECS*1000};
    struct timespec interexpiration_interval = {0, INTER_EXPIRATION_INTERVAL_USECS*1000};
    int i;


    sigset_t signal_set; // To set the process SIGMASK

    struct sigevent timer_sigevent;


#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif



    /* Set the signal mask */
    /***********************/
    CHK(  sigemptyset(&signal_set)  );
    CHK(  sigaddset(&signal_set, SIGNAL_TO_SEND)  );
    CHK(  pthread_sigmask(SIG_BLOCK, &signal_set, NULL)  );


    /* Initialise the results to zero */
    /**********************************/
    for (i = 0 ; i < NUMBER_OF_EXPIRATIONS ; i++)
    {
        siginfos_recvd[i].siginfo_value = 0;
        siginfos_recvd[i].signal_processing_time.tv_sec = 0;
        siginfos_recvd[i].signal_processing_time.tv_nsec = 0;
    }
        

    /* Create the thread attributes and define its priority */
    /* Assign a priority lower to the main thread.          */
    /********************************************************/
    CHK(  pthread_attr_init(&waiter_thread_attr)  );
    schedparam.sched_priority = sched_get_priority_min(SCHED_FIFO) + 5;
    CHK(  pthread_attr_setschedparam(&waiter_thread_attr, &schedparam)  );


    memset(&schedparam, 0xFF, sizeof(schedparam) );
    schedparam.sched_priority = sched_get_priority_min(SCHED_FIFO) + 3;
    CHK(  pthread_setschedparam(pthread_self(), SCHED_FIFO, &schedparam)  );


    /* Define the timers */
    /*********************/
    timer_sigevent.sigev_notify = SIGEV_SIGNAL;
    timer_sigevent.sigev_signo = SIGNAL_TO_SEND;

    for (i = 0 ; i < NUMBER_OF_EXPIRATIONS ; i++)
    {
        timer_sigevent.sigev_value.sival_int = BASE_VALUE_TO_SEND + i;
        CHK(  timer_create(CLOCK_REALTIME, &timer_sigevent, &timers[i]) );
    }

    /* Arm the timers in the future */
    /********************************/
    CHK( clock_gettime(CLOCK_REALTIME, &initial_time) );
    printf("Initial_time: %d sec %d nsec\n", initial_time.tv_sec, initial_time.tv_nsec);

    add_timespec(&programmed_time, &initial_time, &grace_interval);

    for (i = 0 ; i < NUMBER_OF_EXPIRATIONS ; i++)
    {
        struct itimerspec arm_time;
        
        arm_time.it_value = programmed_time;
        arm_time.it_interval = zero_time;

        CHK(  timer_settime(timers[i], TIMER_ABSTIME, &arm_time, NULL) );
        printf("Arming timer %d for %d sec %d nsec\n", i, programmed_time.tv_sec, programmed_time.tv_nsec);
        
        add_timespec(&programmed_time, &programmed_time, &interexpiration_interval);
    } 

    /* create the waiter thread and wait for its end */
    /*************************************************/
    CHK(  pthread_create(&waiter_tid, &waiter_thread_attr, waiter_thread_body, siginfos_recvd)  );
    CHK(  pthread_join(waiter_tid, NULL) );


    for (i = 0 ; i < NUMBER_OF_EXPIRATIONS ; i++)
    {
        assert(siginfos_recvd[i].siginfo_value = BASE_VALUE_TO_SEND + i);
        printf("Test # %d received at %d sec %d nsec\n", 
               i, 
               siginfos_recvd[i].signal_processing_time.tv_sec,
               siginfos_recvd[i].signal_processing_time.tv_nsec);
    }
               
    printf("Test OK\n");

    return 0;
}


// ----------------------------------------------------------------

static void * waiter_thread_body(void *thread_arg)
{
    int i;
    int err;

    sigset_t signal_set; // To do the sigwaitinfo
    siginfo_recvd_t *siginfos_recvd;

    siginfos_recvd = (siginfo_recvd_t *) thread_arg;


    CHK(  sigemptyset(&signal_set)  );
    CHK(  sigaddset(&signal_set, SIGNAL_TO_SEND)  );

    for (i = 0 ; i < NUMBER_OF_EXPIRATIONS ; i++)
    {
        struct timespec current_time;
        siginfo_t siginfo_received;

        printf("Waiter expecting expiration # %d\n", i);

        err = sigwaitinfo(&signal_set, &siginfo_received);
        
        if (err == -1)
        {
            err = errno;
            printf("Error in sigwaitinfo %d\n", err);
            exit(1);
        }

        clock_gettime(CLOCK_REALTIME, &current_time);
        printf("WAITER:  EXPIRATION # %d, received siginfo %d at %d sec %d nsec\n", 
               i, siginfo_received.si_value.sival_int,
               current_time.tv_sec, current_time.tv_nsec);

        siginfos_recvd[i].siginfo_value = siginfo_received.si_value.sival_int;
        siginfos_recvd[i].signal_processing_time = current_time;
    }

    return NULL;
}


