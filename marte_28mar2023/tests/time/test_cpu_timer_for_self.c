//  Test for all architectures
/*
 * test_cpu_timer_for_self.c
 *
 * by Miguel marciano
 *
 * A thread program a CPU-timer for itself. The signal is accepted by the
 * main thread.
 *
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
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif


/*****************************/
/*   D E F I N I T I O N S   */
/*****************************/

#define SIGNAL_TO_SEND (SIGRTMIN + 2)
#define VALUE_TO_SEND  42
static const struct timespec TIMER_INTERVAL= {0,200000000};
static const struct timespec TIMER_INTERVAL_PLUS_EPSILON=  {0,210000000};
static const struct timespec TIMER_INTERVAL_MINUS_EPSILON= {0,190000000};

#define decr_timespec(t1, t2) \
do { \
  if ((t1)->tv_nsec < (t2)->tv_nsec) { \
    (t1)->tv_sec -= (t2)->tv_sec + 1; \
    (t1)->tv_nsec = (t1)->tv_nsec + 1000000000 - (t2)->tv_nsec; \
  } else { \
    (t1)->tv_sec -= (t2)->tv_sec; \
    (t1)->tv_nsec -= (t2)->tv_nsec; \
  } \
} while (0)


#define  add_timespec( sum , t1 , t2 ) \
do { \
  (sum)->tv_sec  = (t1)->tv_sec  + (t2)->tv_sec; \
  (sum)->tv_nsec = (t1)->tv_nsec + (t2)->tv_nsec; \
  if ((sum)->tv_nsec >= 1000000000) { \
    (sum)->tv_sec ++; \
    (sum)->tv_nsec -= 1000000000; \
  } \
} while (0)


#define smaller_timespec(t1, t2) \
 ( \
  (t1)->tv_sec < (t2)->tv_sec || ((t1)->tv_sec == (t2)->tv_sec &&   \
  (t1)->tv_nsec < (t2)->tv_nsec) \
 )


static struct timespec zero_time = {0, 0};


/***************************/
/*   P R O T O T Y P E S   */
/***************************/
static void * thread_body(void *thread_arg);
static void frsh_eat(const struct timespec *cpu_time);


/*************************************/
/*  S T A T I C   V A R I A B L E S  */
/*************************************/
static struct timespec start_execution;
static struct timespec signal_reception;

int main ()
{
    int err = -1;

    pthread_attr_t thread_attr;
    pthread_t tid;
    struct sched_param schedparam;

    int signal_received = 0;
    sigset_t signal_set;
    siginfo_t siginfo_received;
    struct timespec work_interval = {0, 100000000}; // 0.1 seconds

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

    memset(&thread_attr, 0, sizeof(thread_attr) );
    memset(&tid, 0, sizeof(tid) );
    memset(&schedparam, 0xFF, sizeof(schedparam) );

    memset(&siginfo_received, 0, sizeof(siginfo_received) );
    memset(&signal_set, 0xFF, sizeof(signal_set) );
    memset(&start_execution, 0xFF, sizeof(start_execution) );
    memset(&signal_received, 0xFF, sizeof(signal_received) );


    /* Set the signal mask */
    /***********************/
    CHK(  sigemptyset(&signal_set)  );
    CHK(  sigaddset(&signal_set, SIGNAL_TO_SEND)  );
    CHK(  pthread_sigmask(SIG_BLOCK, &signal_set, NULL)  );


    /* Create the thread attributes and define its priority */
    /* Assign a priority higher to the main thread.         */
    /********************************************************/
    CHK(  pthread_attr_init(&thread_attr)  );
    schedparam.sched_priority = sched_get_priority_min(SCHED_FIFO) + 3;
    CHK(  pthread_attr_setschedparam(&thread_attr, &schedparam)  );


    memset(&schedparam, 0xFF, sizeof(schedparam) );
    schedparam.sched_priority = sched_get_priority_min(SCHED_FIFO) + 5;
    CHK(  pthread_setschedparam(pthread_self(), SCHED_FIFO, &schedparam)  );


    /* create the periodic thread.  It won't execute yet */
    /* because it has lower priority than main().        */
    /*****************************************************/
    CHK(  pthread_create(&tid, &thread_attr, thread_body, NULL)  );


    /* We execute a little bit to ensure that execution time */
    /* and real time differ                                  */
    /*********************************************************/
    printf("Main works for some time...\n");
    frsh_eat(&work_interval);

    /* Now we do the wait in order to allow the thread to run */
    /**********************************************************/
    printf("About to do the wait\n");
    err = sigwaitinfo(&signal_set, &siginfo_received);
    if (err == -1)
    {
        err = errno;
    }
    else
    {
        err = 0;
    }

    /* We measure the time of signal reception */
    /*******************************************/
    clock_gettime(CLOCK_REALTIME, &signal_reception);

    decr_timespec(&signal_reception, &start_execution);

    printf("signal received=%d value=%d (%d?), err=%d\n",
           siginfo_received.si_signo, VALUE_TO_SEND,
           siginfo_received.si_value.sival_int,
           err);
    assert(siginfo_received.si_signo==SIGNAL_TO_SEND);
    assert(siginfo_received.si_value.sival_int==VALUE_TO_SEND);

    printf("Elapsed time between sigwait and timer expiration: %d %d\n",
           signal_reception.tv_sec, signal_reception.tv_nsec);
    assert(smaller_timespec(&TIMER_INTERVAL_MINUS_EPSILON, &signal_reception));
    assert(smaller_timespec(&signal_reception, &TIMER_INTERVAL_PLUS_EPSILON));

    printf("Test OK\n");
    return 0;
}


// ----------------------------------------------------------------

static void * thread_body(void *thread_arg)
{
    struct timespec before_work_time = {-1, -1};
    struct timespec after_work_time = {-1, -1};

    clockid_t clockid;

    struct sigevent timer_sigevent;
    timer_t timerid;
    struct itimerspec arm_time = { { -1, -1}, {-1, -1} };
    struct timespec work_interval = {0, 100000000}; // 0.1 seconds


    memset(&clockid, 0, sizeof(clockid) );
    memset(&timer_sigevent, 0, sizeof(timer_sigevent) );
    memset(&timerid, 0, sizeof(timerid) );


    /* Get the thread's cputime clock */
    /**********************************/
    CHK(  pthread_getcpuclockid(pthread_self(), &clockid) );


    /* Create a timer and arm it with a given budget */
    /*************************************************/
    timer_sigevent.sigev_notify = SIGEV_SIGNAL;
    timer_sigevent.sigev_signo = SIGNAL_TO_SEND;
    timer_sigevent.sigev_value.sival_int = VALUE_TO_SEND;
    CHK(  timer_create(clockid, &timer_sigevent, &timerid)  );


    arm_time.it_value = TIMER_INTERVAL;
    arm_time.it_interval = zero_time;
    CHK(  timer_settime(timerid, 0, &arm_time, NULL) );

    /* We measure the start execution timer */
    /* and enter in the periodic cycle.     */
    /****************************************/
    clock_gettime(CLOCK_REALTIME, &start_execution);

    while(1)
    {
        clock_gettime(CLOCK_REALTIME, &before_work_time);

        printf("Start periodic work  at %d, %d\n",
               before_work_time.tv_sec, before_work_time.tv_nsec);

        frsh_eat(&work_interval);

        clock_gettime(CLOCK_REALTIME, &after_work_time);

        printf("End periodic work  at %d, %d\n",
               after_work_time.tv_sec, after_work_time.tv_nsec);

        decr_timespec(&after_work_time, &before_work_time);
        printf("Elapsed time:  %d %d\n", after_work_time.tv_sec,
               after_work_time.tv_nsec);
    } // while

    return NULL;
}

// ---------------------------------------------------------

static void frsh_eat(const struct timespec *cpu_time)
{
    clockid_t clock_id;
    struct timespec current_time, time_to_go;

    // NOTE: there should be a constant for the cpu_clock_id of the caller
    // to avoid calling 'fosa_thread_get_cputime_clock'
    (void)pthread_getcpuclockid(pthread_self(), &clock_id);

    (void)clock_gettime(clock_id, &current_time);
    add_timespec(&time_to_go, &current_time, cpu_time);

    while (smaller_timespec(&current_time, &time_to_go)) {
        (void)clock_gettime(clock_id, &current_time);
    }
}







