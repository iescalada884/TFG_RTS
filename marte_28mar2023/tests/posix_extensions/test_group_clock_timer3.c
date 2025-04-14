//  Test for all architectures
/*
 * test_thread_sets.c
 *
 * All 3 threads (A, B and C) must contribute to the CPU timer expiration.
 *
 * Needs load_loop.c/load_loop.h for the eat() function.
 */


#include <unistd.h>
#include <stdio.h>
#include <stdarg.h>

#include <assert.h>
#include <stdlib.h> // for exit in assert
#include <string.h> // for memset
#include <stdbool.h>

#include <pthread.h>
#include <sched.h>

#include <misc/timespec_operations.h>
#include <misc/load_loop.h>
#include <misc/error_checks.h>
#include <thread_sets.h>


#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif


/*****************************/
/*   D E F I N I T I O N S   */
/*****************************/
#define RT_ERROR_SIGWAIT -2
#define RT_ERROR_TIMER   -3

#define SIGNAL_TIMER         (SIGRTMAX - 1)

#define SIGNAL_A             (SIGRTMAX - 2)
#define SIGNAL_KILL_A        (SIGRTMAX - 3)

#define SIGNAL_B             (SIGRTMAX - 4)
#define SIGNAL_KILL_B        (SIGRTMAX - 5) 
#define SIGNAL_C             (SIGRTMAX - 6)
#define SIGNAL_KILL_C        (SIGRTMAX - 7)

#define PRIO_MAIN 4
#define PRIO_A 5
#define PRIO_B 6
#define PRIO_C 7
#define PRIO_CATCHER 8


typedef struct _my_thread_arg_t
{
    char identifier[100];
    int  signum;
    int  sigkill;
} my_thread_arg_t;

typedef void * (*thread_code_t) (void *);



/***************************/
/*   P R O T O T Y P E S   */
/***************************/
static void *controlled_thread(void *thread_arg);
static void *catcher_thread(void *arg);

static void create_thread(thread_code_t thread_code,  
                          void *arg, 
                          int priority, 
                          pthread_t *tid);

static int thread_set_prio(pthread_t tid, int prio);

static int my_timer_create(clockid_t clockid, int signal, siginfo_t info, timer_t *timer);

static int rel_timer_arm(timer_t timerid, struct timespec *value);


static void time_printf(const char *format, ...);


/***************************/
/*  S T A T I C   D A T A  */
/***************************/
static struct timespec start_time;
static bool timer_expired;


int main ()
{
#if MARTE_ARCHITECTURE == ARCH_X86
  SERIAL_CONSOLE_INIT();
#endif

    pthread_t tid_A, tid_B, tid_C, tid_catcher;
    my_thread_arg_t thread_arg_A, thread_arg_B, thread_arg_C;
    float eat_time_A, eat_time_B, eat_time_C;
    struct timespec group_time;
    siginfo_t siginfo_A, siginfo_B, siginfo_C, siginfo_timer;
    
    marte_thread_set_t thread_set;
    clockid_t group_clock_id;
    timer_t group_timer;

    /* Initialize base time and thread arguments */
    /*********************************************/
    printf("Adjusting...\n");
    adjust();
    clock_gettime(CLOCK_MONOTONIC, &start_time);

    strcpy(thread_arg_A.identifier, "THREAD A");
    thread_arg_A.signum = SIGNAL_A;
    thread_arg_A.sigkill = SIGNAL_KILL_A;
    siginfo_A.si_value.sival_ptr = &eat_time_A;

    strcpy(thread_arg_B.identifier, "THREAD B");
    thread_arg_B.signum = SIGNAL_B;
    thread_arg_B.sigkill = SIGNAL_KILL_B;
    siginfo_B.si_value.sival_ptr = &eat_time_B;

    strcpy(thread_arg_C.identifier, "THREAD C");
    thread_arg_C.signum = SIGNAL_C;
    thread_arg_C.sigkill = SIGNAL_KILL_C;
    siginfo_C.si_value.sival_ptr = &eat_time_C;

    
    /* We set our priority and create the threads.  The threads */
    /* will be blocked waiting for the signal.                  */
    /************************************************************/
    CHK( thread_set_prio(pthread_self(), sched_get_priority_min(SCHED_FIFO) + PRIO_MAIN)  );

    create_thread(controlled_thread, &thread_arg_A, PRIO_A, &tid_A);
    create_thread(controlled_thread, &thread_arg_B, PRIO_B, &tid_B);
    create_thread(controlled_thread, &thread_arg_C, PRIO_C, &tid_C);

    create_thread(catcher_thread, NULL, PRIO_CATCHER, &tid_catcher);
    

    /* We create the thread group, get its clock and create a timer */
    /****************************************************************/
    CHK( marte_threadset_create(&thread_set) );
    CHK( marte_getgroupcpuclockid(thread_set, &group_clock_id) );
    CHK( my_timer_create(group_clock_id, SIGNAL_TIMER, siginfo_timer, &group_timer) );

    /* We add the 3 threads to the group */
    /*************************************/
    CHK( marte_threadset_add(thread_set, tid_A) );
    CHK( marte_threadset_add(thread_set, tid_B) );
    CHK( marte_threadset_add(thread_set, tid_C) );


    /* First experiment:  We arm the group CPU timer with 5 sec and */
    /* make each thread execute two seconds each.                   */
    /****************************************************************/
    eat_time_A = 2;
    eat_time_B = 2;
    eat_time_C = 2;

    group_time.tv_sec = 5;
    group_time.tv_nsec = 0;

    timer_expired = false;
    time_printf("MAIN:  Arming group timer for 5 seconds\n");
    CHK( rel_timer_arm(group_timer, &group_time) );

    CHK( sigqueue(0, SIGNAL_A, siginfo_A.si_value) );
    CHK( sigqueue(0, SIGNAL_B, siginfo_B.si_value) );
    CHK( sigqueue(0, SIGNAL_C, siginfo_C.si_value) );

    time_printf("MAIN:  Back after all threads\n");

    assert(timer_expired != false);
    
    printf("Test OK\n");
    return 0;
}


// ----------------------------------------------------------------

/**
 *  This is a controlled thread.  It stays waiting for a signal and
 *  then eats the requested time sent in the signal info.
 **/
static void *controlled_thread(void *arg)
{
    sigset_t signal_set;


    my_thread_arg_t *thread_arg = (my_thread_arg_t *) arg;

    time_printf("%s: Initializing\n", thread_arg->identifier);

    sigemptyset(&signal_set);
    sigaddset(&signal_set, thread_arg->signum);
    sigaddset(&signal_set, thread_arg->sigkill);

    CHK( pthread_sigmask(SIG_BLOCK, &signal_set, NULL) );

    while(1)
    {
        int signal_received;
        siginfo_t info_received;
        float *eat_time;

        signal_received = sigwaitinfo(&signal_set, &info_received);

        if (signal_received == thread_arg->sigkill)
        {
            time_printf("%s: Terminating\n", thread_arg->identifier);
            break;
        }

        assert(signal_received == thread_arg->signum);

        eat_time = (float *) info_received.si_value.sival_ptr;

        time_printf("%s: about to eat %f sec\n", thread_arg->identifier, *eat_time);

        eat(*eat_time);
    }
        
    return NULL;
}

// ------------------------------------------------------------------

static void *catcher_thread(void *arg)
{
    sigset_t signal_set;

    time_printf("CATCHER:  Initializing\n");

    sigemptyset(&signal_set);
    sigaddset(&signal_set, SIGNAL_TIMER);

    CHK( pthread_sigmask(SIG_BLOCK, &signal_set, NULL) );

    while(1)
    {
        int signal_received;
        siginfo_t info_received;

        signal_received = sigwaitinfo(&signal_set, &info_received);
        assert(signal_received == SIGNAL_TIMER);

        time_printf("CATCHER:  Group timer expired!!!\n");
        timer_expired = true;
    }

    return NULL;
}


// ------------------------------------------------------------------------

static void create_thread(thread_code_t thread_code,  
                          void *arg, 
                          int priority, 
                          pthread_t *tid)
{
    pthread_attr_t attr;
    struct sched_param param;


    CHK( pthread_attr_init(&attr) );

    // detachstate = detached thread (no join operation allowed)
    CHK( pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED)  );

    // inheritsched = explicit, so that we can explicitly set the attributes
    CHK( pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED)  );

    // schedpolicy = fixed priorities
    CHK( pthread_attr_setschedpolicy(&attr, SCHED_FIFO) );

    param.sched_priority = priority;
    CHK( pthread_attr_setschedparam(&attr, &param)  );
    
    CHK( pthread_create(tid, &attr, thread_code, arg) );

}


// ------------------------------------------------------------------------


static int thread_set_prio(pthread_t tid, int prio)
{
    struct sched_param param;
    int policy, ret_value;

    ret_value = pthread_getschedparam(tid, &policy, &param);
    if (ret_value!=0) return ret_value;

    param.sched_priority = prio;
    return pthread_setschedparam(tid, policy, &param);
}


// ------------------------------------------------------------------------

static int my_timer_create(clockid_t clockid, int signal, siginfo_t info, timer_t *timer)
{
    struct sigevent evp;
    evp.sigev_notify = SIGEV_SIGNAL;
    evp.sigev_signo = signal;
    evp.sigev_value = info.si_value;

    return timer_create(clockid, &evp, timer);
}



// ------------------------------------------------------------------------

static int rel_timer_arm(timer_t timerid, struct timespec *value)
{
    struct itimerspec timer_value;

    // set the timer to the specified value, one shot only
    timer_value.it_value = *value;
    timer_value.it_interval.tv_sec = 0;
    timer_value.it_interval.tv_nsec = 0;
    
    // arm the timer
    return timer_settime(timerid, 0, &timer_value, NULL);
}


// ------------------------------------------------------------------------



static void time_printf(const char *format, ...)
{
    va_list args;

    struct timespec current_time;

    clock_gettime(CLOCK_MONOTONIC, &current_time);
    
    decr_timespec(&current_time, &start_time);


    printf("%ld:", current_time.tv_sec * 1000L + current_time.tv_nsec/1000000);

    va_start(args, format);
    vprintf(format, args);
    va_end(args);
}

