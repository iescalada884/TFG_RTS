//  Test for all architectures
/*
** test_program_timer_in_the_past.c
**
** by Miguel marciano
**
** Program a timer for a time in the past. We use a watch dog thread
** to finish the program if the signal is nor delivered in a very
** short period of time.
*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>


#include <assert.h>
#include <sys/marte_configuration_parameters.h>

#include <pthread.h>
#include <signal.h>
#include <time.h>
#include <sched.h>

#include <misc/error_checks.h>
#include <misc/load_loop.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define SIGNAL_TO_SEND (SIGRTMIN + 2)

#define NS_DELAY 100000000

static struct timespec zero_time = {0, 0};

volatile int watch_dog_started = 0;

// Watch dog thread
void * watch_dog(void * arg) {
  struct timespec rel_time = {0, (long)(NS_DELAY*2.0)};
  watch_dog_started = 1;
  nanosleep(&rel_time, NULL);
  printf("\nERROR: watch_dog finishes process\n");
  exit(-1);
}

// main thread
int main()
{

    struct timespec past_time = {-1, 1};
    struct timespec current_time = {-1, 1};
    struct itimerspec arm_time = { { -1, -1}, {-1, -1} };

    timer_t timer;
    struct sigevent timer_sigevent;
    sigset_t signal_set;
    siginfo_t siginfo;
    pthread_t tid;
    pthread_attr_t attr;
    struct sched_param params;
    struct sigaction sigact;

    int my_test_value = 42;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

    adjust();

    // Real time signal and no handler to avoid signal delivery
    sigact.sa_handler=SIG_DFL;
    sigact.sa_flags=SA_SIGINFO;
    CHKE( sigaction(SIGNAL_TO_SEND, &sigact , NULL) );

    /* We block the signals that we are going to process */
    /*****************************************************/
    printf("Block signals\n");
    CHK(  sigemptyset(&signal_set)  );
    CHK(  sigaddset(&signal_set, SIGNAL_TO_SEND)  );
    CHK(  pthread_sigmask(SIG_BLOCK, &signal_set, NULL)  );

    // create watch dog thread (it could be removed when sigtimedwait
    // is implemented)
    CHK( pthread_attr_init(&attr) );
    params.sched_priority=sched_get_priority_max (SCHED_FIFO);
    CHK( pthread_attr_setschedpolicy(&attr, SCHED_FIFO) );
    CHK( pthread_attr_setschedparam(&attr, &params) );
    CHK( pthread_create(&tid, &attr, watch_dog, NULL) );

    assert(watch_dog_started);

    // defensive initializers
    // memset(&timer, 0xFF, sizeof(timer) );
    // memset(&timer_sigevent, 0xFF, sizeof(timer_sigevent) );
    // memset(&signal_set, 0xFF, sizeof(signal_set) );
    // memset(&siginfo, 0xFF, sizeof(siginfo) );


    /* We create a timer based on the FOSA_CLOCK_REALTIME */
    /******************************************************/
    printf("Create timer\n");
    timer_sigevent.sigev_notify = SIGEV_SIGNAL;
    timer_sigevent.sigev_signo = SIGNAL_TO_SEND;
    timer_sigevent.sigev_value.sival_int = my_test_value;
    CHK(  timer_create(CLOCK_MONOTONIC, &timer_sigevent, &timer)  );


    /* I get the time BEFORE doing the eat */
    /***************************************/
    printf("Get time (time0)\n");
    CHK(  clock_gettime(CLOCK_MONOTONIC, &past_time) );
    printf("Current time:  %d sec, %d nsec\n", past_time.tv_sec,
	   past_time.tv_nsec);

    /* Now I do some work for some secs */
    /************************************/
    printf("eat\n");
    eat(NS_DELAY / 1000000000.0);

    /* I read the time again */
    /*************************/
    printf("Read the time again\n");
    CHK(  clock_gettime(CLOCK_MONOTONIC, &current_time) );
    printf("Current time:  %d sec, %d nsec\n", current_time.tv_sec,
	   current_time.tv_nsec);

    /* I arm the timer to the past                                    */
    /* Since we have blocked this signal, the signal should be stored */
    /******************************************************************/
    printf("Arming the timer for time0\n");
    arm_time.it_value = past_time;
    arm_time.it_interval = zero_time;
    CHK(  timer_settime(timer, TIMER_ABSTIME, &arm_time, NULL) );

    /* Now I do the sigwait */
    /************************/
    printf("Waiting for the signal\n");

    CHKE( sigwaitinfo(&signal_set, &siginfo) );
    printf("Signal accepted!!!\n");
    printf("Received signal No: %d with info sival %d\n", siginfo.si_signo,
	   siginfo.si_value.sival_int);

    printf("Test OK\n");
    return 0;
}
