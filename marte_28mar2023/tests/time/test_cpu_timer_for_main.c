//  Test for all architectures
/*
 * test_cpu_timer_for_main.c
 *
 * The main thread programs a CPU-timer for itself. The signal is accepted by
 * another thread.
 *
 */
#include <assert.h>
#include <stdlib.h> // for exit in assert
#include <stdio.h>
#include <pthread.h>
#include <sched.h>
#include <time.h>
#include <signal.h>
#include <misc/timespec_operations.h>
#include <misc/error_checks.h>
#include <misc/load_loop.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define NUM_SIGNAL_LOOPS 5
#define SIG_INFO 101
#define NS_PROG_TIMER 2e6
#define S_EAT_TIME (NS_PROG_TIMER / 1e9 * 5)
#define SIGNAL_NUM SIGRTMIN

struct timespec t_arm_timer, t_accept_signal;

// counter of the number of signals accepted by sigwaiter
volatile int num_accepted_signals=0;

/*
 * Thread that waits for timer signals
 */
void * sigwaiter(void *arg) {
  sigset_t set;
  siginfo_t sig_info;

  sigemptyset(&set);
  sigaddset(&set,SIGNAL_NUM);

  while (1) {
    printf("sigwaiter: waits signal\n");
    // wait signal
    CHKE( sigwaitinfo(&set, &sig_info) );
    clock_gettime(CLOCK_MONOTONIC, &t_accept_signal);
    printf ("sigwaiter: Signal %d accepted with info: (%d)\n",
	    sig_info.si_signo, sig_info.si_value.sival_int);
    decr_timespec(&t_accept_signal, &t_arm_timer);
    printf ("sigwaiter: dif time: %f\n", timespec_to_double(&t_accept_signal));

    assert(sig_info.si_value.sival_int==SIG_INFO);
    num_accepted_signals++;
  }
}

/*
 * main: arm timer to send signal
 */
int main() {
  sigset_t set;
  struct sigaction sigact;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  printf("S_PROG_TIMER:%f  S_EAT_TIME:%f\n", NS_PROG_TIMER / 1e9, S_EAT_TIME);

  // adjust eat
  adjust();
  
  // set as real-time signal
  sigact.sa_handler=SIG_DFL;
  sigact.sa_flags=SA_SIGINFO;
  CHKE (sigaction(SIGNAL_NUM, &sigact , NULL) );

  // set signal mask
  sigemptyset(&set);
  sigaddset(&set, SIGNAL_NUM);
  CHK( pthread_sigmask(SIG_BLOCK, &set, NULL) );

  // create sigwaiter thread
  pthread_t th_sigwaiter;
  pthread_attr_t attr;
  struct sched_param sch_param;
  CHK( pthread_attr_init(&attr) );
  CHK( pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED) );
  CHK( pthread_attr_setschedpolicy(&attr, SCHED_FIFO) ); 
  sch_param.sched_priority=sched_get_priority_max(SCHED_FIFO);
  CHK( pthread_attr_setschedparam(&attr, &sch_param) );

  CHK(pthread_create(&th_sigwaiter, &attr, sigwaiter, NULL)); 
 
  // create timer
  timer_t timer_id;
  struct sigevent event;
 
  event.sigev_notify = SIGEV_SIGNAL;
  event.sigev_signo = SIGNAL_NUM;
  event.sigev_value.sival_int = SIG_INFO;

  CHKE( timer_create (CLOCK_THREAD_CPUTIME_ID, &event, &timer_id) );
  
  // uses timer
  struct itimerspec timerdata = {.it_value={0, NS_PROG_TIMER},
				 .it_interval={0,0}};
   
  int i;
  for(i=0; i<NUM_SIGNAL_LOOPS; i++) {
    printf("main: arms timer\n");
    clock_gettime(CLOCK_MONOTONIC, &t_arm_timer);
    printf ("    at: %f\n", timespec_to_double(&t_arm_timer));

    clock_gettime(CLOCK_MONOTONIC, &t_arm_timer);
    
    // arm timer
    CHKE(timer_settime(timer_id, 0, &timerdata, NULL));
    eat(S_EAT_TIME);
  }
  
  printf("main: expected signal number: %d, accepted signal number: %d\n",
	 NUM_SIGNAL_LOOPS, num_accepted_signals);

  // Check the timer has expired all the times
  assert(num_accepted_signals==NUM_SIGNAL_LOOPS);

  printf("Test OK\n");
  return 0;
}
