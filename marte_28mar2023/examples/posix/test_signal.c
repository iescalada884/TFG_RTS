/*
 * Thread that accepts the SIGINT signal using sigwaitinfo. Displays a
 * message when received. Terminates the process when the fourth
 * signal is received.
 */
#include <stdio.h>
#include <signal.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <misc/error_checks.h>
static int error_status=-1;

void * sigwaiter (void * arg)
{
  sigset_t set;
  siginfo_t siginfo;
  int counter = 0;

  sigemptyset (&set);
  sigaddset (&set, SIGINT);
  /* The signal mask is inherited from the parent */
  /* It must have SIGINT masked */

  while (1) {
    CHKE( sigwaitinfo (&set, &siginfo) );
    if (siginfo.si_signo == SIGINT) {
      counter ++;
      if (counter < 4) {
	printf ("SIGINT %d received. signo: %d, si_code:%d, si_value:%d\n",
		counter, siginfo.si_signo, siginfo.si_code,
		siginfo.si_value.sival_int);
      } else {
	printf ("Final signal received\n");
	exit(0);
      }
    } else {
      /* unexpected signal */
      printf ("Unexpected signal %d received\n", siginfo.si_signo);
      pthread_exit ((void *)&error_status);
    }
  }
}


/* Main thread, that sets the mask for itself and its child */

int main ()
{
  sigset_t set;
  pthread_t th;
  union sigval value = {0};
  struct sigaction action;
  int ret;

  /* Set signal mask */
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  CHK( pthread_sigmask(SIG_BLOCK, &set, NULL) );

  /* Set SA_SIGINFO for SIGINT */
  /* This is unnecessary in MaRTE OS since all the signals behave as
     realtime signals by default, but it is necessary to write a full
     conforming POSIX application. */
  action.sa_handler = SIG_DFL;
  sigemptyset(&action.sa_mask);
  action.sa_flags = SA_SIGINFO;
  CHKE( sigaction (SIGINT, &action, NULL) );

  /* Create task */
  CHK( pthread_create(&th, NULL, sigwaiter, NULL) );

  /* Send signals */
  while (1) {
    value.sival_int++;
    printf ("Main sends signal %d\n", value.sival_int);
    CHKE( sigqueue (0, SIGINT, value) );
    sleep(5);
  }
}
