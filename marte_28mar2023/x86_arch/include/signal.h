/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *                                's i g n a l'
 *
 *                                      H
 *
 * File 'signal.h'                                                     By MAR.
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/

#ifndef	_SIGNAL_H_
#define	_SIGNAL_H_

#include <sys/cpp_macros.h>
#include <sys/types.h>
#include <sys/marte_types.h>
#include <sys/marte_general_constants.h>
#include <sys/marte_signals.h> // in this file are defined the signals numbers

CPP_BEGIN_DECLS

#define	SIG_DFL		(void (*)(int))_MARTE_SIG_DFL
#define	SIG_IGN		(void (*)(int))_MARTE_SIG_IGN

typedef marte_sigset_t sigset_t;
#define __sigset_t_defined 1

/*
 * POSIX 1003.1b: Generic value to pass back to an application.
 */
union sigval
{
  int	sival_int;
  void	*sival_ptr;
};
# define __have_sigval_t        1

/*
 * This structure is passed to signal handlers
 * that use the new SA_SIGINFO calling convention (see below).
 */
typedef struct
{
  int		si_signo;
  int		si_code;
  union sigval	si_value;
} siginfo_t;
# define __have_siginfo_t       1
/* Values for si_code, indicating the source of the signal */
#define SI_USER    _MARTE_SI_USER   /* sent by kill(), raise(), or abort() */
#define SI_QUEUE   _MARTE_SI_QUEUE  /* sent by sigqueue() */
#define SI_TIMER   _MARTE_SI_TIMER  /* generated by an expired timer */

/*
 * sigaction
 */
struct	sigaction {
  union {				/* signal handler */
    void (*sa_u_handler) (int);
    void (*sa_u_sigaction) (int, siginfo_t *, void *);
  } sa_u;
  sigset_t sa_mask;		/* signal mask to apply */
  int	   sa_flags;		/* see signal options below */
};
#define sa_handler	sa_u.sa_u_handler
#define sa_sigaction	sa_u.sa_u_sigaction
/* Signal options */
#define SA_SIGINFO _MARTE_SA_SIGINFO  // use sa_sigaction calling convention
#define SA_RESTART 0x10000000        // NOT IMPLEMENTED (same value as Linux)

/*
 * sigevent
 */
#define SIGEV_NONE   _MARTE_SIGEV_NONE  /* No notification when event occurs */
#define SIGEV_SIGNAL _MARTE_SIGEV_SIGNAL /* Generate signal when event occurs */
struct sigevent {
  int sigev_notify;
  int sigev_signo;
  union sigval sigev_value;
};
# define __have_sigevent_t      1
/*
 * Flags for sigprocmask:
 */
#define	SIG_BLOCK	_MARTE_SIG_BLOCK    /* block specified signal set */
#define	SIG_UNBLOCK	_MARTE_SIG_UNBLOCK  /* unblock specified signal set */
#define	SIG_SETMASK	_MARTE_SIG_SETMASK  /* set specified signal set */


int kill(int pid, int sig);

int sigemptyset(sigset_t *set);
int sigfillset(sigset_t *set);
int sigaddset(sigset_t *set, int signo);
int sigdelset(sigset_t *set, int signo);
int sigismember(const sigset_t *set, int signo);

int sigaction(int sig, const struct sigaction *act,
	      struct sigaction *oact);

int pthread_sigmask(int how, const sigset_t *set, sigset_t *oset);
int sigprocmask(int how, const sigset_t *set, sigset_t *oset); //  not implemented yet

int sigpending(sigset_t *set); //  not implemented yet
int sigsuspend(const sigset_t *mask); // not implemented yet

int sigwait(const sigset_t *set, int *sig);
int sigwaitinfo(const sigset_t *set, siginfo_t *info);
#include <time.h>
//int sigtimedwait(const sigset_t *set, siginfo_t *info,
//		 const struct timespec *timeout); (not implemented yet)

int sigqueue(int pid, int signo, const union sigval value);

int pthread_kill(pthread_t thread, int sig);

//unsigned int alarm(unsigned int seconds); (not implemented yet)

//int pause(void) (not implemented yet)

// void (*signal(int sig, void (*func)(int)))(int); // (not implemented yet)

int raise(int sig);

CPP_END_DECLS
#endif	/* _SIGNAL_H_ */
