/*
 * MaRTE OS
 * Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
 *
 * This file has been generated automatically by 'mkmarte'
 * using constants defined in the Ada part of the kernel.
 *
 * Do Not Edit.
 */

#ifndef _MARTE_SYS_MARTE_SIGNALS_H_
#define _MARTE_SYS_MARTE_SIGNALS_H_

#define NSIG         63

#define SIGHUP	 1 /* hangup                                       */
#define SIGINT	 2 /* interrupt                                    */
#define SIGQUIT	 3 /* quit                                         */
#define SIGILL	 4 /* illegal instruction                          */
#define SIGTRAP	 5 /* trace trap                                   */
#define SIGABRT	 6 /* abort()                                      */
#define SIGBUS	 7 /* bus error                                    */
#define SIGFPE	 8 /* floating point exception                     */
#define SIGKILL	 9 /* kill (cannot be caught or ignored)           */
#define SIGUSR1	10 /* user defined signal 1                        */
#define SIGSEGV	11 /* segmentation violation                       */
#define SIGUSR2	12 /* user defined signal 2                        */
#define SIGPIPE	13 /* write on a pipe with no one to read it       */
#define SIGALRM	14 /* alarm clock                                  */
#define SIGTERM	15 /* Termination Signal                           */
#define SIGCHLD	17 /* to parent on child stop or exit              */
#define SIGCONT	18 /* continue a stopped process                   */
#define SIGSTOP	19 /* sendable stop signal not from tty            */
#define SIGTSTP	20 /* stop signal from tty                         */
#define SIGTTIN	21 /* Read from ctr term by member of bg proc group*/
#define SIGTTOU	22 /* Write to ctr term by member of bg proc group */
#define SIGVTALRM	26 /* virtual timer expired                        */
#define SIGPROF	27 /* PROF timer expired                           */
#define SIGUNUSED	31 /* unused signal                                */
#define SIGRTMIN	32 /* minimum value for the RT signals             */
#define SIGRTMAX	63 /* maximum value for the RT signals             */

#endif /* _MARTE_SYS_MARTE_SIGNALS_H_ */
