/*
 * MaRTE OS
 * Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
 *
 * This file has been generated automatically by 'mkmarte'
 * using constants defined in the Ada part of the kernel.
 *
 * Do Not Edit.
 */

#ifndef _MARTE_SYS_MARTE_GENERAL_CONSTANTS_H_
#define _MARTE_SYS_MARTE_GENERAL_CONSTANTS_H_

#define _MARTE_SCHED_FIFO                                0 /* FIFO scheduling policy             */
#define _MARTE_SCHED_RR                                  1 /* Round robin scheduling policy      */
#define _MARTE_SCHED_SPORADIC                            2 /* Sporadic Server scheduling policy  */
#define _MARTE_SCHED_OTHER                               3 /* Another scheduling policy          */
#define _MARTE_SCHED_APP                                 4 /* Application-defined sched. policy  */
#define _MARTE_SCHED_EDF                                 5 /* EDF scheduling policy              */
#define _MARTE_PTHREAD_CREATE_JOINABLE                   0 /* Joinable pthread                   */
#define _MARTE_PTHREAD_CREATE_DETACHED                   1 /* Detached pthread                   */
#define _MARTE_PTHREAD_SCOPE_SYSTEM                     10 /* System scheduling contention scope */
#define _MARTE_PTHREAD_SCOPE_PROCESS                    11 /* Process scheduling contention scope*/
#define _MARTE_PTHREAD_INHERIT_SCHED                    20 /* Policy and attributes inherited    */
#define _MARTE_PTHREAD_EXPLICIT_SCHED                   21 /* Policy and attributes explicit     */
#define _MARTE_SIG_BLOCK                                 0 /* Add signals to the current set     */
#define _MARTE_SIG_UNBLOCK                               1 /* Remove signals from the current set*/
#define _MARTE_SIG_SETMASK                               2 /* Assign current set                 */
#define _MARTE_SIG_DFL                                   1 /* Request for default signal handling*/
#define _MARTE_SIG_IGN                                   2 /* Request that signal be ignored     */
#define _MARTE_SIGEV_NONE                                0 /* No notification when event occurs  */
#define _MARTE_SIGEV_SIGNAL                              1 /* Generate signal when event occurs  */
#define _MARTE_SA_SIGINFO                                1 /* Real-Time signal behaviour         */
#define _MARTE_SI_USER                                   0 /* Signal sent by the kill function   */
#define _MARTE_SI_QUEUE                                  1 /* Signal sent by sigqueue function   */
#define _MARTE_SI_TIMER                                  2 /* Signal sent by timer expiration    */
#define _MARTE_PTHREAD_PRIO_NONE                         0 /* No priority inheritance            */
#define _MARTE_PTHREAD_PRIO_PROTECT                      2 /* Highest ceiling priority protocol  */
#define _MARTE_PTHREAD_PRIO_INHERIT                      1 /* Highest blocked task protocol      */
#define _MARTE_PTHREAD_APPSCHED_PROTOCOL                 3 /* Application-defined protocol       */
#define _MARTE_CLOCK_REALTIME                            1 /* Realtime clock                     */
#define _MARTE_CLOCK_THREAD_CPUTIME_ID                   2 /* CPU-time clock of calling thread   */
#define _MARTE_CLOCK_MONOTONIC                           3 /* Monotonic clock                    */
#define _MARTE_CLOCK_INTERRUPTS_CPUTIME                  4 /* Time consumed by interrupt handlers*/
#define _MARTE_TIMER_ABSTIME                             1 /* Absolute timer                     */
#define _MARTE_PERIODIC_HANDLER                          2 /* Periodic Timed Handler             */
#define _MARTE_PTHREAD_REGULAR                          20 /* Regular thread                     */
#define _MARTE_PTHREAD_APPSCHEDULER                     21 /* Application scheduler thread       */
#define _MARTE_POSIX_APPSCHED_ABSTIMEOUT                 2 /*                                    */
#define _MARTE_O_RDONLY                                  1 /* File open for reading only         */
#define _MARTE_O_WRONLY                                  2 /* File open for writing only         */
#define _MARTE_O_RDWR                                    3 /* File open for reading and writing  */
#define _MARTE_O_NONBLOCK                                8 /* Non-blocking read and write        */
#define _MARTE_O_CREAT                                  16 /* If file does not exist, create     */
#define _MARTE_SEEK_SET                                  0 /* Set file offset to offset          */
#define _MARTE_SEEK_CUR                                  1 /* Set offset to current plus offset  */
#define _MARTE_SEEK_END                                  2 /* Set offset to EOF plus offset      */
#define _MARTE_POSIX_INTR_HANDLED_NOTIFY                30 /* Interrupt handled, notify thread   */
#define _MARTE_POSIX_INTR_HANDLED_DO_NOT_NOTIFY         31 /* Interrupt handled, do not notify   */
#define _MARTE_POSIX_INTR_NOT_HANDLED                   32 /* Interrupt not handled              */
#define _MARTE_ITIMER_REAL                              40 /* Decrements in real time            */
#define _MARTE_ITIMER_VIRTUAL                           41 /* Decrements in process virtual time */
#define _MARTE_ITIMER_PROF                              42 /* Dec. both in p. v. time and system */

#endif /* _MARTE_SYS_MARTE_GENERAL_CONSTANTS_H_ */
