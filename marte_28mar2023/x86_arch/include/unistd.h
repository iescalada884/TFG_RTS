/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                'u n i s t d'
 *
 *                                      H
 *
 * File 'unistd.h'                                     by Mar. and Fguerreira
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

#ifndef _MARTE_UNISTD_H_
#define _MARTE_UNISTD_H_
#include <sys/cpp_macros.h>
#include <sys/types.h>

CPP_BEGIN_DECLS

/* Conformance with the Minimal Realtime System Profile */
#define _POSIX_AEP_REALTIME_MINIMAL  1

/* Presence of optional features */
#define _POSIX_AEP_REALTIME_LANG_C89  1

/* Job control is NOT supported.  */
#undef _POSIX_JOB_CONTROL // NOT suported in PSE51

/* Processes have a saved set-user-ID and a saved set-group-ID.  */
#undef _POSIX_SAVED_IDS // NOT suported in PSE51

/* Priority scheduling is supported.  */
#define _POSIX_PRIORITY_SCHEDULING      1

/* Synchronizing file data is supported.  */
#define _POSIX_SYNCHRONIZED_IO  1

/* The fsync function is present.  */
//#define _POSIX_FSYNC    1 Not yet

/* Mapping of files to memory is supported.  */
#undef _POSIX_MAPPED_FILES // NOT suported in PSE51

/* Locking of all memory is supported.  */
//#define _POSIX_MEMLOCK  1 Not yet

/* Locking of ranges of memory is supported.  */
//#define _POSIX_MEMLOCK_RANGE    1 Not yet

/* Setting of memory protections is supported.  */
#undef _POSIX_MEMORY_PROTECTION // NOT suported in PSE51

/* Implementation supports `poll' function.  */
//#define _POSIX_POLL     1 ??

/* Implementation supports `select' and `pselect' functions.  */
//#define _POSIX_SELECT   1 ??

/* Only root can change owner of file.  */
#undef _POSIX_CHOWN_RESTRICTED // NOT suported in PSE51

/* `c_cc' member of 'struct termios' structure can be disabled by
   using the value _POSIX_VDISABLE.  */
#undef _POSIX_VDISABLE // NOT suported in PSE51

/* Filenames are not silently truncated.  */
#define _POSIX_NO_TRUNC 1

/* Tell we have POSIX threads.  */
#define _POSIX_THREADS  1

/* We have the reentrant functions described in POSIX.  */
//#define _POSIX_REENTRANT_FUNCTIONS      1 ??
//#define _POSIX_THREAD_SAFE_FUNCTIONS    1 Not yet

/* We provide priority scheduling for threads.  */
#define _POSIX_THREAD_PRIORITY_SCHEDULING       1

/* We provide priority protection protocol for mutexes.  */
#define _POSIX_THREAD_PRIO_PROTECT       1

/* We provide priority inheritance protocol for mutexes.  */
#define _POSIX_THREAD_PRIO_INHERIT       1

/* We support user-defined stack sizes.  Not yet */
#undef _POSIX_THREAD_ATTR_STACKSIZE

/* We support user-defined stacks.  Not yet  */
#undef _POSIX_THREAD_ATTR_STACKADDR

/* Semaphores are not supported.  */
#undef _POSIX_SEMAPHORES

/* Real-time signals are supported.  */
//#define _POSIX_REALTIME_SIGNALS 1 Not yet

/* We support asynchronous I/O.  */
#undef _POSIX_ASYNCHRONOUS_IO // NOT suported in PSE51

/* We support POSIX timers.  */
#define _POSIX_TIMERS  1

/* Message queues are not supported.  */
#undef _POSIX_MESSAGE_PASSING

/* POSIX Version  */
#define _POSIX_VERSION  199506L

#ifndef NULL
#define NULL		0
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned int size_t; // Don't change the size of size_t
#endif
#ifndef _SSIZE_T
#define _SSIZE_T
typedef signed int ssize_t;  // Don't change the size of ssize_t
#endif

int close(int __fd);
ssize_t read(int __fd, void *__buf, size_t __n);
ssize_t write(int __fd, const void *__buf, size_t __n);
int ioctl(int __fd, int __request, void *__arg);
int unlink(const char *__path);
off_t lseek(int __fd, off_t __offset, int __whence);

// lseek constants for parameter __whence
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

// Standard file streams
#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

unsigned int sleep(unsigned int seconds);
int usleep(useconds_t useconds);

void _exit(int status);
/* Not implemented */
char *getcwd(char *buf, size_t size);

/*
int access(const char *__name, oskit_mode_t __mode);
int chdir(const char *__path);
int chown(const char *__path, oskit_uid_t __owner, oskit_gid_t __group);
int dup(int __fd);
int dup2(int __oldfd, int __newfd);
int link(const char *__path1, const char *__path2);
int rmdir(const char *__path);

unsigned int alarm(unsigned int);
int execl(const char *, const char *, ...);
int execle(const char *, const char *, ...);
int execlp(const char *, const char *, ...);
int execv(const char *, char * const *);
int execve(const char *, char * const *, char * const *);
int execvp(const char *, char * const *);
int fdatasync(int);
oskit_pid_t fork(void);
long fpathconf(int, int);
int fsync(int);
int ftruncate(int, oskit_off_t);
char *getcwd(char *, size_t);
oskit_gid_t getegid(void);
oskit_uid_t geteuid(void);
oskit_gid_t getgid(void);
int getgroups(int, oskit_gid_t[]);
char *getlogin(void);
int gethostname(char *, int);
oskit_pid_t getpgrp(void);
oskit_pid_t getpid(void);
oskit_pid_t getppid(void);
oskit_uid_t getuid(void);
int isatty(int);
long pathconf(const char *, int);
int pause(void);
int pipe(int *);
int setgid(oskit_gid_t);
int setpgid(oskit_pid_t, oskit_pid_t);
oskit_pid_t setsid(void);
int setuid(oskit_uid_t);
long sysconf(int);
oskit_pid_t tcgetpgrp(int);
int tcsetpgrp(int, oskit_pid_t);
char *ttyname(int);

extern char *optarg;
extern int optind, optopt, opterr, optreset;

int fchdir(int __fd);
int fchown(int __fd, oskit_uid_t __owner, oskit_gid_t __group);
int getopt(int argc, char * const argv[], const char *optstring);
int lchown(const char *__path, oskit_uid_t __owner, oskit_gid_t __group);
int readlink(const char  *__path, void *__buf, size_t __n);
int symlink(const char *__path1, const char *__path2);

int mknod(const char *__name, oskit_mode_t __mode, oskit_dev_t __dev); */

CPP_END_DECLS
#endif /* _MARTE_UNISTD_H_ */
