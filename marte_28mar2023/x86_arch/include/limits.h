/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                'l i m i t s'
 *
 *                                      H
 *
 * File 'limits.h'                                                     by Mar.
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


#ifndef _MARTE_LIMITS_H_
#define _MARTE_LIMITS_H_

#include <sys/marte_signals.h>
#include <sys/marte_configuration_parameters.h>


/*
 * POSIX Minimum and Maximum Values
 */

/* Minimum number of operations in one list I/O call.  */
#define _POSIX_AIO_LISTIO_MAX   2

/* Minimal number of outstanding asynchronous I/O operations.  */
#define _POSIX_AIO_MAX          1

/* Maximum length of arguments to `execve', including environment.  */
#define _POSIX_ARG_MAX          4096

/* Maximum simultaneous processes per real user ID.  */
#define _POSIX_CHILD_MAX        6

/* Minimal number of timer expiration overruns.  */
#define _POSIX_DELAYTIMER_MAX   32

/* Maximum link count of a file.  */
#define _POSIX_LINK_MAX         8

/* Number of bytes in a terminal canonical input queue.  */
#define _POSIX_MAX_CANON        255

/* Number of bytes for which space will be
   available in a terminal input queue.  */
#define _POSIX_MAX_INPUT        255

/* Maximum number of message queues open for a process.  */
#define _POSIX_MQ_OPEN_MAX      8

/* Maximum number of supported message priorities.  */
#define _POSIX_MQ_PRIO_MAX      32

/* Number of simultaneous supplementary group IDs per process.  */
#define _POSIX_NGROUPS_MAX      0

/* Number of files one process can have open at once.  */
#define _POSIX_OPEN_MAX         16

/* Number of descriptors that a process may examine with `pselect' or
   `select'.  */
#define _POSIX_FD_SETSIZE       _POSIX_OPEN_MAX

/* Number of bytes in a filename.  */
#define _POSIX_NAME_MAX         14

/* Number of bytes in a pathname.  */
#define _POSIX_PATH_MAX         255

/* Number of bytes than can be written atomically to a pipe.  */
#define _POSIX_PIPE_BUF         512

/* Minimal number of realtime signals reserved for the application.  */
#define _POSIX_RTSIG_MAX        8

/* Number of semaphores a process can have.  */
#define _POSIX_SEM_NSEMS_MAX    256

/* Maximal value of a semaphore.  */
#define _POSIX_SEM_VALUE_MAX    32767

/* Number of pending realtime signals.  */
#define _POSIX_SIGQUEUE_MAX     _MARTE_SIGQUEUE_MAX

/* Largest value of a `ssize_t'.  */
#define _POSIX_SSIZE_MAX        32767

/* Number of streams a process can have open at once.  */
#define _POSIX_STREAM_MAX       8

/* Maximum length of a timezone name (element of `tzname').  */
#define _POSIX_TZNAME_MAX       3

/* Maximum number of connections that can be queued on a socket.  */
#define _POSIX_QLIMIT           1

/* Maximum number of bytes that can be buffered on a socket for send
   or receive.  */
#define _POSIX_HIWAT            _POSIX_PIPE_BUF

/* Maximum number of elements in an `iovec' array.  */
#define _POSIX_UIO_MAXIOV       16

/* Maximum number of characters in a tty name.  */
#define _POSIX_TTY_NAME_MAX     9

/* Number of timer for a process.  */
#define _POSIX_TIMER_MAX        32

/* Maximum length of login name.  */
#define _POSIX_LOGIN_NAME_MAX   9

/* Maximum clock resolution in nanoseconds.  */
#define _POSIX_CLOCKRES_MIN     20000000

/*
 * Runtime Invariant Values
 */
#define RTSIG_MAX SIGRTMAX - SIGRTMIN + 1

#ifndef SSIZE_MAX
# define SSIZE_MAX      INT_MAX
#endif


/* This value is a guaranteed minimum maximum. */

#ifndef NGROUPS_MAX
# define NGROUPS_MAX    _POSIX_NGROUPS_MAX
#endif

/* These assume 8-bit `char's, 16-bit `short int's,
   and 32-bit `int's and `long int's.  */
/* taken from Linux '/usr/include/limits.h' */
#define __WORDSIZE      32

/* Number of bits in a `char'.	*/
#  define CHAR_BIT	8

/* Minimum and maximum values a `signed char' can hold.  */
#  define SCHAR_MIN	(-128)
#  define SCHAR_MAX	127

/* Maximum value an `unsigned char' can hold.  (Minimum is 0.)  */
#  define UCHAR_MAX	255

/* Minimum and maximum values a `char' can hold.  */
#  ifdef __CHAR_UNSIGNED__
#   define CHAR_MIN	0
#   define CHAR_MAX	UCHAR_MAX
#  else
#   define CHAR_MIN	SCHAR_MIN
#   define CHAR_MAX	SCHAR_MAX
#  endif

/* Minimum and maximum values a `signed short int' can hold.  */
#  define SHRT_MIN	(-32768)
#  define SHRT_MAX	32767

/* Maximum value an `unsigned short int' can hold.  (Minimum is 0.)  */
#  define USHRT_MAX	65535

/* Minimum and maximum values a `signed int' can hold.  */
#  define INT_MIN	(-INT_MAX - 1)
#  define INT_MAX	2147483647

/* Maximum value an `unsigned int' can hold.  (Minimum is 0.)  */
#  define UINT_MAX	4294967295U

/* Minimum and maximum values a `signed long int' can hold.  */
#  if __WORDSIZE == 64
#   define LONG_MAX	9223372036854775807L
#  else
#   define LONG_MAX	2147483647L
#  endif
#  define LONG_MIN	(-LONG_MAX - 1L)

/* Maximum value an `unsigned long int' can hold.  (Minimum is 0.)  */
#  if __WORDSIZE == 64
#   define ULONG_MAX	18446744073709551615UL
#  else
#   define ULONG_MAX	4294967295UL
#  endif

#  ifdef __USE_ISOC99

/* Minimum and maximum values a `signed long long int' can hold.  */
#   define LLONG_MAX	9223372036854775807LL
#   define LLONG_MIN	(-LLONG_MAX - 1LL)

/* Maximum value an `unsigned long long int' can hold.  (Minimum is 0.)  */
#   define ULLONG_MAX	18446744073709551615ULL

#  endif /* ISO C99 */


#endif // _MARTE_LIMITS_H_
