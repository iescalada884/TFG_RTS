/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                             's y s / t y p e s'
 *
 *                                      H
 *
 * File 'types.h'                                                      by MAR.
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
#ifndef _SYS_TYPES_H_
#define _SYS_TYPES_H_
#include <sys/marte_types.h>
#include <sys/marte_configuration_parameters.h>

#if (MARTE_ARCHITECTURE != ARCH_LINUX_LIB && MARTE_ARCHITECTURE != ARCH_RPI)
typedef long ptrdiff_t;
#endif

typedef int clockid_t;

typedef int clock_t;

#ifndef  __dev_t_defined
typedef unsigned long long dev_t;
# define __dev_t_defined
#endif

#ifndef  __gid_t_defined
typedef unsigned int gid_t;
# define __gid_t_defined
#endif

#ifndef  __ino_t_defined
typedef unsigned int ino_t;
# define __ino_t_defined
#endif

#ifndef  __mode_t_defined
typedef unsigned int mode_t;
# define __mode_t_defined
#endif

#ifndef  __nlink_t_defined
typedef unsigned int nlink_t;
# define __nlink_t_defined
#endif

#ifndef __off_t_defined
typedef int off_t;
#define __off_t_defined
#endif

#ifndef __pid_t_defined
typedef int pid_t;
#define __pid_t_defined
#endif

#ifndef  _SIZE_T
typedef unsigned int size_t;  // Don't change the size of size_t
# define _SIZE_T
#endif

#ifndef   _SSIZE_T
# ifndef  __ssize_t_defined
typedef signed int ssize_t;  // Don't change the size of ssize_t
#  define _SSIZE_T
#  define __ssize_t_defined
# endif
#endif

typedef int time_t;

typedef int *timer_t;

typedef long suseconds_t;

#ifndef  __uid_t_defined
typedef unsigned int uid_t;
# define __uid_t_defined
#endif

#ifndef  __useconds_t_defined
typedef unsigned long useconds_t;
# define __useconds_t_defined
#endif

#ifndef  _BITS_PTHREADTYPES_H
# define _BITS_PTHREADTYPES_H    1
#endif
// To avoid inclusion of '/usr/include/bits/pthreadtypes.h' in
// architectures linux and linux_lib.
// '/usr/include/bits/pthreadtypes.h' redefines types as
// 'pthread_attr_t', 'pthread_cond_t', etc.

/* Thread type */
typedef marte_TCB_t *pthread_t;

typedef marte_pthread_attr_t pthread_attr_t;

/* Mutexes types */
typedef marte_pthread_mutex_t pthread_mutex_t;

typedef  marte_pthread_mutexattr_t pthread_mutexattr_t;

/* Condition Variables types */
typedef marte_pthread_cond_t pthread_cond_t;

typedef marte_pthread_condattr_t pthread_condattr_t;

/* Semaphore types */
typedef marte_sem_t sem_t;

/* Thread-specific data keys */
typedef marte_pthread_key_t pthread_key_t;

/* Pthread once */
typedef marte_pthread_once_t pthread_once_t;

/* Timed Handlers */
typedef _marte_timed_handler_t marte_timed_handler_t;

/* Thread Sets */
typedef _marte_thread_set_t marte_thread_set_t;

#endif /* _SYS_TYPES_H_ */
