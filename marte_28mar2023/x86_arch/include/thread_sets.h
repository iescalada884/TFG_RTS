/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                           'p t h r e a d _ s e t s'
 *
 *                                      H
 *
 * File 'pthread_sets.h'                                              by MAR.
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
 *--------------------------------------------------------------------------*/

#ifndef	_MARTE_PTHREAD_SETS_H_
#define _MARTE_PTHREAD_SETS_H_

#include <sys/cpp_macros.h>
#include <sys/types.h>

CPP_BEGIN_DECLS

#define NULL_THREAD_SET 0

int marte_threadset_create(marte_thread_set_t *set);
// Errors
//   [EAGAIN] no resources free

int marte_threadset_destroy(marte_thread_set_t set);
// Errors
//   [EINVAL] invalid set

int marte_threadset_empty(marte_thread_set_t set);
// Errors
//   [EINVAL] invalid set

int marte_threadset_add(marte_thread_set_t set, pthread_t thread_id);
// Errors
//   [EINVAL]  invalid set
//   [ENOTSUP] thread already a member of some other group
//   [EAGAIN]  no resources free
//   [ESRCH]   thread_id doesn't identify a valid thread

int marte_threadset_del(marte_thread_set_t set, pthread_t thread_id);
// Errors
//   [EINVAL]  invalid set
//   [EINVAL]  thread is not a member of the group
//   [ESRCH]   thread_id doesn't identify a valid thread

int marte_threadset_first(marte_thread_set_t set, pthread_t *thread_id);
// Errors
//   [EINVAL]  invalid set
//   [ESRCH]   the set is empty

int marte_threadset_next(marte_thread_set_t set, pthread_t *thread_id);
// Errors
//   [EINVAL]  invalid set
//   [EINVAL]  thread has been removed from the set since the last call to
//             marte_threadset_first or  marte_threadset_next
//   [ESRCH]   there aren't more threads in set

int marte_threadset_getset(pthread_t thread_id, marte_thread_set_t *set);
// Return Value
//    If successful, the marte_threadset_getset() function shall store the set
//    of the thread at *set (or the value NULL_THREAD_SET if the thread does
//    not belong to any group) and shall return zero. Otherwise, an error
//    number shall be returned to indicate the error.
// Errors
//   [ESRCH]   thread_id doesn't identify a valid thread

int marte_threadset_ismember(const marte_thread_set_t *set,
			     pthread_t thread_id,
			     int * is_member);
// Errors
//   [EINVAL]  invalid set
//   [ESRCH]   thread_id doesn't identify a valid thread

int marte_getgroupcpuclockid (const marte_thread_set_t set,
			      clockid_t *clock_id);
// Errors
//   [EINVAL]  invalid set
//   [EINVAL]  clock_id is a null pointer

CPP_END_DECLS
#endif /* _MARTE_PTHREAD_SETS_H_ */
