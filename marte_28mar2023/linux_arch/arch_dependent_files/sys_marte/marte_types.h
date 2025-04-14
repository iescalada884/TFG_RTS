/*
 * MaRTE OS
 * Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
 *
 * This file has been generated automatically by 'mkmarte'
 * using constants defined in the Ada part of the kernel.
 *
 * Do Not Edit.
 */

#ifndef _MARTE_SYS_MARTE_TYPES_H_
#define _MARTE_SYS_MARTE_TYPES_H_

typedef struct { char b[344]; } marte_TCB_t;
typedef struct { char b[136]; } marte_pthread_attr_t;
typedef struct { char b[140]; } marte_pthread_mutex_t;
typedef struct { char b[88]; } marte_pthread_mutexattr_t;
typedef struct { char b[440]; } marte_pthread_cond_t;
typedef struct { char b[4]; } marte_pthread_condattr_t;
typedef struct { char b[64]; } marte_sigset_t;
typedef struct { char b[424]; } marte_sem_t;
typedef struct { char b[288]; } marte_posix_appsched_actions_t;
typedef struct { char b[4]; } marte_pthread_key_t;
typedef struct { char b[4]; } marte_posix_appsched_eventset_t;
typedef struct { char b[146]; } marte_pthread_once_t;
#define _MARTE_PTHREAD_ONCE_INIT {{'p', 'O', 'N', 'i', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}}
typedef struct { char b[92]; } _marte_timed_handler_t;
typedef struct { char b[4]; } _marte_thread_set_t;

#endif /* _MARTE_SYS_MARTE_TYPES_H_ */
