#ifndef _TARGET_H_
#define _TARGET_H_

// #include <pthread.h>
//
// #define TLSF_MLOCK_T            pthread_mutex_t
// #define TLSF_CREATE_LOCK(l)     pthread_mutex_init (l, NULL)
// #define TLSF_DESTROY_LOCK(l)    pthread_mutex_destroy(l)
// #define TLSF_ACQUIRE_LOCK(l)    pthread_mutex_lock(l)
// #define TLSF_RELEASE_LOCK(l)    pthread_mutex_unlock(l)

// << MaRTE OS
#include <sys/kernel.h>
#define TLSF_MLOCK_T            unsigned
#define TLSF_CREATE_LOCK(l)
#define TLSF_DESTROY_LOCK(l)
#define TLSF_ACQUIRE_LOCK(l)    kernel_enter_critic_section(l)
#define TLSF_RELEASE_LOCK(l)    kernel_leave_critic_section(*(l))
// >> MaRTE OS

#endif
