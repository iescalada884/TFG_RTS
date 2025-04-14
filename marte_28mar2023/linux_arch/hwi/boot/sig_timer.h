#ifndef __BOOT_SIG_TIMER_H__
#define __BOOT_SIG_TIMER_H__

#include <signal.h>

#ifdef CONFIG_USE_HIGH_RESOLTION_TIMERS

#define SIG_TIMER SIGUSR2

#else 

#define SIG_TIMER SIGALRM

#endif

#endif // __BOOT_SIG_TIMER_H__
