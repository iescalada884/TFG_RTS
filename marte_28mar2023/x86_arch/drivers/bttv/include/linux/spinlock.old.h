#ifndef __LINUX_SPINLOCK_H
#define __LINUX_SPINLOCK_H

#include "spinlock/stringify.h"
#include "spinlock/atomic.h"
#include "spinlock/system.h"
#include "spinlock/rwlock.h"

/*
 * Must define these before including other files, inline functions need them
 */
#define LOCK_SECTION_NAME			\
	".text.lock." __stringify(KBUILD_BASENAME)

#define LOCK_SECTION_START(extra)		\
	".subsection 1\n\t"			\
	extra					\
	".ifndef " LOCK_SECTION_NAME "\n\t"	\
	LOCK_SECTION_NAME ":\n\t"		\
	".endif\n\t"

#define LOCK_SECTION_END			\
	".previous\n\t"

/*
 * Your basic SMP spinlocks, allowing only a single CPU anywhere
 */

typedef struct {
	volatile unsigned long lock;
}spinlock_t;

#define SPINLOCK_MAGIC_INIT	/* */

#define SPIN_LOCK_UNLOCKED (spinlock_t) { 1 SPINLOCK_MAGIC_INIT }

#define spin_lock_init(x)	do { *(x) = SPIN_LOCK_UNLOCKED; } while(0)

/*
 * Simple spin lock operations.  There are two variants, one clears IRQ's
 * on the local processor, one does not.
 *
 * We make no fairness assumptions. They have a cost.
 */

#define spin_is_locked(x)	(*(volatile signed char *)(&(x)->lock) <= 0)
#define spin_unlock_wait(x)	do { barrier(); } while(spin_is_locked(x))

#define spin_lock_string \
	"\n1:\t" \
	"lock ; decb %0\n\t" \
	"js 2f\n" \
	LOCK_SECTION_START("") \
	"2:\t" \
	"rep;nop\n\t" \
	"cmpb $0,%0\n\t" \
	"jle 2b\n\t" \
	"jmp 1b\n" \
	LOCK_SECTION_END

#define spin_unlock_string \
	"movb $1,%0" \
		:"=m" (lock->lock) : : "memory"

static inline void _raw_spin_unlock(spinlock_t *lock)
{
	__asm__ __volatile__(
		spin_unlock_string
	);
}

static inline int _raw_spin_trylock(spinlock_t *lock)
{
	char oldval;
	__asm__ __volatile__(
		"xchgb %b0,%1"
		:"=q" (oldval), "=m" (lock->lock)
		:"0" (0) : "memory");
	return oldval > 0;
}

static inline void _raw_spin_lock(spinlock_t *lock)
{
	__asm__ __volatile__(
		spin_lock_string
		:"=m" (lock->lock) : : "memory");
}

/*
 * Read-write spinlocks, allowing multiple readers
 * but only one writer.
 *
 * NOTE! it is quite common to have readers in interrupts
 * but no interrupt writers. For those circumstances we
 * can "mix" irq-safe locks - any writer needs to get a
 * irq-safe write-lock, but readers can get non-irqsafe
 * read-locks.
 */
typedef struct {
	volatile unsigned int lock;
} rwlock_t;

#define RWLOCK_MAGIC_INIT	/* */

#define RW_LOCK_UNLOCKED (rwlock_t) { RW_LOCK_BIAS RWLOCK_MAGIC_INIT }

#define rwlock_init(x)	do { *(x) = RW_LOCK_UNLOCKED; } while(0)

#define rwlock_is_locked(x) ((x)->lock != RW_LOCK_BIAS)

/*
 * On x86, we implement read-write locks as a 32-bit counter
 * with the high bit (sign) being the "contended" bit.
 *
 * The inline assembly is non-obvious. Think about it.
 *
 * Changed to use the same technique as rw semaphores.  See
 * semaphore.h for details.  -ben
 */
/* the spinlock helpers are in arch/i386/kernel/semaphore.c */

static inline void _raw_read_lock(rwlock_t *rw)
{
	__build_read_lock(rw, "__read_lock_failed");
}

static inline void _raw_write_lock(rwlock_t *rw)
{
	__build_write_lock(rw, "__write_lock_failed");
}

#define _raw_read_unlock(rw)		asm volatile("lock ; incl %0" :"=m" ((rw)->lock) : : "memory")
#define _raw_write_unlock(rw)	asm volatile("lock ; addl $" RW_LOCK_BIAS_STR ",%0":"=m" ((rw)->lock) : : "memory")

static inline int _raw_write_trylock(rwlock_t *lock)
{
	atomic_t *count = (atomic_t *)lock;
	if (atomic_sub_and_test(RW_LOCK_BIAS, count))
		return 1;
	atomic_add(RW_LOCK_BIAS, count);
	return 0;
}

/*MaRTE OS*/
/*Traido de linux/preempt.h*/
#define preempt_disable()		do { } while (0)
#define preempt_enable_no_resched()	do { } while (0)
#define preempt_enable()		do { } while (0)
#define preempt_check_resched()		do { } while (0)

/*
 * Define the various spin_lock and rw_lock methods.  Note we define these
 * regardless of whether CONFIG_SMP or CONFIG_PREEMPT are set. The various
 * methods are defined as nops in the case they are not required.
 */
#define spin_trylock(lock)	({preempt_disable(); _raw_spin_trylock(lock) ? \
				1 : ({preempt_enable(); 0;});})

#define write_trylock(lock)	({preempt_disable();_raw_write_trylock(lock) ? \
				1 : ({preempt_enable(); 0;});})

#define spin_lock(lock)	\
do { \
	preempt_disable(); \
	_raw_spin_lock(lock); \
} while(0)

#define write_lock(lock) \
do { \
	preempt_disable(); \
	_raw_write_lock(lock); \
} while(0)


#define read_lock(lock)	\
do { \
	preempt_disable(); \
	_raw_read_lock(lock); \
} while(0)

#define spin_unlock(lock) \
do { \
	_raw_spin_unlock(lock); \
	preempt_enable(); \
} while (0)

#define write_unlock(lock) \
do { \
	_raw_write_unlock(lock); \
	preempt_enable(); \
} while(0)

#define read_unlock(lock) \
do { \
	_raw_read_unlock(lock); \
	preempt_enable(); \
} while(0)

#define spin_lock_irqsave(lock, flags) \
do { \
	local_irq_save(flags); \
	preempt_disable(); \
	_raw_spin_lock(lock); \
} while (0)

#define spin_lock_irq(lock) \
do { \
	local_irq_disable(); \
	preempt_disable(); \
	_raw_spin_lock(lock); \
} while (0)

#define spin_lock_bh(lock) \
do { \
	local_bh_disable(); \
	preempt_disable(); \
	_raw_spin_lock(lock); \
} while (0)

#define read_lock_irqsave(lock, flags) \
do { \
	local_irq_save(flags); \
	preempt_disable(); \
	_raw_read_lock(lock); \
} while (0)

#define read_lock_irq(lock) \
do { \
	local_irq_disable(); \
	preempt_disable(); \
	_raw_read_lock(lock); \
} while (0)

#define read_lock_bh(lock) \
do { \
	local_bh_disable(); \
	preempt_disable(); \
	_raw_read_lock(lock); \
} while (0)

#define write_lock_irqsave(lock, flags) \
do { \
	local_irq_save(flags); \
	preempt_disable(); \
	_raw_write_lock(lock); \
} while (0)

#define write_lock_irq(lock) \
do { \
	local_irq_disable(); \
	preempt_disable(); \
	_raw_write_lock(lock); \
} while (0)

#define write_lock_bh(lock) \
do { \
	local_bh_disable(); \
	preempt_disable(); \
	_raw_write_lock(lock); \
} while (0)

#define spin_unlock_irqrestore(lock, flags) \
do { \
	_raw_spin_unlock(lock); \
	local_irq_restore(flags); \
	preempt_enable(); \
} while (0)

#define _raw_spin_unlock_irqrestore(lock, flags) \
do { \
	_raw_spin_unlock(lock); \
	local_irq_restore(flags); \
} while (0)

#define spin_unlock_irq(lock) \
do { \
	_raw_spin_unlock(lock); \
	local_irq_enable(); \
	preempt_enable(); \
} while (0)

#define spin_unlock_bh(lock) \
do { \
	_raw_spin_unlock(lock); \
	preempt_enable(); \
	local_bh_enable(); \
} while (0)

#define read_unlock_irqrestore(lock, flags) \
do { \
	_raw_read_unlock(lock); \
	local_irq_restore(flags); \
	preempt_enable(); \
} while (0)

#define read_unlock_irq(lock) \
do { \
	_raw_read_unlock(lock); \
	local_irq_enable(); \
	preempt_enable(); \
} while (0)

#define read_unlock_bh(lock) \
do { \
	_raw_read_unlock(lock); \
	preempt_enable(); \
	local_bh_enable(); \
} while (0)

#define write_unlock_irqrestore(lock, flags) \
do { \
	_raw_write_unlock(lock); \
	local_irq_restore(flags); \
	preempt_enable(); \
} while (0)

#define write_unlock_irq(lock) \
do { \
	_raw_write_unlock(lock); \
	local_irq_enable(); \
	preempt_enable(); \
} while (0)

#define write_unlock_bh(lock) \
do { \
	_raw_write_unlock(lock); \
	preempt_enable(); \
	local_bh_enable(); \
} while (0)

#define spin_trylock_bh(lock)	({ local_bh_disable(); preempt_disable(); \
				_raw_spin_trylock(lock) ? 1 : \
				({preempt_enable(); local_bh_enable(); 0;});})

#endif /* __LINUX_SPINLOCK_H */
