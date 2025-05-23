/*
 *  Generic cache management functions. Everything is arch-specific,  
 *  but this header exists to make sure the defines/functions can be
 *  used in a generic way.
 *
 *  2000-11-13  Arjan van de Ven   <arjan@fenrus.demon.nl>
 *
 */

#ifndef _LINUX_PREFETCH_H
#define _LINUX_PREFETCH_H

#include <linux/compiler.h>
/* #include <asm/processor.h> */
/* #include <asm/cache.h> */

/*
	prefetch(x) attempts to pre-emptively get the memory pointed to
	by address "x" into the CPU L1 cache. 
	prefetch(x) should not cause any kind of exception, prefetch(0) is
	specifically ok.

	prefetch() should be defined by the architecture, if not, the 
	#define below provides a no-op define.	
	
	There are 3 prefetch() macros:
	
	prefetch(x)  	- prefetches the cacheline at "x" for read
	prefetchw(x)	- prefetches the cacheline at "x" for write
	spin_lock_prefetch(x) - prefectches the spinlock *x for taking
	
	there is also PREFETCH_STRIDE which is the architecure-prefered 
	"lookahead" size for prefetching streamed operations.
	
*/

/*
 *	These cannot be do{}while(0) macros. See the mental gymnastics in
 *	the loop macro.
 */
 
#ifndef ARCH_HAS_PREFETCH
static inline void prefetch(const void *x) {}
#endif

#ifndef ARCH_HAS_PREFETCHW
static inline void prefetchw(const void *x) {}
#endif

#ifndef ARCH_HAS_SPINLOCK_PREFETCH
#define spin_lock_prefetch(x) prefetchw(x)
#endif

#ifndef PREFETCH_STRIDE
#define PREFETCH_STRIDE (4*L1_CACHE_BYTES)
#endif

#endif
