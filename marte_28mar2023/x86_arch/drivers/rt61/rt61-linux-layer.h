// --------------------------------------------
// Module: rt61-linux-layer
// Abstract: linux compatibility layer. Some
// fake functions so the driver can compile.
// --------------------------------------------
#ifndef _MARTE_RT61_LINUXLAYER_H_
#define _MARTE_RT61_LINUXLAYER_H_
// ---------------------------------------------
//  1) KERNEL BASIC INTEGER TYPES and CONSTANTS
// ---------------------------------------------
#define GFP_KERNEL 2
#define __iomem
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef u32 dma_addr_t;
typedef u16 __le16;
typedef u32 __le32;
// Interrupt constants
#define SA_SHIRQ        0x04000000
#define IRQF_SHARED     SA_SHIRQ
// --------------------------------------------
//  2) LITTLE/BIG ENDIAN CONVERSIONS
// --------------------------------------------
// We will use the card only on a x86 platform so everything is little endian
// -----------------------------------------------------------------------
#define cpu_to_le32(x) (u32)(x)
#define cpu_to_le16(x) (u16)(x)
#define le32_to_cpu(x) (x)
#define le16_to_cpu(x) (x)
// --------------------------------------------
//  3) PRINTING FUNCTIONS (map all to printc)
// --------------------------------------------
#include <stdio.h>
#define printk printc
#define	KERN_EMERG
#define	KERN_ALERT
#define	KERN_CRIT
#define	KERN_ERR
#define	KERN_WARNING
#define	KERN_NOTICE
#define	KERN_INFO
#define	KERN_DEBUG
// --------------------------------------------
//  4) TIMING FUNCTIONS (map all to nanosleep)
// --------------------------------------------
#include <time.h>
#define msleep(u) { struct timespec ts = {0, u*1000000}; \
                    nanosleep (&ts, NULL);  \
}
#define udelay(u) { struct timespec ts = {0, u*1000}; \
                    nanosleep (&ts, NULL);  \
}
// --------------------------------------------
//  5) MEMORY FUNCTIONS
// --------------------------------------------
// MaRTE OS memory model is very simple, there is no division
// between the kernel and the application and paging is not used.
// -----------------------------------------------------------------------
#include <malloc.h>
#include <string.h>
#define kzalloc(size,flags) malloc(size)
#define kmalloc(size,flags) malloc(size)
#define copy_from_user(to,from,n) memcpy(to,from,n)
#define copy_to_user(to,from,n) memcpy(to,from,n)
#define memcpy_fromio(to,from,n) memcpy(to,from,n)
#define memcpy_toio(to,from,n) memcpy(to,from,n)
#define unlikely(x) (x)  // No cache optimizations
#define likely(x) (x)    // No cache optimizations
// -----------------------------------------------------------------------
static inline void * vmalloc_32(size_t size)
{
   void *mem;
   unsigned long diff;

   mem = malloc(size+12);
   diff = (unsigned long)((((unsigned long)mem/4)+1)*4-(unsigned long)mem);
   *(unsigned long *)(mem+diff) = (diff | 0x80000000);

   return (mem+diff+4);
}
// -----------------------------------------------------------------------
static inline void *dma_alloc_coherent(struct pci_device *dev, size_t size,
                                       dma_addr_t *dma_handle)
{
   void *ret;
   ret = (void *)vmalloc_32(size);
   if (ret != NULL) {
      memset(ret, 0, size);
      *dma_handle = (dma_addr_t)ret;
   }
   return ret;
}
// -----------------------------------------------------------------------
static inline void *pci_alloc_consistent(struct pci_device *hwdev, size_t size,
                                         dma_addr_t *dma_handle)
{
   return dma_alloc_coherent(hwdev == NULL ? NULL : hwdev, size, dma_handle);
}
// -----------------------------------------------------------------------
static inline void kfree(void * x)
{
   free(x);
}
// --------------------------------------------
//  6) SOCKET BUFFERS
// --------------------------------------------
// Some little functions to emulate the behavior of the socket buffer
// structure used by network drivers to store frames of data
// -----------------------------------------------------------------------
struct sk_buff {
   unsigned char *data;
   unsigned int len;
};
// -----------------------------------------------------------------------
static inline struct sk_buff *dev_alloc_skb (unsigned int len)
{
   struct sk_buff *skb = (struct sk_buff *)malloc (sizeof (struct sk_buff));
   skb->data = (unsigned char *)malloc (len);
   skb->len = len;
   return skb;
}
// -----------------------------------------------------------------------
static inline unsigned char *skb_put (struct sk_buff *skb, int len)
{
   return skb->data;
}
// --------------------------------------------
//  7) ERROR CODES
// --------------------------------------------
#define EPERM            1      /* Operation not permitted */
#define ENOENT           2      /* No such file or directory */
#define ESRCH            3      /* No such process */
#define EINTR            4      /* Interrupted system call */
#define EIO              5      /* I/O error */
#define ENXIO            6      /* No such device or address */
#define E2BIG            7      /* Argument list too long */
#define ENOEXEC          8      /* Exec format error */
#define EBADF            9      /* Bad file number */
#define ECHILD          10      /* No child processes */
#define EAGAIN          11      /* Try again */
#define ENOMEM          12      /* Out of memory */
#define EACCES          13      /* Permission denied */
#define EFAULT          14      /* Bad address */
#define ENOTBLK         15      /* Block device required */
#define EBUSY           16      /* Device or resource busy */
#define EEXIST          17      /* File exists */
#define EXDEV           18      /* Cross-device link */
#define ENODEV          19      /* No such device */
#define ENOTDIR         20      /* Not a directory */
#define EISDIR          21      /* Is a directory */
#define EINVAL          22      /* Invalid argument */
#define ENFILE          23      /* File table overflow */
#define EMFILE          24      /* Too many open files */
#define ENOTTY          25      /* Not a typewriter */
#define ETXTBSY         26      /* Text file busy */
#define EFBIG           27      /* File too large */
#define ENOSPC          28      /* No space left on device */
#define ESPIPE          29      /* Illegal seek */
#define EROFS           30      /* Read-only file system */
#define EMLINK          31      /* Too many links */
#define EPIPE           32      /* Broken pipe */
#define EDOM            33      /* Math argument out of domain of func */
#define ERANGE          34      /* Math result not representable */
// --------------------------------------------
//  8) ETHERNET ADDRESS VALIDATION
// --------------------------------------------
static inline int is_zero_ether_addr(const u8 *addr)
{
   return !(addr[0] | addr[1] | addr[2] | addr[3] | addr[4] | addr[5]);
}
// -----------------------------------------------------------------------
static inline int is_multicast_ether_addr(const u8 *addr)
{
   return (0x01 & addr[0]);
}
// -----------------------------------------------------------------------
static inline int is_valid_ether_addr(const u8 *addr)
{
   // FF:FF:FF:FF:FF:FF is a multicast address so we don't need to
   // explicitly check for it here.
   return !is_multicast_ether_addr(addr) && !is_zero_ether_addr(addr);
}
#endif  // _MARTE_RT61_LINUXLAYER_H_
