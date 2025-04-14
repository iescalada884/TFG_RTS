#ifndef __COMBTTV_H
#define __COMBTTV_H
#include <debug_marte.h>
#include <malloc.h>

#define __KERNEL__

#define GFP_ATOMIC 1
#define GFP_KERNEL 2

extern void vfree(void *addr);
extern void *vmalloc(unsigned long size);
extern void * vmalloc_32(size_t size);
extern void *kmalloc(size_t size, int flags);
extern void kfree(void *addr);

#define MAX_NUMBER_BUFFERS 6

#define KERNEL_VERSION(a,b,c) (((a) << 16) + ((b) << 8) + (c))

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#define __u64 unsigned long long
#define __u32 unsigned long
#define __u16 unsigned short
#define __u8 unsigned char

#define __s64 signed long long
#define __s32 signed long
#define __s16 signed short
#define __s8 signed char


#define u64 unsigned long long
#define u32 unsigned long
#define u16 unsigned short
#define u8 unsigned char

#define s64 signed long long
#define s32 signed long
#define s16 signed short
#define s8 signed char

#ifdef CONFIG_HIGHMEM64G
typedef u64 dma_addr_t;
#else
typedef u32 dma_addr_t;
#endif
typedef u64 dma64_addr_t;
#define loff_t unsigned long long

/*We use LITTLE ENDIAN computers*/
#define cpu_to_le32(x) (u32)x


#define VM_WRITE	0x00000002
#define VM_SHARED	0x00000008
#define VM_IO           0x00004000	/* Memory mapped I/O or similar */
#define VM_DONTEXPAND	0x00040000	/* Cannot expand with mremap() */

struct vm_operations_struct;

struct vm_area_struct {
  void * vm_private_data;
  unsigned long vm_start;		/* Our start address within vm_mm. */
  unsigned long vm_end;		/* The first byte after our end address
				   within vm_mm. */

  int num;
  //  struct file * vm_file;		/* File we map to (can be NULL). */
  
  /* Function pointers to deal with this struct. */
  struct vm_operations_struct * vm_ops;

};

/*
 * These are the virtual MM functions - opening of an area, closing and
 * unmapping it (needed to keep files on disk up-to-date etc), pointer
 * to the functions called when a no-page or a wp-page exception occurs. 
 */
struct vm_operations_struct {
	void (*open)(struct vm_area_struct * area);
	void (*close)(struct vm_area_struct * area);
	struct page * (*nopage)(struct vm_area_struct * area, unsigned long address, int unused);
};

#define in_interrupt() 0
#define BUG()

#include <string.h>
#define copy_from_user(to,from,n) memcpy(to,from,n)
#define copy_to_user(to,from,n) memcpy(to,from,n)

//#include <linux/completion.h>

#define SA_SHIRQ               1
#define SA_INTERRUPT           1


/*MaRTE OS*/
#ifndef iminor
#define iminor(a) 0
#endif

extern int i2c_debug;

/* ----- global defines ----------------------------------------------- */
#define DEB(x) if (i2c_debug>=1) x;
#define DEB2(x) if (i2c_debug>=2) x;
#define DEBSTAT(x) if (i2c_debug>=3) x; /* print several statistical values*/
#define DEBPROTO(x) if (i2c_debug>=9) { x; }
 	/* debug the protocol by showing transferred bits */

#define dev_dbg3(a,b,c) printc(b,c)
#define dev_dbg2(a,b) printc(b)

#define dev_err4(a,b,c,d) printc(b,c.d)
#define dev_err3(a,b,c) printc(b,c)
#define dev_err2(a,b) printc(b)

#define dev_warn3(a,b,c) printc(b,c)

#define BUG_ON(condition) do { if (!!((condition)!=0)) return 0; } while(0)



#include <stdio.h>

#define O_NONBLOCK	  04000






/*Port from include/linux/stat.h*/
#define S_IFMT  00170000
#define S_IFSOCK 0140000
#define S_IFLNK	 0120000
#define S_IFREG  0100000
#define S_IFBLK  0060000
#define S_IFDIR  0040000
#define S_IFCHR  0020000
#define S_IFIFO  0010000
#define S_ISUID  0004000
#define S_ISGID  0002000
#define S_ISVTX  0001000

#define S_ISLNK(m)	(((m) & S_IFMT) == S_IFLNK)
#define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#define S_ISCHR(m)	(((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)	(((m) & S_IFMT) == S_IFBLK)
#define S_ISFIFO(m)	(((m) & S_IFMT) == S_IFIFO)
#define S_ISSOCK(m)	(((m) & S_IFMT) == S_IFSOCK)

#define S_IRWXU 00700
#define S_IRUSR 00400
#define S_IWUSR 00200
#define S_IXUSR 00100

#define S_IRWXG 00070
#define S_IRGRP 00040
#define S_IWGRP 00020
#define S_IXGRP 00010

#define S_IRWXO 00007
#define S_IROTH 00004
#define S_IWOTH 00002
#define S_IXOTH 00001

#define S_IRWXUGO	(S_IRWXU|S_IRWXG|S_IRWXO)
#define S_IALLUGO	(S_ISUID|S_ISGID|S_ISVTX|S_IRWXUGO)
#define S_IRUGO		(S_IRUSR|S_IRGRP|S_IROTH)
#define S_IWUGO		(S_IWUSR|S_IWGRP|S_IWOTH)
#define S_IXUGO		(S_IXUSR|S_IXGRP|S_IXOTH)


#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))


#define	KERN_EMERG		/* system is unusable			*/
#define	KERN_ALERT		/* action must be taken immediately	*/
#define	KERN_CRIT		/* critical conditions			*/
//#define	KERN_ERR		/* error conditions			*/
#define	KERN_WARNING		/* warning conditions			*/
#define	KERN_NOTICE		/* normal but significant condition	*/
#define	KERN_INFO		/* informational			*/
#define	KERN_DEBUG		/* debug-level messages			*/

#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)

/**
 * container_of - cast a member of a structure out to the containing structure
 *
 * @ptr:	the pointer to the member.
 * @type:	the type of the container struct this is embedded in.
 * @member:	the name of the member within the struct.
 *
 */
#define container_of(ptr, type, member) ({			\
        const typeof( ((type *)0)->member ) *__mptr = (ptr);	\
        (type *)( (char *)__mptr - offsetof(type,member) );})

#define printk printc
#define printke printe
#define sprintf26 sprintf
#define snprintf26 snprintf





#endif
