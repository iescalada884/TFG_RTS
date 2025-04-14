#ifndef _LINUX_ERRNO_H
#define _LINUX_ERRNO_H

#include <errno.h>

#ifndef EIO
#define	EIO		 5	/* I/O error */
#endif

#ifndef EFAULT
#define	EFAULT		14	/* Bad address */
#endif

#ifndef ENODEV
#define	ENODEV		19	/* No such device */
#endif

#ifndef ENOBUFS
#define	ENOBUFS		105	/* No buffer space available */
#endif

#ifndef ENOIOCTLCMD
#define ENOIOCTLCMD	515	/* No ioctl command */
#endif

#ifndef EREMOTEIO
#define EREMOTEIO	121	/* Remote I/O error */
#endif

#ifndef ENFILE
#define ENFILE        3100
#endif

#endif /*_LINUX_ERRNO_H*/
