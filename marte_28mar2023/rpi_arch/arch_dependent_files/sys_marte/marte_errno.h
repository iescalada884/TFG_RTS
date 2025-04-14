/*
 * MaRTE OS
 * Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
 *
 * This file has been generated automatically by 'mkmarte'
 * using constants defined in the Ada part of the kernel.
 *
 * Do Not Edit.
 */

#ifndef _MARTE_SYS_MARTE_ERRNO_H_
#define _MARTE_SYS_MARTE_ERRNO_H_

#define EFAULT                 14 /* Bad Address                         */
#define EAGAIN                 11 /* Rsrc temporarily unavailable        */
#define EBUSY                  16 /* Device busy                         */
#define EDEADLK                35 /* Resource deadlock avoided           */
#define EINTR                   4 /* Interrupted system call             */
#define EINVAL                 22 /* Invalid argument                    */
#define ESRCH                   3 /* No such process                     */
#define EPERM                   1 /* Operation not permitted             */
#define ENOTSUP                95 /* Not supported                       */
#define ENOSYS                 38 /* Operation not implemented           */
#define ETIMEDOUT             110 /* Operation timed out                 */
#define ENOMEM                 12 /* Not enough space                    */
#define ERANGE                 34 /* Result too large                    */
#define EMASKED               200 /* Appsched event masked               */
#define EREJECT               201 /* Appsched has rejected object        */
#define EPOLICY               202 /* Not an appscheduler or bad policy   */
#define EMFILE                 24 /* Too many files open in the system   */
#define ENAMETOOLONG           36 /* Lenght of path exceeds              */
#define EBADF                   9 /* Not a valid open file descriptor    */
#define ENOENT                  2 /* Specified pathname does not exist   */
#define EACCES                 13 /* Wrong attempt to access a file      */
#define ENOISR                300 /* No ISR associated with thread       */

#endif /* _MARTE_SYS_MARTE_ERRNO_H_ */
