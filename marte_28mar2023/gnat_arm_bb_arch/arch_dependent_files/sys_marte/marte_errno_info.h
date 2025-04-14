/*
 * MaRTE OS
 * Copyright (C) 2000-2008, Universidad de Cantabria, SPAIN
 *
 * This file has been generated automatically by 'mkmarte'
 * using constants defined in the Ada part of the kernel.
 *
 * Do Not Edit.
 */

#ifndef _MARTE_SYS_MARTE_ERRNO_INFO_H_
#define _MARTE_SYS_MARTE_ERRNO_INFO_H_

#include <sys/marte_errno.h>

static struct errdef {
    int     num;
    char    *str;
} errlist[] = {
  { EFAULT        , "Bad Address                        " },
  { EAGAIN        , "Rsrc temporarily unavailable       " },
  { EBUSY         , "Device busy                        " },
  { EDEADLK       , "Resource deadlock avoided          " },
  { EINTR         , "Interrupted system call            " },
  { EINVAL        , "Invalid argument                   " },
  { ESRCH         , "No such process                    " },
  { EPERM         , "Operation not permitted            " },
  { ENOTSUP       , "Not supported                      " },
  { ENOSYS        , "Operation not implemented          " },
  { ETIMEDOUT     , "Operation timed out                " },
  { ENOMEM        , "Not enough space                   " },
  { ERANGE        , "Result too large                   " },
  { EMASKED       , "Appsched event masked              " },
  { EREJECT       , "Appsched has rejected object       " },
  { EPOLICY       , "Not an appscheduler or bad policy  " },
  { EMFILE        , "Too many files open in the system  " },
  { ENAMETOOLONG  , "Lenght of path exceeds             " },
  { EBADF         , "Not a valid open file descriptor   " },
  { ENOENT        , "Specified pathname does not exist  " },
  { EACCES        , "Wrong attempt to access a file     " },
  { ENOISR        , "No ISR associated with thread      " },
};

#endif /* _MARTE_SYS_MARTE_ERRNO_INFO_H_ */
