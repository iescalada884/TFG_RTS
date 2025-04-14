// MaRTE OS
#include <errno.h>
// In <errno.h> errno is defined as:
// #define errno  (*pthread_errno())               /* per-thread error number */
// extern int *pthread_errno();

int
__get_errno(void)
{
  return errno;
}

// __set_errno llready defined in marte-posix_pthread.ads
//  procedure Set_Pthread_Errno (Errno : in Int);
//   pragma Export (C, Set_Pthread_Errno, External_Name => "__set_errno");
/*
void
__set_errno(int err)
{
  errno = err;
}
*/
