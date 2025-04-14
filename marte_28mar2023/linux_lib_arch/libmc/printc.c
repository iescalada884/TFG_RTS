#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>

static volatile int finished_marte_initialization = 0;

/*
 * printc_end_of_kernel_initialization
 */
void printc_end_of_kernel_initialization ()
{
  finished_marte_initialization = 1;
}

/*
 * printc_write
 */
// used in 'kernel_console.ads' as 'Stdout_Direct_Write'
ssize_t printc_write(int fd, const void *buf, size_t count)
{
  if (finished_marte_initialization)
    return write(fd, buf, count);
  else
    return count;
}


/*
 * printc
 */
int printc (const char *fmt, ...)
{
  va_list	args;
  int err = 0;
  va_start(args, fmt);
  if (finished_marte_initialization) {
    err = vprintf(fmt, args);
  }
  va_end(args);
  return err;
}


/*
 * printe
 */
int printe (const char *fmt, ...)
{
  va_list	args;
  int err;
  va_start(args, fmt);
  err = vprintf(fmt, args);
  va_end(args);

  return err;
}
