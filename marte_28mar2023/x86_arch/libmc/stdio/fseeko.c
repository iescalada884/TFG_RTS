// MaRTE OS
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>

int fseeko(FILE *stream, off_t offset, int whence) {
  return fseek(stream, (long) offset, whence);
}
