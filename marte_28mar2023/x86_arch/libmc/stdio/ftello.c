// MaRTE OS
#include <stdio.h>

off_t ftello(FILE *stream) {
  // XXX ftell is not implemented yet
  return (off_t) ftell(stream);
}
