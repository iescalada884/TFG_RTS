#include <stdio.h>

/*
 * fflush
 *
 * MaRTE OS drivers doesn't provide the fflush operation, so we just return 
 * 0 (success).
 */
int fflush(FILE *stream) {
  return 0;
}
