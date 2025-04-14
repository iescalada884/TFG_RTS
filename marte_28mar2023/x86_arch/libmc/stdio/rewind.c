/*
 * MaRTe OS.
 *
 *  22-6-07: SANGORRIN: rewind implemented
 *
 */

#include <stdio.h>
#include <unistd.h>

void rewind(FILE *stream)
{
   (void) fseek(stream, 0L, SEEK_SET);
}

