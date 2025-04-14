/*
 * MaRTe OS.
 *
 * 22-6-07: SANGORRIN: fseek implemented
 * 10-7-08: CAMINANTE: return -1 or 0 (not the offset)
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>

int fseek(FILE *stream, long offset, int whence)
{
        off_t ret;
        ret = lseek(fileno(stream), (off_t) offset, whence);
        if (ret == (off_t)-1) {
                // NOTE: i dont set errno because lseek already does it
                return -1;
        } else {
                return 0;
        }
}
