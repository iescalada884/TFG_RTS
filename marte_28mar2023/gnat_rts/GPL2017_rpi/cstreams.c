#include <stdlib.h>

#define FILE void

int    __gnat_constant_eof      = -1;
int    __gnat_constant_iofbf    = -1;
int    __gnat_constant_iolbf    = -1;
int    __gnat_constant_ionbf    = -1;
int    __gnat_constant_seek_end = -1;

int
__gnat_feof (FILE *stream)
{
  return -1;
}

int
__gnat_ferror (void *stream)
{
   return -1;
}

char *
__gnat_full_name (char *nam, char *buffer)
{
  return (char *)0;
}

int
__gnat_fileno (FILE *stream)
{
   return -1;
}

FILE *
__gnat_constant_stderr (void)
{
  return NULL;
}

FILE *
__gnat_constant_stdin (void)
{
  return NULL;
}

FILE *
__gnat_constant_stdout (void)
{
  return NULL;
}

int
__gnat_is_fifo (const char* path)
{
  return 0;
}
