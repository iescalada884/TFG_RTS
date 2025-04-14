
#include <stdio.h>
#include <errno.h>
#include <string.h>

void perror(const char *s)
{
  if (s != NULL && s[0] != 0)     
    printe("%s: ", s); 
    
  printe("%s\n", strerror(errno));
}
