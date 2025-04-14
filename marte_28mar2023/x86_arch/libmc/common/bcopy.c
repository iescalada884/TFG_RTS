#include "mem.h"
void bcopy(const void *s2, void *s1, unsigned int n){
	memmove(s1, s2, n);
}
