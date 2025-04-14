#include "linux_syscalls.h"
#include <time.h>

_syscall1(time_t, time, time_t *, t);
