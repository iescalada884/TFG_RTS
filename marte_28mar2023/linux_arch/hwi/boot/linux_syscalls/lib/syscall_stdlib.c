#include "linux_syscalls.h"

_syscall0(int, getuid);
proc_syscall1(void, exit, int, status);
