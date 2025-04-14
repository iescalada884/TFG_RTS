CC=gcc
CFLAGS := -m32 -march=i686 $(CFLAGS) -I../usr_incl -O2 -Wall

all: syscall_stdio.o syscall_timer.o syscall_time.o syscall_signal.o syscall_stdlib.o
	mv syscall_stdio.o syscall_timer.o syscall_time.o syscall_signal.o syscall_stdlib.o ../..

syscall_stdio.o: syscall_stdio.c linux_syscalls.h
syscall_time.o: syscall_time.c linux_syscalls.h
syscall_timer.o: syscall_timer.c linux_syscalls.h
syscall_signal.o: syscall_signal.c linux_syscalls.h
syscall_stdlib.o: syscall_stdlib.c linux_syscalls.h

clean:
	rm -f *.o *.a *~
