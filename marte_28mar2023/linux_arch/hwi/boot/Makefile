MAKEFLAGS= -j1

.PHONY: dummy

CC=gcc
CFLAGS:= -m32 -march=i686 $(CFLAGS) -D$(VIRTUAL_TIMER) -fno-stack-protector

OBJS= base_trap_default_handler.o base_irq_default_handler.o hw_timer.o interrupts.o linux_signals.o fake_linux_sigset.o

all: $(OBJS)
	make -C linux_syscalls/lib
	mv *.o $(MPATH)/lib/

clean:
	make -C linux_syscalls/lib clean
	rm -rf *~ *.o *.a

timers.o: *.c *.h linux_syscalls/lib/linux_syscalls.h

interrupts.o: *.c *.h linux_syscalls/lib/linux_syscalls.h

base_trap_default_handler.o: *.c *.h linux_syscalls/lib/linux_syscalls.h

base_irq_default_handler.o: *.c *.h linux_syscalls/lib/linux_syscalls.h

hw_timer.o: *.c *.h linux_syscalls/lib/linux_syscalls.h

linux_signals.o: *.c *.h linux_syscalls/lib/linux_syscalls.h

fake_linux_sigset.o: *.c *.h linux_syscalls/lib/linux_syscalls.h
