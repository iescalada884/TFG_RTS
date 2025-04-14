
CFLAGS2=

all: wrapper_main_c.o  wrapper_main_cpp.o   wrapper_main_ada.o  _exit.o

wrapper_main_c.o: wrapper_main_c.c Makefile
	../../utils/mgcc -c $(CFLAGS) $(CFLAGS2) -dont_change_main_name wrapper_main_c.c

wrapper_main_cpp.o: wrapper_main_c.c Makefile
	../../utils/mgcc -c $(CFLAGS) $(CFLAGS2) -dont_change_main_name -DMARTE_CPP wrapper_main_c.c -o wrapper_main_cpp.o

wrapper_main_ada.o: wrapper_main_ada.c Makefile
	../../utils/mgcc -c $(CFLAGS) $(CFLAGS2) -dont_change_main_name wrapper_main_ada.c

_exit.o: _exit.c  Makefile
	gcc -c $(CFLAGS) $(CFLAGS2) -m32 -march=i686 _exit.c -I$(MPATH)/arch/hwi/boot/linux_syscalls/lib -I$(MPATH)/arch/hwi/boot/linux_syscalls/usr_incl
