
CFLAGS2=
XTRATUM_FLAGS = -Dx86 -I/home/xmuser/xm_v3.7.1-bin/xm/include --include xm_inc/config.h --include xm_inc/arch/arch_types.h -m32 -fomit-frame-pointer -I/home/xmuser/xm_v3.7.1-bin/xal/include -fno-builtin

OBJS_C= wrapper_main_c.o \
	wrapper_main_ada.o \
	wrapper_main_cpp.o \
	_exit.o

all: $(OBJS_C)

wrapper_main_c.o: wrapper_main_c.c Makefile
	../../utils/mgcc -o wrapper_main_c.o \
		-c $(CFLAGS) $(CFLAGS2) $(XTRATUM_FLAGS) wrapper_main_c.c

wrapper_main_ada.o: wrapper_main_ada.c Makefile
	../../utils/mgcc -o wrapper_main_ada.o \
		-c $(CFLAGS) $(CFLAGS2) $(XTRATUM_FLAGS) wrapper_main_ada.c

wrapper_main_cpp.o: wrapper_main_c.o
	cp wrapper_main_c.o wrapper_main_cpp.o

_exit.o: _exit.c
	../../utils/mgcc -c $(XTRATUM_FLAGS) _exit.c
