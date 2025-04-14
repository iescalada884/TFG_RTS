
# CFLAGS and GNATFLAGS are environment variables set by mkkernel

DEFAULT: all

compile_all:
	../../../utils/mgcc -c  *.c $(CFLAGS)
	../../../utils/mgnatmake -c $(CFLAGS) $(GNATFLAGS) -Imarte_src_dirs simple_dynamic_memory.adb

all: compile_all
	mv *.o ../
