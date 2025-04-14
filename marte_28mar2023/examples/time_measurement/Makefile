CC = ../../utils/mgcc
CFLAGS = -Wall -g -I../../arch/drivers/svga ../../misc/logger.o ../../misc/time_measurement_posix.o ../../misc/time_measurement_hwtime.o -lsvga -lm
GNAT = ../../utils/mgnatmake
ADA_FLAGS = -gnata -gnato -g -Imarte_src_dirs
CXX = ../../utils/mg++

%.exe: time_measurement_posix.o time_measurement_hwtime.o logger.o
	@exec echo -en "\n>> Compiling $@: ";
	@if [ -f $*.c ]; then $(CC) $(CFLAGS) $*.c -o $@ 1> /dev/null; fi
	@if [ -f $*.adb ]; then $(GNAT) $(ADA_FLAGS) $*.adb -o $@ 1> /dev/null; fi
	@if [ -f $*.cc ]; then $(CXX) $(CFLAGS) $*.cc -o $@ 1> /dev/null; fi
	@exec echo "  [OK]";

time_measurement_posix.o:
	make -C ../../misc time_measurement_posix.o

time_measurement_hwtime.o:
	make -C ../../misc time_measurement_hwtime.o

logger.o:
	make -C ../../misc logger.o

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"