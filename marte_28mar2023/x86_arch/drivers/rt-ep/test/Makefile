.PHONY: clean tests_compilation

CC=../../../../utils/mgcc
GNATMAKE=../../../../utils/mgnatmake
ADA_INCLUDES = -Imarte_src_dirs
ADA_FLAGS = -fno-strict-aliasing -gnatn -gnatp -O2 # -gnata -gnato -g # -gnatn -gnatp -O3 # -fno-strict-aliasing -gnatn -gnatp -O2
C_INCLUDES = -I../
CFLAGS = -O2 # -g -Wall
SRC := $(wildcard *.c -or *.adb)
EXEC := $(patsubst %.c, %.exe, $(SRC))
EXEC := $(patsubst %.adb, %.exe, $(EXEC))
LDFLAGS =

%.exe: $(SRC) time_measurement_hwtime.o logger.o
	@exec echo -en "\n>> Compiling $@: ";
	@if [ -f $*.c ]; then $(CC) $(CFLAGS) $*.c $(C_INCLUDES) $(LDFLAGS) -o $@ 1> /dev/null; fi
	@if [ -f $*.adb ]; then $(GNATMAKE) $(ADA_FLAGS) $*.adb $(ADA_INCLUDES) -o $@ 1> /dev/null; fi
	@exec echo "  [OK]";

time_measurement_linux_reader.exe:
	gcc $(CFLAGS) time_measurement_linux_reader.c -o time_measurement_linux_reader.exe

time_measurement_hwtime.o:
	make -C ../../../../misc time_measurement_hwtime.o

logger.o:
	make -C ../../../../misc logger.o

tests_compilation: $(EXEC)

clean:
	@exec echo -e "\n>> Cleaning up RTEP TESTS directory:";
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
 	-or -name '*.exe' \) -print -delete
	@rm -fv master.img
	@rm -fv slave.img
	@rm -fv other_slave.img
	@exec echo "  [OK]"
