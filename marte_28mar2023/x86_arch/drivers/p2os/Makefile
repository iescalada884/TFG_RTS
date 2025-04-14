.PHONY: all clean

CC = ../../../utils/mgcc
CXX = ../../../utils/mg++
GNAT = ../../../utils/mgnatmake
CFLAGS = -g -Wall
LDFLAGS =
ADA_FLAGS = -gnata -gnato -g

SRCS := $(wildcard *.c -or *.adb -or *.ads)
OBJS := $(patsubst %.c,%.o,$(wildcard *.c))
HDRS := $(wildcard *.h)

all: $(OBJS)

%.o: %.c $(SRCS) $(HDRS)
	$(CC) $(CFLAGS) -c $<

%.exe: $(SRCS)
	@exec echo -en "\n>> Compiling $@: ";
	@if [ -f $*.c ]; then $(CC) $(CFLAGS) $*.c -o $@ 1> /dev/null; fi
	@if [ -f $*.adb ]; then $(GNAT) $(ADA_FLAGS) $*.adb -o $@ 1> /dev/null; fi
	@if [ -f $*.cc ]; then $(CXX) $(CFLAGS) $*.cc -o $@ 1> /dev/null; fi
	@exec echo "  [OK]";

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"
