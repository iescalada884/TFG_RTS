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

test_novatel_ada.exe: $(SRCS)
	$(GNAT) $(ADA_FLAGS) test_novatel_ada.adb -o $@ -largs -L../../../lib -lm -lmarte -lm 1> /dev/null

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"


