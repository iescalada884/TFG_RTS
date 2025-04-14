.PHONY: all clean

CC = ../../../utils/mgcc
CFLAGS = -g -Wall
LDFLAGS =

SRCS := $(wildcard *.c)
OBJS := $(patsubst %.c,%.o,$(wildcard *.c))
HDRS := $(wildcard *.h)

all: $(OBJS)

%.o: %.c $(SRCS) $(HDRS)
	$(CC) $(CFLAGS) -c $<

%.exe: %.c all
	@exec echo -e "\n>> Building $@: ";
	@if [ -f $< ]; \
	then \
		$(CC) $(CFLAGS) $< $(LDFLAGS) -o $@; \
	fi;
	@exec echo ">> End Building $@";

clean:
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
	-or -name '*.exe' \) -print -delete
