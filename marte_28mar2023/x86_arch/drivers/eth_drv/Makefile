CC = ../../../utils/mgcc
CFLAGS = -Wall -g -DCONFIG_PCI # -DCONFIG_ISA
SRCS := $(wildcard *.c)
OBJS := $(patsubst %.c,%.o,$(wildcard *.c))

default: $(OBJS)

%.o: %.c $(SRCS)
	$(CC) -c $(CFLAGS) $<

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali'  \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"
