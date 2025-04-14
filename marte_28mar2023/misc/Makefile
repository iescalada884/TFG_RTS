.PHONY: clean all

CC = ../utils/mgcc
MGNATMAKE = ../utils/mgnatmake
MGNATBIND = ../utils/mgnatbind
CFLAGS = -Wall -g -I../x86_arch/include/misc
SRCS := $(sort $(wildcard *.c))
OBJS_X86 := $(patsubst %.c,%.o,$(wildcard *.c))

OBJS_XTRATUM:=circular_memory_buffer.o  load_loop.o  time_measurement_hwtime.o freelist.o time_measurement_posix.o

# Only objs to be compiled in Linux and Linux_lib architectures
OBJS_LINUX := console_management.o load_loop.o

OBJS_RPI := circular_memory_buffer.o  load_loop.o  freelist.o time_measurement_posix.o

drivers_dir := $(wildcard ../arch/drivers/*)
GNATFLAGS  = $(foreach dir,$(drivers_dir),-aI$(dir))
GNATFLAGS += -Imarte_src_dirs -aO../lib/

default all:
	$(error Use make {x86,linux,linux_lib})

x86: $(OBJS_X86) console_management.o

xtratum: $(OBJS_XTRATUM)

linux linux_lib: $(OBJS_LINUX)
	gcc -c console_management_c.c

rpi: $(OBJS_RPI)

%.o: %.c ../include/misc/%.h
	$(CC) -c $(CFLAGS) $<

console_management.o: console_management.adb
	$(MGNATMAKE) -c $(GNATFLAGS) $< -o $@
	$(MGNATBIND) -Lconsole_management_ -F -n console_management.ali $(GNATFLAGS)
	$(MGNATMAKE) -c $(GNATFLAGS) b~console_management.adb

clean:
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
	-or -name '*.exe' \) -print -delete
