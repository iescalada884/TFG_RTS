CFLAGS2=

OBJS_C= rtc_c.o processor_registers_c.o

MGCC=../../utils/mgcc

all: $(OBJS_C)

rtc_c.o: rtc.c ../include/stdio.h ../include/time.h ../include/sys/pio.h \
 Makefile
	$(MGCC) -c -o rtc_c.o $(CFLAGS) $(CFLAGS2) rtc.c

processor_registers_c.o: processor_registers_c.c ../include/stdio.h Makefile
	$(MGCC) -c -o processor_registers_c.o $(CFLAGS) $(CFLAGS2) processor_registers_c.c

