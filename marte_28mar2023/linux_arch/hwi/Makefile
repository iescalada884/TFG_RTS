.PHONY: boot

CFLAGS2=
MGCC = ../../utils/mgcc

all: marte-hal-processor_registers_c.o boot

#rtc_c.o: rtc.c ../../include/stdio.h ../../include/time.h \
#         ../../include/sys/pio.h Makefile
#	mgcc -c -o rtc_c.o $(CFLAGS) $(CFLAGS2) rtc.c

marte-hal-processor_registers_c.o: marte-hal-processor_registers_c.c Makefile
	$(MGCC) -c $(CFLAGS) $(CFLAGS2) marte-hal-processor_registers_c.c

boot:
	make -C boot/
