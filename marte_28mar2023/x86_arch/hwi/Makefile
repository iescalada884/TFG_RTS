.PHONY: boot

CFLAGS2=

OBJS_C= rtc_c.o interrupt_tables_c.o processor_registers_c.o time_stamp_counter_c.o local_apic_c.o clock_modulation.o

MGCC=../../utils/mgcc

all: $(OBJS_C) boot

no_libboot: $(OBJS_C) boot_without_libboot

rtc_c.o: rtc.c ../include/stdio.h ../include/time.h ../include/sys/pio.h \
 Makefile
	$(MGCC) -c -o rtc_c.o $(CFLAGS) $(CFLAGS2) rtc.c

interrupt_tables_c.o: interrupt_tables.c ../include/stdio.h Makefile
	$(MGCC) -c -o interrupt_tables_c.o $(CFLAGS) $(CFLAGS2) interrupt_tables.c

processor_registers_c.o: processor_registers_c.c ../include/stdio.h Makefile
	$(MGCC) -c -o processor_registers_c.o $(CFLAGS) $(CFLAGS2) processor_registers_c.c

time_stamp_counter_c.o: time_stamp_counter_c.c ../include/sys/pio.h ../include/stdio.h Makefile
	$(MGCC) -c -o time_stamp_counter_c.o $(CFLAGS) $(CFLAGS2) time_stamp_counter_c.c

local_apic_c.o: local_apic_c.c ../include/sys/pio.h ../include/stdio.h Makefile
	$(MGCC) -c -o local_apic_c.o $(CFLAGS) $(CFLAGS2) local_apic_c.c

clock_modulation.o: clock_modulation.c ../include/stdio.h ../include/sys/clock_modulation.h Makefile
	$(MGCC) -c -o clock_modulation.o $(CFLAGS) $(CFLAGS2) clock_modulation.c

boot:
	make libs -C boot/

boot_without_libboot:
	make compile -C boot/
