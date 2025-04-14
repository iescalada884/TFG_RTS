MGCC = ../../../utils/mgcc

DEFAULT: uart_simple.o uart_simple_s.o

%.o: %.c Makefile
	$(MGCC) -c $(CFLAGS) $<

uart_simple_s.o: uart_simple_s.S
	$(MGCC) -c $<
