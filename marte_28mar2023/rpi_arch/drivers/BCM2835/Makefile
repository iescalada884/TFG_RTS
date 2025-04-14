MGCC = ../../../utils/mgcc

DEFAULT: bcm2835.o  hs422.o  phat.o  soft_pwm.o

%.o: %.c Makefile
	$(MGCC) -c $(CFLAGS) $<
