
# CFLAGS is an environment variable set by mklibmc
DEFINES=
CFLAGS2= -w
MGCC = ../../../utils/mgcc

DEFAULT: libm

libm:
	$(MGCC) -c *.c $(CFLAGS) $(CFLAGS2)
	ar -r $(MPATH)/lib/libm.a *.o
