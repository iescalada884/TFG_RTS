
# CFLAGS, MPATH and MARCH are environment variables set by mklibmc
ifeq ($(MARCH),rpi)
INCLUDES=-I.
else
INCLUDES=-nostdinc -I. -I$(MPATH)/include/ -I$(MPATH)/include/oskit/c
endif
DEFINES=
CFLAGS2= -Wall
MGCC = ../../utils/mgcc

DEFAULT: libmc

libmc:
	$(MGCC) -c *.c  $(CFLAGS) $(CFLAGS2) $(INCLUDES) $(DEFINES)

	$(MGCC) -c stdtime/*.c $(CFLAGS) $(CFLAGS2) $(INCLUDES) $(DEFINES)

	$(MGCC) -c stdio/*.c $(CFLAGS) $(CFLAGS2) $(INCLUDES) $(DEFINES)

	$(MGCC) -c stdlib/*.c $(CFLAGS) $(CFLAGS2) -w $(INCLUDES) $(DEFINES)

	$(MGCC) -c string/*.c $(CFLAGS) $(CFLAGS2) $(INCLUDES) $(DEFINES)

ifeq ($(MARCH),rpi)
	$(MGCC) -c common/*.c $(CFLAGS) $(CFLAGS2) $(INCLUDES) $(DEFINES)
else
	$(MGCC) -c x86/*.c $(CFLAGS) $(CFLAGS2) $(INCLUDES) $(DEFINES)
	$(MGCC) -c x86/*.S  $(CFLAGS) $(CFLAGS2) $(INCLUDES) $(DEFINES) -DASSEMBLER
endif

	mv *.o $(MPATH)/lib/
