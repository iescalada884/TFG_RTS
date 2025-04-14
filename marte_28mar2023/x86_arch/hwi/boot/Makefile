
# CFLAGS is an environment variable set by mkkernel
INCLUDES=-nostdinc -I. -I../../include/ -I../../include/oskit/c
CFLAGS2=
MGCC=../../../utils/mgcc

.PHONY: compile libs

libs: compile
	mv *.o ../../../lib/

compile:
	$(MGCC) -c *.c  $(CFLAGS) $(CFLAGS2) $(INCLUDES) -w
	$(MGCC) -c *.S  $(CFLAGS) $(CFLAGS2) $(INCLUDES) -w -DASSEMBLER
