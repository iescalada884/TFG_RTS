.PHONY: clean

CC = ../../../utils/mgcc
C_FLAGS = -g -Wall

DEFAULT: rt61.o

test_rt61: test_rt61.c rt61.o rt61-interface.h
	$(CC) $(CFLAGS) test_rt61.c rt61.o
	rm *.o

rt61.o : rt61.c rt61.h rt61-interface.h rt61-linux-layer.h
	$(CC) -c $(CFLAGS) rt61.c

clean:
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
 	-or -name '*.exe' \) -print -delete