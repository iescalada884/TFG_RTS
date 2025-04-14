
CFLAGS2=

OBJS_C= marte-direct_io_c.o                    \
	miscellany.o                           \
	marte-spy_c.o                          \
	marte-getitimer_c.o                    \
	marte-setitimer_c.o

DEFAULT: $(OBJS_C)

marte-direct_io_c.o: marte-direct_io_c.c Makefile
	../utils/mgcc -c $(CFLAGS) $(CFLAGS2) marte-direct_io_c.c

miscellany.o: miscellany.c Makefile
	../utils/mgcc -c  $(CFLAGS) $(CFLAGS2) miscellany.c

marte-spy_c.o: marte-spy_c.c Makefile
	../utils/mgcc -c  $(CFLAGS) $(CFLAGS2) -U_POSIX_SOURCE marte-spy_c.c

marte-getitimer_c.o: marte-getitimer_c.c Makefile
	../utils/mgcc -c  $(CFLAGS) $(CFLAGS2) marte-getitimer_c.c

marte-setitimer_c.o: marte-setitimer_c.c Makefile
	../utils/mgcc -c  $(CFLAGS) $(CFLAGS2) marte-setitimer_c.c
