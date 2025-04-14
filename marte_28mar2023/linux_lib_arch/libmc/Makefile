
# CFLAGS is an environment variable set by mklibmc
DEFINES=
CFLAGS2= -Wall -m32 -march=i686
MGCC = ../../utils/mgcc

all: libmc

libmc:
	$(MGCC) -c printc.c pthread_cleanup_void.c $(CFLAGS) $(CFLAGS2) $(DEFINES)
	gcc -c write_read_wrapper.c $(CFLAGS) $(CFLAGS2) $(DEFINES)
	gcc -c get_linux_errno.c $(CFLAGS) $(CFLAGS2) $(DEFINES)
	$(MGCC) -c stdio/*.c $(CFLAGS) $(CFLAGS2) $(DEFINES)

	mv *.o $(MPATH)/lib/
