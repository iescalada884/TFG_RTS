CFLAGS=-g
DEFAULT:

edf: edf_sched.o edf_sched.h edf_threads.c Makefile
	mgcc $(CFLAGS) edf_threads.c edf_sched.o
edf_sched.o: edf_sched.h edf_sched.c Makefile
	mgcc $(CFLAGS) -c edf_sched.c

pcp: pcp_sched.o pcp_sched.h pcp_threads.c Makefile
	mgcc $(CFLAGS) pcp_threads.c pcp_sched.o
pcp_sched.o: pcp_sched.h pcp_sched.c Makefile
	mgcc $(CFLAGS) -c pcp_sched.c

cbs: cbs_sched.o cbs_sched.h cbs_threads.c Makefile
	mgcc $(CFLAGS) cbs_threads.c cbs_sched.o
cbs_sched.o: cbs_sched.h cbs_sched.c Makefile
	mgcc $(CFLAGS) -c cbs_sched.c
