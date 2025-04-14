
CFLAGS2=

OBJS_C=	start.o \
	wrapper_main_c.o \
	wrapper_main_ada.o

all: $(OBJS_C)

.PHONY: FORCE

start.o: start.S Makefile FORCE
	../../utils/mgcc -o start.o \
		-c $(CFLAGS) $(CFLAGS2) start.S

wrapper_main_c.o: wrapper_main_c.c Makefile FORCE
	../../utils/mgcc -o wrapper_main_c.o \
		-c $(CFLAGS) $(CFLAGS2) wrapper_main_c.c

wrapper_main_ada.o: wrapper_main_ada.c Makefile FORCE
	../../utils/mgcc -g -o wrapper_main_ada.o \
		-c $(CFLAGS) $(CFLAGS2) wrapper_main_ada.c

#wrapper_main_cpp.o: wrapper_main_c.o
#	cp wrapper_main_c.o wrapper_main_cpp.o
