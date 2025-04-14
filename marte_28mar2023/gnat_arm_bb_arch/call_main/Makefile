
CFLAGS2=

OBJS_C= wrapper_main_c.o \
	wrapper_main_ada.o

all: $(OBJS_C)

.PHONY: FORCE

wrapper_main_c.o: wrapper_main_c.c Makefile FORCE
	../../utils/mgcc -o wrapper_main_c.o \
		-c $(CFLAGS) $(CFLAGS2) -dont_change_main_name wrapper_main_c.c

wrapper_main_ada.o: wrapper_main_ada.c Makefile FORCE
	../../utils/mgcc -g -o wrapper_main_ada.o \
		-c $(CFLAGS) $(CFLAGS2) -dont_change_main_name wrapper_main_ada.c

#wrapper_main_cpp.o: wrapper_main_c.o
#	cp wrapper_main_c.o wrapper_main_cpp.o
