
CFLAGS2=

OBJS_C= base_multiboot_main.o \
	wrapper_main_c.o \
	wrapper_main_ada.o \
	wrapper_main_cpp.o

all: $(OBJS_C)

base_multiboot_main.o: base_multiboot_main.c Makefile
	../../utils/mgcc -o base_multiboot_main.o \
		-c $(CFLAGS) $(CFLAGS2) base_multiboot_main.c

wrapper_main_c.o: wrapper_main_c.c Makefile
	../../utils/mgcc -o wrapper_main_c.o \
		-c $(CFLAGS) $(CFLAGS2) wrapper_main_c.c

wrapper_main_ada.o: wrapper_main_ada.c Makefile
	../../utils/mgcc -o wrapper_main_ada.o \
		-c $(CFLAGS) $(CFLAGS2) wrapper_main_ada.c

wrapper_main_cpp.o: wrapper_main_c.o
	cp wrapper_main_c.o wrapper_main_cpp.o
