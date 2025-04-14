MGCC = ../../../utils/mgcc
CFLAGS = -Wall -g

default:
	make -C ../../../misc circular_memory_buffer.o
	cp ../../../misc/circular_memory_buffer.o .
	$(MGCC) -c $(CFLAGS) membuffer_driver_c.c

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali'  \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"