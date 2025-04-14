CC = ../../../utils/mgcc
CFLAGS = -Wall

console_switcher.o: console_switcher.c ../serial_port/serial_port_driver_c.c ../text_console_driver/text_console.c Makefile

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali'  \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"