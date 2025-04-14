CC = ../../utils/mgcc
MGNATMAKE = ../../utils/mgnatmake
CFLAGS = -Wall -I../../arch/drivers/svga
LDFLAGS = -lsvga -lm
GNATFLAGS = -g -aI../../kernel \
	-aI../../arch/drivers/mouse \
	-aI../../arch/drivers/keyboard_driver \
	-aI../../arch/drivers/text_console_driver \
	-aI../../arch/drivers/console_switcher \
	-aI../../arch/drivers/serial_port \
	-aI../../arch/drivers/eth_drv \
	-aI../../arch/hwi \
	-aI../../sll \
	-aI../../misc \
	-aI../../posix5 \
	-aO../../lib

default:
	@exec echo -e "\n>> please type make test_xxx.exe ";

%.exe: %.c oscilloscope.o
	@exec echo -e "\n>> Building $@: ";
	@if [ -f $< ]; \
	then \
		$(CC) $(CFLAGS) $< $(LDFLAGS) oscilloscope.o -o $@; \
	fi;
	@exec echo ">> End Building $@";

test_oscilloscope_mouse.exe: test_oscilloscope_mouse.c oscilloscope.o mouse_bindings.o
	$(CC) $(CFLAGS) $< $(LDFLAGS) ../../posix5/*.o ../../drivers/mouse/*.o oscilloscope.o mouse_bindings.o -o $@; \

mouse_bindings.o: mouse_bindings.adb mouse_bindings.ads
	$(MGNATMAKE) -c mouse_bindings.adb $(GNATFLAGS) -o $@

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' -or -name 'mprogram*' \) -print -delete
	@exec echo ">> End Cleaning"
