MGCC = ../../../../utils/mgcc
MGNATMAKE = ../../../../utils/mgnatmake

none:
	@echo "use 'make <target>'"
	@echo "   targets: test   test_xpm"

test: test.c
	$(MGCC) $(CFLAGS) test.c -lsvga -lm -I..

test_vga_rectangle_fill: test_vga_rectangle_fill.c
	$(MGCC) $(CFLAGS) test_vga_rectangle_fill.c -lsvga -lm -I..

test_vga: test_vga.adb
	$(MGNATMAKE) test_vga.adb -aI../  -largs -lsvga -lm

test_vga_640: test_vga_640.adb
	$(MGNATMAKE) test_vga_640.adb -aI../ -largs -lsvga -lm

test_xpm: test_xpm.c
	$(MGCC) $(CFLAGS) test_xpm.c -lsvga -lm -I..

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'a.out' -or -name 'mprogram*' \) -print -delete
	@exec echo ">> End Cleaning"