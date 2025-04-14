GNATMAKE = mgnatmake

drivers_dir := $(wildcard ../../arch/drivers/*)
GNATFLAGS = $(foreach dir,$(drivers_dir),-aI$(dir))
GNATFLAGS += -aI../../kernel/ -aI../../sll/ -aI../../arch/hwi/ \
	-aI../../posix5/ -aI../../misc/ -aI../ada/reports -aO../../lib/

default: tetris.exe

tetris.exe:
	$(GNATMAKE) $(GNATFLAGS) tetris.adb -o $@

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"