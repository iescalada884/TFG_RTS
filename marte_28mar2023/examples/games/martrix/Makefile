.PHONY: clean default martrix.exe

GNATMAKE = mgnatmake
ADAFLAGS = -gnata -gnato -g

default: martrix.exe

martrix.exe:
	$(GNATMAKE) $(ADAFLAGS) martrix.adb -o martrix.exe -Imarte_src_dirs -aO../../lib -largs -lsvga -lm

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"
