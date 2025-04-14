.PHONY: all clean

GNAT = ../utils/mgnatmake
ADA_FLAGS = -gnata -gnato -g -Imarte_src_dirs -aO../lib

all:
	$(GNAT) -c $(ADA_FLAGS) posix_all.ads -D ../lib

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"
