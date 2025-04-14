.PHONY: clean default

CC=mgcc
CXX=mg++
GNATMAKE  = ../../../utils/mgnatmake
GNATBIND  = ../../../utils/mgnatbind
ADA_INCLUDES = -Imarte_src_dirs -aO../../../lib
ADA_FLAGS = -fno-strict-aliasing -gnatn -gnatp -O2
CFLAGS = -g -Wall
SRC := $(wildcard *.c -or *.adb)

default:
	@exec echo -e "\n>> Building SB16:";
	@$(GNATMAKE) -c $(ADA_FLAGS) sb16.adb $(ADA_INCLUDES) -D ../../../lib 1> /dev/null
	@$(GNATMAKE) -c $(ADA_FLAGS) dma.adb $(ADA_INCLUDES) -D ../../../lib 1> /dev/null
	@exec echo "  [OK]"

%.exe: $(SRC)
	@exec echo -en "\n>> Compiling $@: ";
	@if [ -f $*.c ]; then $(CC) $(CFLAGS) $*.c -o $@ 1> /dev/null; fi
	@if [ -f $*.adb ]; then $(GNATMAKE) $(ADA_FLAGS) $(ADA_INCLUDES) $*.adb -o $@ 1> /dev/null; fi
	@if [ -f $*.cc ]; then $(CXX) $(CFLAGS) $*.cc -o $@ 1> /dev/null; fi
	@exec echo "  [OK]";

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"
