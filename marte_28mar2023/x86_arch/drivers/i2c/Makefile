.PHONY: all help clean

GNATMAKE  = ../../../utils/mgnatmake
GNATBIND  = ../../../utils/mgnatbind
CC        = ../../../utils/mgcc
CFLAGS    = -g -Wall
ADA_INCLUDES = -Imarte_src_dirs -aO../../../lib
ADA_FLAGS = -gnata -gnato -g # -gnata -gnato -g  # -gnatn -gnatp -O3 # -fno-strict-aliasing -gnatn -gnatp -O2
HELP_SOURCES = $(wildcard *.ads)
HELP_SOURCES += -Imarte_src_dirs -I../../../lib

all: i2c_elite_c.o i2c_c_interface.o
	@exec echo -e "\n>> Building I2C:";
	@$(GNATMAKE) -c $(ADA_FLAGS) i2c_c_bindings.ads $(ADA_INCLUDES) -D ../../../lib 1> /dev/null
	@$(GNATBIND) -Li2c_ada -F -n i2c_c_bindings.ali $(ADA_INCLUDES) 1> /dev/null
	@$(GNATMAKE) -c $(ADA_FLAGS) b~i2c_c_bindings.adb -D ../../../lib 1> /dev/null
	@exec echo "  [OK]"

i2c_elite_c.o : i2c_elite_c.c ../../include/sys/i2c_elite.h
	$(CC) -c $(CFLAGS) i2c_elite_c.c -o ../../../lib/$@

i2c_c_interface.o : i2c_c_interface.c ../../include/sys/i2c.h
	$(CC) -c $(CFLAGS) i2c_c_interface.c -o ../../../lib/$@

%.exe:
	@exec echo -en "\n>> Compiling $@: ";
	@if [ -f $*.c ]; then $(CC) $(CFLAGS) $*.c -o $@ 1> /dev/null; fi
	@if [ -f $*.adb ]; then $(GNATMAKE) $(ADA_FLAGS) $*.adb $(ADA_INCLUDES) -o $@ 1> /dev/null; fi
	@exec echo "  [OK]";

help: all
	@exec echo -e "\n>> Generating I2C Cross-Reference html help:";
	@gnathtml.pl -cc green -d -l1 $(HELP_SOURCES) -o html > /dev/null
	@exec echo "  [OK], check ./html/index.html"

clean:
	@exec echo -e "\n>> Cleaning up I2C directories:";
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
	-or -name '*.exe' \) -print -delete
	@exec echo "  [OK]"
