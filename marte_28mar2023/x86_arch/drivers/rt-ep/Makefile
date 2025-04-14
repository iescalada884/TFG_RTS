.PHONY: all help clean

GNATMAKE  = ../../../utils/mgnatmake
GNATBIND  = ../../../utils/mgnatbind
ADA_INCLUDES = -Imarte_src_dirs -aO../../../lib
ADA_FLAGS = -fno-strict-aliasing -gnatn -gnatp -O2 # -gnata -gnato -g  # -gnatn -gnatp -O3 # -fno-strict-aliasing -gnatn -gnatp -O2
HELP_SOURCES = $(wildcard *.ads)
HELP_SOURCES += -Imarte_src_dirs -I../../../lib

all:
	@exec echo -e "\n>> Building RTEP:";
	@$(GNATMAKE) -c $(ADA_FLAGS) rtep_c.ads $(ADA_INCLUDES) -D ../../../lib 1> /dev/null
	@$(GNATBIND) -Lrtep_ada -F -n rtep_c.ali $(ADA_INCLUDES) 1> /dev/null
	@$(GNATMAKE) -c $(ADA_FLAGS) b~rtep_c.adb -D ../../../lib 1> /dev/null
	@exec echo "  [OK]"

help: all
	@exec echo -e "\n>> Generating RTEP Cross-Reference html help:";
	@gnathtml.pl -cc green -d -l1 $(HELP_SOURCES) -o doc/html > /dev/null
	@tar -C doc/ -zcvf doc/rtep_xref_html.tar.gz html > /dev/null
	@exec echo "  [OK], check doc/html/index.html"

clean:
	@exec echo -e "\n>> Cleaning up RTEP directories:";
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
	-or -name '*.exe' \) -print -delete
	@rm -Rfv doc/html
	@exec echo "  [OK]"