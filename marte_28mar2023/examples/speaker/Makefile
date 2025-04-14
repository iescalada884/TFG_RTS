GNATMAKE = mgnatmake
GNATFLAGS = -Wall -g
LDFLAGS =

default:
	@exec echo -e "\n>> Nothing by default ";

%.exe: %.adb
	@exec echo -e "\n>> Building $@: ";
	@if [ -f $< ]; \
	then \
		$(GNATMAKE) $(GNATFLAGS) $< $(LDFLAGS) -o $@; \
	fi;
	@exec echo ">> End Building $@";

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' -or -name 'mprogram*' \) -print -delete
	@exec echo ">> End Cleaning"