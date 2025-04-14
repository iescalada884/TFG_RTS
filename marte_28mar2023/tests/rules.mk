.PHONY: none dependencies clean clean_all

none:
	@exec echo -e "\nPlease introduce a target, i.e: 'make test_dtm_xxx.exe'";

%.exe: dependencies
	@exec echo -e "\n>> Building $@: ";
	@exec echo -e ">>    mgcc:`which mgcc` ";
	@exec echo -e ">>    mgnatmake:`which mgnatmake` ";
	@if [ -f $*.c ]; then $(CC) $(CFLAGS) $(LDFLAGS) $*.c -o $@; fi;
	@if [ -f $*.adb ]; \
	then \
 		$(GNATMAKE) $(GNATFLAGS) $*.adb -o $@; \
	fi;
	@if [ -f $*.cc ]; then $(CXX) $(CFLAGS) $(LDFLAGS) $*.cc -o $@; fi;
	@exec echo "  [OK]";

dependencies:

clean:
	@exec echo -e "\n>> Cleaning (log files not cleaned)... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali'  \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out'  \
		-or -name '*.exe.out' -or -name '*.exe.img'  \
		-or -name '*.exe.list' -or -name 'mprogram.map' \
		-or -name '*.exe.bin' \) -print -delete
	@exec echo ">> End Cleaning"

clean_all: clean
	@exec echo -e "\n>> Cleaning log files... ";
	@find \( -name '*.log' \) -print -delete
	@exec echo ">> End Cleaning"
