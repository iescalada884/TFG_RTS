.clean: all

all:
	make -C cxx/libsupc++/ all
	make -C ustl-src/ all
	make -C ustl-src/ install

clean:
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
	-or -name '*.exe' -or -name 'a.out' \) -print -delete
	make -C ustl-src/ clean
	@rm -R -f ustl
