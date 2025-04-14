.PHONY: call_main  hwi

all: call_main  hwi

all_but_libboot: call_main hwi_without_libboot

call_main:
	make -C call_main/

hwi:
	make -C hwi/

hwi_without_libboot:
	make no_libboot -C hwi/
