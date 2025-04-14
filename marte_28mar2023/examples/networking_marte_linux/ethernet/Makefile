# @Author: H. Perez
# @Date:   28-Feb-2018
# @Email:  perezh@unican.es
# @Last modified by:   marte
# @Last modified time: 28-Feb-2018

# COMPILING SETUP
C_COMPILER= /usr/bin/
CC        = $(C_COMPILER)/gcc
ADA_CC    = mgcc         # IN PATH
ADA_GNATMAKE = mgnatmake    # IN PATH

CFLAGS  = -Wall -Werror
LDFLAGS = #COMPLETE WITH FLAGS FOR LINKER

MPROGRAM =
EXPORT_DIR=/mnt/export

# DEFAULT TESTS TO COMPILE
all: bin/linux_logger bin/test_logger_ada

# RULES
bin/linux_logger: linux_eth_receive_log.c
	@mkdir -p $(@D)
	$(CC) -o $@ $< $(LDFLAGS) $(CFLAGS)

bin/test_eth: test_ethr_driver.c
	@mkdir -p $(@D)
	$(ADA_CC) -o $@ $< $(LDFLAGS) $(CFLAGS)
	@make copy MPROGRAM=$@ --no-print-directory

bin/test_logger_ada: test_logger_manual_ada.adb
		@mkdir -p $(@D)
		$(ADA_GNATMAKE) -o $@ $< $(LDFLAGS) $(CFLAGS) -Imarte_src_dirs
		@make copy MPROGRAM=$@ --no-print-directory

.PHONY: all clean

clean:
	rm -rf obj
	rm -f *~
	rm -f *.o *.ali
	rm -f bin/*

#COMPLETE WITH COMMAND FOR REMOTE LOAD
copy:
	cp $(MPROGRAM) $(EXPORT_DIR)/mprogram_ensa
