CC = mgcc
CFLAGS = -Wall -g
GNAT = ../../utils/mgnatmake
ADA_FLAGS = -gnata -gnato -g -Imarte_src_dirs

default: logger.o

logger.o:
	make -C ../../misc logger.o

test_logger.exe : test_logger.c logger.o
	$(CC) $(CFLAGS) test_logger.c ../../misc/logger.o -o test_logger.exe

test_logger_manual.exe : test_logger_manual.c logger.o
	$(CC) $(CFLAGS) test_logger_manual.c ../../misc/logger.o -o test_logger_manual.exe

test_logger_manual_ada.exe : test_logger_manual_ada.adb logger.o
	$(GNAT) $(ADA_FLAGS) test_logger_manual_ada.adb -o test_logger_manual_ada.exe

linux_eth_receive_log.exe: linux_eth_receive_log.c linux_eth_receive_log.h
	gcc $(CFLAGS) linux_eth_receive_log.c -o linux_eth_receive_log.exe

linux_rtep_sniffer.exe: linux_rtep_sniffer.c
	gcc $(CFLAGS) linux_rtep_sniffer.c -o linux_rtep_sniffer.exe

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"