.PHONY: clean

CC = ../../../utils/mgcc
MGNATMAKE=../../../utils/mgnatmake
C_FLAGS = -g -Wall
ADA_INCLUDES = -li2c
ADA_FLAGS = -gnata -gnato -g  # -gnatn -gnatp -O3
DEFAULT: cmps03_c.o

cmps03_c.o: cmps03_c.c
	$(CC) -c $(CFLAGS) cmps03_c.c

test_cmps03:
	$(MGNATMAKE) $(ADA_FLAGS) test_cmps03.adb $(ADA_INCLUDES)

test_cmps03_c:
	$(CC) $(C_FLAGS) test_cmps03_c.c -li2c

clean:
	@find \( -name '*.[oa]' -or -name '*.ali' -or -name '*~*' -or -name 'mprogram*' \
 	-or -name '*.exe' \) -print -delete
