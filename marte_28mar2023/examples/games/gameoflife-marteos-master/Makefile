.PHONY: clean default life.exe

CC = mgcc
CFLAGS = -Wall -I../../../arch/drivers/svga
LDFLAGS = -lsvga -lm
GNATMAKE = mgnatmake
ADAFLAGS = -gnata -gnato -g

default: life.exe

life.exe: 
	$(CC) $(CFLAGS) life.c $(LDFLAGS) -o $@;

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' \) -print -delete
	@exec echo ">> End Cleaning"
