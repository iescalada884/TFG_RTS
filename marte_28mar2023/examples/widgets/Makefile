CC = mgcc
CFLAGS = -Wall -g

default:
	@exec echo -e "\n>> Nothing by default ";

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'mprogram' -or -name 'a.out' -or -name 'mprogram*' \) -print -delete
	@exec echo ">> End Cleaning"