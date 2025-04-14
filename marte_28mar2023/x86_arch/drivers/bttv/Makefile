# Creacion de la libreria libbttv.a
FLAGS=
MGCC = ../../../utils/mgcc
#-g
INCLUDES_LINUX=-I./include/

creacion objs:
# BTTV
	rm -f libbttv.a
	$(MGCC) -c $(FLAGS) $(INCLUDES_LINUX) -I. *.c

# creacion libreria
	ar -rc libbttv.a *.o
	rm *.o
	cp libbttv.a ../../../lib
