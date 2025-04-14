# Creacion de la libreria libbttv.a
FLAGS=
#-g
INCLUDES_LINUX=-I../include/
INCLUDES_BTTV=-I..
INCLUDES_SVGA=-I../../svga

PROGRAM=una_captura
none:
	@echo "use 'make <target>'"
	@echo "   targets: una_captura   test_continuo"

una_captura: una_captura.c
	mgcc $(FLAGS) $(INCLUDES_BTTV) $(INCLUDES_SVGA) $(INCLUDES_LINUX) una_captura.c -lbttv -lsvga -lm
	rm una_captura.o

test_continuo: test_continuo.c
	mgcc $(FLAGS) $(INCLUDES_BTTV) $(INCLUDES_SVGA) $(INCLUDES_LINUX) test_continuo.c -lbttv -lsvga -lm
	rm test_continuo.o
