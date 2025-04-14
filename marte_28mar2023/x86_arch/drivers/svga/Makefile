# Creacion de la libreria libsvga2.a
CFLAGS= # -O3 -march=pentium-m (improves performance by 1% or so only)
MGCC = ../../../utils/mgcc
#-g
all:
# common
	$(MGCC) -c -I. $(CFLAGS) vgamem.c
	$(MGCC) -c -I. $(CFLAGS) accel.c
	$(MGCC) -c -I. $(CFLAGS) attdacs.c
	$(MGCC) -c -I. $(CFLAGS) btdacs.c
	$(MGCC) -c -I. $(CFLAGS) glib.c
	$(MGCC) -c -I. $(CFLAGS) IBMRGB52x.c
	$(MGCC) -c -I. $(CFLAGS) icd2061a.c
	$(MGCC) -c -I. $(CFLAGS) ics_gendac.c
	$(MGCC) -c -I. $(CFLAGS) icw.c
	$(MGCC) -c -I. $(CFLAGS) interface.c
	$(MGCC) -c -I. $(CFLAGS) interrupt.c
	$(MGCC) -c -I. $(CFLAGS) lrmi.c
	$(MGCC) -c -I. $(CFLAGS) modetab.c
	$(MGCC) -c -I. $(CFLAGS) neo.c
	$(MGCC) -c -I. $(CFLAGS) normal.c
	$(MGCC) -c -I. $(CFLAGS) nv3.c
	$(MGCC) -c -I. $(CFLAGS) r128.c
	$(MGCC) -c -I. $(CFLAGS) ramdac.c
	$(MGCC) -c -I. $(CFLAGS) s3dacs.c
	$(MGCC) -c -I. $(CFLAGS) savage.c
	$(MGCC) -c -I. $(CFLAGS) sierra.c
	$(MGCC) -c -I. $(CFLAGS) timing.c
	$(MGCC) -c -I. $(CFLAGS) vesa.c
	$(MGCC) -c -I. $(CFLAGS) vga.c
	$(MGCC) -c -I. $(CFLAGS) vgaaccel.c
	$(MGCC) -c -I. $(CFLAGS) vgaclear.c
	$(MGCC) -c -I. $(CFLAGS) vgacopy.c
	$(MGCC) -c -I. $(CFLAGS) vgadraw.c
	$(MGCC) -c -I. $(CFLAGS) vgadrv.c
	$(MGCC) -c -I. $(CFLAGS) vgafig.c
	$(MGCC) -c -I. $(CFLAGS) vga_helper.c
	$(MGCC) -c -I. $(CFLAGS) vgaio.c
	$(MGCC) -c -I. $(CFLAGS) vgaline.c
	$(MGCC) -c -I. $(CFLAGS) vgamisc.c
	$(MGCC) -c -I. $(CFLAGS) vgammvgaio.c
	$(MGCC) -c -I. $(CFLAGS) vgapal.c
	$(MGCC) -c -I. $(CFLAGS) vgapci.c
	$(MGCC) -c -I. $(CFLAGS) vgapix.c
	$(MGCC) -c -I. $(CFLAGS) vgaregs.c
	$(MGCC) -c -I. $(CFLAGS) vgarelvgaio.c
	$(MGCC) -c -I. $(CFLAGS) xpm.c

# creacion libreria
	rm -f libsvga.a
	ar -r libsvga.a *.o
	rm *.o
	cp libsvga.a ../../../lib/

clean:
	@exec echo -e "\n>> Cleaning... ";
	@find \( -name '*.[oa]' -or -name '*~*' -or -name '*.ali' -or -name '*.log' \
		-or -name '*.exe' -or -name 'a.out' -or -name 'mprogram*' \) -print -delete
	@exec echo ">> End Cleaning"
