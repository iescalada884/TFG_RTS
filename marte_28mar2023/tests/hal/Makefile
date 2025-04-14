# include MaRTE configuration (architecture) 
include ../../utils/Makefile_marte.cfg

GNATFLAGS += -gnatwa -gnaty -gnatyu -gnatwe -gnatyg -I../ada/reports/ -Imarte_src_dirs -largs fake_marte_init.o -margs

ifneq ($(CURRENT_ARCH),xtratum)
# any architecture but "xtratum"

include ../config.mk
include ../rules.mk

else
# xtratum architecture

ADA_MAIN=1
ADAFLAGS=-g
include ../../arch/l0x1-config/Makefile

previous:
	ln -sf ../../arch/l0x1-config/p1.cfg
	ln -sf ../../arch/l0x1-config/xm_cf.ia32.xml

endif

dependencies: fake_marte_init.o


fake_marte_init.o: fake_marte_init.c
	mgcc -c -g fake_marte_init.c