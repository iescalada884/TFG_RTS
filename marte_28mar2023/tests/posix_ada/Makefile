# include MaRTE configuration (architecture) 
include ../../utils/Makefile_marte.cfg

GNATFLAGS += -I../ada/reports/ -Imarte_src_dirs

ifneq ($(CURRENT_ARCH),xtratum)
# any architecture but "xtratum"

include ../config.mk
include ../rules.mk

else
# xtratum architecture

ADA_MAIN=1
ADAFLAGS=-g
GNATFLAGS += -Imarte_src_dirs
include ../../arch/l0x1-config/Makefile

previous:
	ln -sf ../../arch/l0x1-config/p1.cfg
	ln -sf ../../arch/l0x1-config/xm_cf.ia32.xml

endif
