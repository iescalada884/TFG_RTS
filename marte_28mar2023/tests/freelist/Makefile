# include MaRTE configuration (architecture) 
include ../../utils/Makefile_marte.cfg

ifneq ($(CURRENT_ARCH),xtratum)
# any architecture but "xtratum"

include ../config.mk
include ../rules.mk

else
# xtratum architecture

include ../../arch/l0x1-config/Makefile

previous:
	ln -sf ../../arch/l0x1-config/p1.cfg
	ln -sf ../../arch/l0x1-config/xm_cf.ia32.xml

endif

dependencies:
	make -C ../../misc/ freelist.o

CFLAGS += -I../../include/misc
LDFLAGS += ../../misc/freelist.o
GNATFLAGS += -gnat05 -I../ada/reports/ -I../../misc -aI../../kernel -aO../../lib -largs ../../misc/freelist.o -margs