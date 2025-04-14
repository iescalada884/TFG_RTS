# include MaRTE configuration (architecture) 
include ../../utils/Makefile_marte.cfg

ifneq ($(CURRENT_ARCH),xtratum)
# any architecture but "xtratum"

include ../config.mk
include ../rules.mk

dependencies:
	make -C ../../misc/ circular_memory_buffer.o

else
# xtratum architecture

include ../../arch/l0x1-config/Makefile

previous:
	ln -sf ../../arch/l0x1-config/p1.cfg
	ln -sf ../../arch/l0x1-config/xm_cf.ia32.xml
	make -C ../../misc/ circular_memory_buffer.o

endif

CFLAGS += -I../../include/misc
LDFLAGS += ../../misc/circular_memory_buffer.o
