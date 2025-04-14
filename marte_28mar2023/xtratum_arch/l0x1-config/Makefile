# XAL_PATH: path to the XTRATUM directory
XAL_PATH=/home/xmuser/xm_v3.7.1-bin/xal

all: container.bin resident_sw
include $(XAL_PATH)/common/rules.mk

# MPATH, M_LD_LIBSPATH and M_LD_GCC_LIBS are defined in
# 'utils/Makefile_marte.cfg'

-include dep.mk

# XMLCF: path to the XML configuration file
XMLCF=xm_cf.$(ARCH).xml

TARGET_CFLAGS+=-I$(MPATH)/arch/include -g
CFLAGS+=-I$(MPATH)/arch/include -g
ADAFLAGS+=
ADALINKERFLAGS+= 

LDFLAGS+=
TARGET_LDFLAGS+=

ifndef ADA_MAIN
# not Ada
MaRTE_WRAPPER_MAIN=$(MPATH)/arch/call_main/wrapper_main_c.o
else
# Ada program
MaRTE_WRAPPER_MAIN=$(MPATH)/arch/call_main/wrapper_main_ada.o
endif

MaRTE_LDFLAGS=$(M_LD_LIBSPATH) $(MaRTE_WRAPPER_MAIN) $(M_LD_GCC_LIBS) -lxm -lxal --start-group -lgcc -lgcc_eh -lc --end-group

XTRATUM_FLAGS = -Wall -O0 -nostdlib -nostdinc -Dx86 -fno-strict-aliasing -fomit-frame-pointer -I/home/xmuser/xm_v3.7.1-bin/xm/include --include xm_inc/config.h --include xm_inc/arch/arch_types.h -m32 -fomit-frame-pointer -I/home/xmuser/xm_v3.7.1-bin/xal/include -fno-builtin

# PARTITIONS: partition files (xef format) composing the example
PARTITIONS=partition1.xef

partition1: $(TARGET1).o  $(EXTRA_OBJS) $(MPATH)/arch/hwi/marte_extirqhandler.o
ifndef ADA_MAIN
# not Ada
	$(LD) -o $@ $(filter %.o,$^) $(TARGET_LDFLAGS) $(LDFLAGS) $(MaRTE_LDFLAGS) -Ttext=$(shell $(XPATHSTART) 0 $(XMLCF))
else
# Ada program
	$(LD) -o $@ b~$(TARGET1).o `sed -n -e '/BEGIN Object/,/END Object/p' < b~$(TARGET1).adb | sed 's/\s*--\s*//g' | egrep -v 'BEGIN|END'  | egrep -v '../../lib' | egrep -v '^-'` $(EXTRA_OBJS) $(MPATH)/arch/hwi/marte_extirqhandler.o $(TARGET_LDFLAGS) $(ADALINKERFLAGS) $(LDFLAGS) $(MaRTE_LDFLAGS) -Ttext=$(shell $(XPATHSTART) 0 $(XMLCF)) -v
endif

	$(XEF) partition1 -o partition1.xef

PACK_ARGS=-h $(XMCORE):xm_cf.xef.xmc \
	-p 0:partition1.xef
	
container.bin: xm_cf.xef.xmc
	$(XMPACK) check xm_cf.xef.xmc $(PACK_ARGS)
	$(XMPACK) build $(PACK_ARGS) $@

iso: resident_sw
	mkiso $@ $^

ifdef ADA_MAIN
$(TARGET1).o:
	mgnatmake -c $(ADAFLAGS) $(GNATFLAGS) $(TARGET1).adb $(ADALINKERFLAGS) -Imarte_src_dirs
	mgnatbind -F $(ADAFLAGS) $(filter-out -gnat05,$(GNATFLAGS)) $(TARGET1).ali -Imarte_src_dirs
	mgnatmake -c $(ADAFLAGS) b~$(TARGET1).adb
endif

# rule to be redeclared in calling Makefile when needed
.PHONY: previous
previous:

