SRCS := $(sort $(wildcard *.cc))
OBJS := $(patsubst %.cc,%.o,$(wildcard *.cc))
LOBJS := $(OBJS)

all: objs

CXXFLAGS=-m32 -march=i686 -fexceptions -Wall -g -fno-builtin -nostdlib -nostdinc -fno-strict-aliasing
CXXFLAGS+= -I../include -I../../include -I../../../x86_arch/include/ -I./ # -fno-omit-frame-pointer
# CXXFLAGS+=-g
CXXFLAGS+=-fomit-frame-pointer

# toolchain configuration
TC_PATH=
TC_PREF=
TC_SUF=

CXX = $(TC_PATH)$(TC_PREF)g++$(TC_SUF)


%.o: %.cc
	$(CXX) $(CXXFLAGS) -c -o $@ $<

objs: $(OBJS)
	$(Q)$(AR) -r libsupc++.a $(LOBJS) $(Q2)
	$(Q)cp libsupc++.a ../../../lib

clean:
	rm *.o *.a
