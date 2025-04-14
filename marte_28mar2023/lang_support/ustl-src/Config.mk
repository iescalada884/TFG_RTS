################ Library version #####################################

LIBNAME		= ustl
MAJOR		= 1
MINOR		= 2
BUILD		= 0

################ Build options #######################################

# BUILD_SHARED	= 1
BUILD_STATIC	= 1
#DEBUG		= 1
NOLIBSTDCPP	= 1

################ Progams #############################################

CXX		= ../../utils/mg++
LD		= ../../utils/mgcc
AR		= ar
RANLIB		= ranlib
DOXYGEN		= doxygen
INSTALL		= /usr/bin/install

INSTALLDIR	= ${INSTALL} -d
INSTALLLIB	= ${INSTALL} -p -m 644
INSTALLEXE	= ${INSTALL} -p -m 755
INSTALLDATA	= ${INSTALL} -p -m 644

################ Destination #########################################

prefix		= ../ustl
exec_prefix	= ../ustl
BINDIR		= ../ustl/bin
INCDIR		= ../../x86_arch/include/ustl
LIBDIR		= ../../lib

################ Compiler options ####################################

WARNOPTS	= -Wall -Wpointer-arith -Wno-cast-align -Wcast-qual -Wsynth \
		-W -Wconversion -Wsign-promo -Woverloaded-virtual -Wshadow  \
		-Wwrite-strings -Wredundant-decls
TGT_OPTS	= -mmmx -msse -mfpmath=sse -msse2  \
		 --param max-inline-insns-single=1024 \
		--param large-function-growth=65535 \
		--param inline-unit-growth=1024 \
		-fvisibility-inlines-hidden

CXXFLAGS	=   -I../ustl/include \
                ${TGT_OPTS} ${WARNOPTS} -I../../x86_arch/include

LDFLAGS		=   -L../ustl/lib

ifdef DEBUG
    CXXFLAGS	+= -O0 -g
else
    CXXFLAGS	+= -O2 -DNDEBUG=1 -nostdinc
    LDFLAGS	+= -s
endif

################ Linker options ######################################

ifdef NOLIBSTDCPP
    LD		= ../../utils/mgcc
    STAL_LIBS	= -lang=cxx # -lsupc++
    LIBS	= ${STAL_LIBS}
endif
SHBLDFL		= -shared -Wl,-soname=${LIBSOLNK}
LIBA		= lib${LIBNAME}.a
LIBSO		= lib${LIBNAME}.so
ifdef MAJOR
    LIBSOLNK	= ${LIBSO}.${MAJOR}
    LIBSOBLD	= ${LIBSO}.${MAJOR}.${MINOR}.${BUILD}
endif

