# Makefile_marte.cfg should be included previously to this file (to use MPATH)

# Variables for C/C++
CC = $(MPATH)/utils/mgcc
CXX = $(MPATH)/utils/mg++
CFLAGS += -Wall -g -Werror
LDFLAGS +=
# Variables for Ada
GNATMAKE = $(MPATH)/utils/mgnatmake
GNATFLAGS += -g