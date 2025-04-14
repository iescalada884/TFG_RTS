WARNINGS = -Wextra -Wall -Wwrite-strings -Wstrict-prototypes \
           -Wmissing-prototypes -Wno-long-long -Wstrict-aliasing=2
CFLAGS+=-g -O2 $(WARNINGS) -DTLSF_USE_LOCKS=1
# CFLAGS+=-ftest-coverage -fprofile-arcs
CC=../../../utils/mgcc

SRCS := $(wildcard *.c -or *.h)
OBJS := $(patsubst %.c,%.o,$(wildcard *.c))

all: $(OBJS)
	mv *.o ../

%.o: $(SRCS)

clean:
	$(RM) -rf *.o *~ *.c.gcov *.gcda *.gcno
