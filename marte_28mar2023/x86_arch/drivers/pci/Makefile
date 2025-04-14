CC = ../../../utils/mgcc

DEFAULT: pci.o

pci.o : pci.c ../../include/sys/pci.h ../../include/sys/pci_ids.h pio.h
	$(CC) -c $(CFLAGS) pci.c

