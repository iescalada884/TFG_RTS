.PHONY: compile

CFLAGS2=

OBJS_C= armDivision.o bit_operations.o context_switch.o divmod.o drawing.o frameBuffer.o hdmi_console.o interrupts.o mailbox.o maths.o processor_registers.o

MGCC=../../utils/mgcc

compile:
	$(MGCC) -c *.c  $(CFLAGS) $(CFLAGS2) $(INCLUDES) -w -v
	$(MGCC) -c *.S  $(CFLAGS) $(CFLAGS2) $(INCLUDES) -w -v
