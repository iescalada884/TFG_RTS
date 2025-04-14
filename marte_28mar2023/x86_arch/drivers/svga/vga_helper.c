#include <stdio.h>
//#include <kernel/log.h>
//#include <ll/i386/hw-instr.h>
//#include <ll/i386/hw-io.h>
#include <sys/pio.h>
#include "tipos.h"

void port_rep_outb(unsigned char* string, int length, int port)
{
   printf("port_rep_outb: NOT SUPPORTED !!\n");
}

void port_out(int value, int port)
{
   outb(port,(BYTE)value); 
}

void port_outw(int value, int port)
{
   outw(port,(WORD)value);
}

void port_outl(int value, int port)
{
   outl(port,(DWORD)value);   
}

int port_in(int port)
{
    return (WORD)inb(port);
}

int port_inw(int port)
{
    return (WORD)inw(port);
}

int port_inl(int port)
{
    return (WORD)inl(port);
}

