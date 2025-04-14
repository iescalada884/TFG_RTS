/*
Linux Real Mode Interface - A library of DPMI-like functions for Linux.

Copyright (C) 1998 by Josh Vanderhoof

You are free to distribute and modify this file, as long as you
do not remove this copyright notice and clearly label modified
versions as being modified.

This software has NO WARRANTY.  Use it at your own risk.
*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>

//#include <ll/i386/hw-data.h>
//#include <ll/i386/x-dosmem.h>

#include "lrmi.h"
#include "libvga.h"

/*This file is not used in MaRTE OS*/

int
LRMI_init(void)
	{
	  //DOS_mem_init();
	return 0;
	}

void *
LRMI_alloc_real(int size)
	{
	  //return DOS_alloc(size);
	  return 0x0;
	}

void
LRMI_free_real(void *m, int s)
	{
	  //	DOS_free(m,s);
	}


int LRMI_int(int i, struct LRMI_regs *r)
	{

/*	unsigned char p1,p2;
		
	X_REGS16 inregs, outregs;
	X_SREGS16 sregs;

	memset(&inregs,0,sizeof(inregs));
	memset(&sregs,0,sizeof(sregs));
	
	inregs.x.ax = r->eax;
	inregs.x.bx = r->ebx;
	inregs.x.cx = r->ecx;
	inregs.x.dx = r->edx;
	sregs.es = r->es;
	sregs.es = r->ds;
	inregs.x.di = r->edi;
	
#ifndef VM86
	p1 = inp(0x21);
	p2 = inp(0xA1);
	outp(0x21,0xFF);
	outp(0xA1,0xFF);
	X_callBIOS(i, &inregs, &outregs, &sregs);
	outp(0x21,p1);
	outp(0xA1,p2);
#else
	vm86_callBIOS(i, &inregs, &outregs, &sregs);
#endif

	r->eax = outregs.x.ax;
	r->ebx = outregs.x.bx;
	r->ecx = outregs.x.cx;
	r->edx = outregs.x.dx;
	r->esi = outregs.x.si;
	r->edi = outregs.x.di;
*/	
	return 1;
	
	}

