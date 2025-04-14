/*----------------------------------------------------------------------------
 *-- -------------------         M a R T E   O S         ------------------ --
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                            's i g c o n t e x t'
 *
 *                                      H
 *
 * File 'sigcontext.h'                                                 by Mar.
 *
 * XXX Taken from '/usr/i386-glibc20-linux/include/asm/sigcontext.h'
 * only to keep 'a-init.c' happy. It MUST be changed in the future.
 *
 * Copyright (C) 1992-2008, Free Software Foundation, Inc.
 *
 *---------------------------------------------------------------------------*/
#ifndef _SIGCONTEXT_H_
#define _SIGCONTEXT_H_

/*
 * As documented in the iBCS2 standard..
 *
 * The first part of "struct _fpstate" is just the
 * normal i387 hardware setup, the extra "status"
 * word is used to save the coprocessor status word
 * before entering the handler.
 */
struct _fpreg {
        unsigned short significand[4];
        unsigned short exponent;
};

struct _fpstate {
        unsigned long   cw,
                        sw,
                        tag,
                        ipoff,
                        cssel,
                        dataoff,
                        datasel;
        struct _fpreg   _st[8];
        unsigned long   status;
};

struct sigcontext_struct {
        unsigned short gs, __gsh;
        unsigned short fs, __fsh;
        unsigned short es, __esh;
        unsigned short ds, __dsh;
        unsigned long edi;
        unsigned long esi;
        unsigned long ebp;
        unsigned long esp;
        unsigned long ebx;
        unsigned long edx;
        unsigned long ecx;
        unsigned long eax;
        unsigned long trapno;
        unsigned long err;
        unsigned long eip;
        unsigned short cs, __csh;
        unsigned long eflags;
        unsigned long esp_at_signal;
        unsigned short ss, __ssh;
        struct _fpstate * fpstate;
        unsigned long oldmask;
        unsigned long cr2;
};


#endif /* _SIGCONTEXT_H_ */
