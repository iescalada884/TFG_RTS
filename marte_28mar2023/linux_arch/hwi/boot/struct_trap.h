#ifndef _STRUCT_TRAP_
#define _STRUCT_TRAP_

/* Definition taken from Oskit include */
struct trap_state
{
        /* Saved segment registers */
        unsigned int    gs;
        unsigned int    fs;
        unsigned int    es;
        unsigned int    ds;

        /* PUSHA register state frame */
        unsigned int    edi;
        unsigned int    esi;
        unsigned int    ebp;
        unsigned int    cr2;    /* we save cr2 over esp for page faults */
        unsigned int    ebx;
        unsigned int    edx;
        unsigned int    ecx;
        unsigned int    eax;

        /* Processor trap number, 0-31.  */
        unsigned int    trapno;

        /* Error code pushed by the processor, 0 if none.  */
        unsigned int    err;

        /* Processor state frame */
        unsigned int    eip;
        unsigned int    cs;
        unsigned int    eflags;
        unsigned int    esp;
        unsigned int    ss;

        /* Virtual 8086 segment registers */
        unsigned int    v86_es;
        unsigned int    v86_ds;
        unsigned int    v86_fs;
        unsigned int    v86_gs;
};

#endif
