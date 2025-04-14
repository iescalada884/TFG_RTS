/* VGAlib version 1.2 - (c) 1993 Tommy Frandsen                    */
/*                                                                 */
/* This library is free software; you can redistribute it and/or   */
/* modify it without any restrictions. This library is distributed */
/* in the hope that it will be useful, but without any warranty.   */

/* Multi-chipset support Copyright 1993 Harm Hanemaayer */
/* partially copyrighted (C) 1993 by Hartmut Schirmer */

#include <stdlib.h>
#include "driver.h"
#include "vga.h"
#include "libvga.h"

#include "svgalib_helper.h"

vga_cardinfo *vga_getcardinfo(void) {
    vga_cardinfo *vci;
    
    vci = malloc(sizeof(vga_cardinfo));
    if(vci==NULL) return vci;
    vci->version = 0x100;
    vci->size = sizeof(vga_cardinfo);
    vci->chipset = __svgalib_chipset;
    vci->physmem = __svgalib_linear_mem_base;
    
    return vci;
}

void vga_waitretrace(void)
{
    if (__svgalib_driverspecs->emul && __svgalib_driverspecs->emul->waitretrace) {
        __svgalib_driverspecs->emul->waitretrace();
    } else {
	while (!(__svgalib_inis1() & 8));
	while (__svgalib_inis1() & 8);
    }
}

static void *__svgalib_linearframebuffer;
/*
 * The way IS_LINEAR gets indicated is rather convoluted; if the driver
 * has EXT_INFO_AVAILABLE, setlinearaddressing will enable
 * the flag in __svgalib_linearset which gets set in the modeinfo by
 * vga_getmodeinfo(). The driver must turn off the flag in
 * __svgalib_linearset if linear addressing gets disabled (e.g. when
 * setting another mode).
 * 
 * For any driver, the chipset getmodeinfo flag can examine a hardware
 * register and set the IS_LINEAR flag if linear addressing is enabled.
 */

unsigned char *
 vga_getgraphmem(void)
{

    DTP(("getgraphmem\n"));

    if (__svgalib_modeinfo_linearset & LINEAR_MODE )
	return __svgalib_linearframebuffer;
    return GM;
}

/*
 * This function is to be called after a SVGA graphics mode set
 * in banked mode. Probing in VGA-compatible textmode is not a good
 * idea.
 */

/* cf. vga_waitretrace, M.Weller */
int vga_setlinearaddressing(void)
{
    int (*lfn) (int op, int param) = __svgalib_driverspecs->linear;
    vga_modeinfo *modeinfo;

    printf("Setlinearaddressing\n");

    modeinfo = vga_getmodeinfo(CM);
    if (!(modeinfo->flags&CAPABLE_LINEAR)) return -1;

    (*lfn) (LINEAR_ENABLE, 0);
    __svgalib_linearframebuffer = LINEAR_POINTER;

    if ((long) __svgalib_linearframebuffer == -1) {
	/* Shouldn't happen. */
	(*lfn) (LINEAR_DISABLE, 0);
	return -1;
    }
    
    __svgalib_modeinfo_linearset |= IS_LINEAR | LINEAR_MODE;

    graph_mem = LINEAR_POINTER;

    return __svgalib_linear_mem_size; /* Who cares? */
}
