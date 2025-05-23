/* $XConsortium: nvreg.h /main/2 1996/10/28 05:13:41 kaleb $ */
/*
 * Copyright 1996-1997  David J. McKay
 *
 * Permission is hereby granted, free of charge, to any person obtaining a 
 * copy of this software and associated documentation files (the "Software"), 
 * to deal in the Software without restriction, including without limitation 
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, 
 * and/or sell copies of the Software, and to permit persons to whom the 
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
 * DAVID J. MCKAY BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF 
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
 * SOFTWARE.
 */

/* $XFree86: xc/programs/Xserver/hw/xfree86/vga256/drivers/nv/nvreg.h,v 3.2.2.1 1998/01/18 10:35:36 hohndel Exp $ */

#ifndef __NVREG_H_
#define __NVREG_H_


/* Little macro to construct bitmask for contiguous ranges of bits */
#define BITMASK(t,b) (((unsigned)(1U << (((t)-(b)+1)))-1)  << (b))
#define MASKEXPAND(mask) BITMASK(1?mask,0?mask)

/* Macro to set specific bitfields (mask has to be a macro x:y) ! */
#define SetBF(mask,value) ((value) << (0?mask))
#define GetBF(var,mask) (((unsigned)((var) & MASKEXPAND(mask))) >> (0?mask) )

#define MaskAndSetBF(var,mask,value) (var)=(((var)&(~MASKEXPAND(mask)) \
                                             | SetBF(mask,value)))

#define DEVICE_BASE(device) (0?NV##_##device)
#define DEVICE_SIZE(device) ((1?NV##_##device) - DEVICE_BASE(device)+1)

/* This is where we will have to have conditional compilation */
#define DEVICE_ACCESS(device,reg) \
  nv##device##Port[((NV_##device##_##reg)-DEVICE_BASE(device))/4]

#define DEVICE_WRITE(device,reg,value) DEVICE_ACCESS(device,reg)=(value)
#define DEVICE_READ(device,reg)        DEVICE_ACCESS(device,reg)
#define DEVICE_PRINT(device,reg) \
  ErrorF("NV_"#device"_"#reg"=#%08lx\n",DEVICE_ACCESS(device,reg))
#define DEVICE_DEF(device,mask,value) \
  SetBF(NV_##device##_##mask,NV_##device##_##mask##_##value)
#define DEVICE_VALUE(device,mask,value) SetBF(NV_##device##_##mask,value)
#define DEVICE_MASK(device,mask) MASKEXPAND(NV_##device##_##mask)

#define PDAC_Write(reg,value)           DEVICE_WRITE(PDAC,reg,value)
#define PDAC_Read(reg)                  DEVICE_READ(PDAC,reg)
#define PDAC_Print(reg)                 DEVICE_PRINT(PDAC,reg)
#define PDAC_Def(mask,value)            DEVICE_DEF(PDAC,mask,value)
#define PDAC_Val(mask,value)            DEVICE_VALUE(PDAC,mask,value)
#define PDAC_Mask(mask)                 DEVICE_MASK(PDAC,mask)

#define PFB_Write(reg,value)            DEVICE_WRITE(PFB,reg,value)
#define PFB_Read(reg)                   DEVICE_READ(PFB,reg)
#define PFB_Print(reg)                  DEVICE_PRINT(PFB,reg)
#define PFB_Def(mask,value)             DEVICE_DEF(PFB,mask,value)
#define PFB_Val(mask,value)             DEVICE_VALUE(PFB,mask,value)
#define PFB_Mask(mask)                  DEVICE_MASK(PFB,mask)

#define PRM_Write(reg,value)            DEVICE_WRITE(PRM,reg,value)
#define PRM_Read(reg)                   DEVICE_READ(PRM,reg)
#define PRM_Print(reg)                  DEVICE_PRINT(PRM,reg)
#define PRM_Def(mask,value)             DEVICE_DEF(PRM,mask,value)
#define PRM_Val(mask,value)             DEVICE_VALUE(PRM,mask,value)
#define PRM_Mask(mask)                  DEVICE_MASK(PRM,mask)

#define PGRAPH_Write(reg,value)         DEVICE_WRITE(PGRAPH,reg,value)
#define PGRAPH_Read(reg)                DEVICE_READ(PGRAPH,reg)
#define PGRAPH_Print(reg)               DEVICE_PRINT(PGRAPH,reg)
#define PGRAPH_Def(mask,value)          DEVICE_DEF(PGRAPH,mask,value)
#define PGRAPH_Val(mask,value)          DEVICE_VALUE(PGRAPH,mask,value)
#define PGRAPH_Mask(mask)               DEVICE_MASK(PGRAPH,mask)

#define PDMA_Write(reg,value)           DEVICE_WRITE(PDMA,reg,value)
#define PDMA_Read(reg)                  DEVICE_READ(PDMA,reg)
#define PDMA_Print(reg)                 DEVICE_PRINT(PDMA,reg)
#define PDMA_Def(mask,value)            DEVICE_DEF(PDMA,mask,value)
#define PDMA_Val(mask,value)            DEVICE_VALUE(PDMA,mask,value)
#define PDMA_Mask(mask)                 DEVICE_MASK(PDMA,mask)

#define PFIFO_Write(reg,value)          DEVICE_WRITE(PFIFO,reg,value)
#define PFIFO_Read(reg)                 DEVICE_READ(PFIFO,reg)
#define PFIFO_Print(reg)                DEVICE_PRINT(PFIFO,reg)
#define PFIFO_Def(mask,value)           DEVICE_DEF(PFIFO,mask,value)
#define PFIFO_Val(mask,value)           DEVICE_VALUE(PFIFO,mask,value)
#define PFIFO_Mask(mask)                DEVICE_MASK(PFIFO,mask)

#define PRAM_Write(reg,value)           DEVICE_WRITE(PRAM,reg,value)
#define PRAM_Read(reg)                  DEVICE_READ(PRAM,reg)
#define PRAM_Print(reg)                 DEVICE_PRINT(PRAM,reg)
#define PRAM_Def(mask,value)            DEVICE_DEF(PRAM,mask,value)
#define PRAM_Val(mask,value)            DEVICE_VALUE(PRAM,mask,value)
#define PRAM_Mask(mask)                 DEVICE_MASK(PRAM,mask)

#define PRAMFC_Write(reg,value)         DEVICE_WRITE(PRAMFC,reg,value)
#define PRAMFC_Read(reg)                DEVICE_READ(PRAMFC,reg)
#define PRAMFC_Print(reg)               DEVICE_PRINT(PRAMFC,reg)
#define PRAMFC_Def(mask,value)          DEVICE_DEF(PRAMFC,mask,value)
#define PRAMFC_Val(mask,value)          DEVICE_VALUE(PRAMFC,mask,value)
#define PRAMFC_Mask(mask)               DEVICE_MASK(PRAMFC,mask)

#define PMC_Write(reg,value)            DEVICE_WRITE(PMC,reg,value)
#define PMC_Read(reg)                   DEVICE_READ(PMC,reg)
#define PMC_Print(reg)                  DEVICE_PRINT(PMC,reg)
#define PMC_Def(mask,value)             DEVICE_DEF(PMC,mask,value)
#define PMC_Val(mask,value)             DEVICE_VALUE(PMC,mask,value)
#define PMC_Mask(mask)                  DEVICE_MASK(PMC,mask)

#define PMC_Write(reg,value)            DEVICE_WRITE(PMC,reg,value)
#define PMC_Read(reg)                   DEVICE_READ(PMC,reg)
#define PMC_Print(reg)                  DEVICE_PRINT(PMC,reg)
#define PMC_Def(mask,value)             DEVICE_DEF(PMC,mask,value)
#define PMC_Val(mask,value)             DEVICE_VALUE(PMC,mask,value)
#define PMC_Mask(mask)                  DEVICE_MASK(PMC,mask)


#define PBUS_Write(reg,value)         DEVICE_WRITE(PBUS,reg,value)
#define PBUS_Read(reg)                DEVICE_READ(PBUS,reg)
#define PBUS_Print(reg)               DEVICE_PRINT(PBUS,reg)
#define PBUS_Def(mask,value)          DEVICE_DEF(PBUS,mask,value)
#define PBUS_Val(mask,value)          DEVICE_VALUE(PBUS,mask,value)
#define PBUS_Mask(mask)               DEVICE_MASK(PBUS,mask)


#define PRAMDAC_Write(reg,value)         DEVICE_WRITE(PRAMDAC,reg,value)
#define PRAMDAC_Read(reg)                DEVICE_READ(PRAMDAC,reg)
#define PRAMDAC_Print(reg)               DEVICE_PRINT(PRAMDAC,reg)
#define PRAMDAC_Def(mask,value)          DEVICE_DEF(PRAMDAC,mask,value)
#define PRAMDAC_Val(mask,value)          DEVICE_VALUE(PRAMDAC,mask,value)
#define PRAMDAC_Mask(mask)               DEVICE_MASK(PRAMDAC,mask)


#define PDAC_ReadExt(reg) \
  ((PDAC_Write(INDEX_LO,(NV_PDAC_EXT_##reg) & 0xff)),\
  (PDAC_Write(INDEX_HI,((NV_PDAC_EXT_##reg) >> 8) & 0xff)),\
  (PDAC_Read(INDEX_DATA)))

#define PDAC_WriteExt(reg,value)\
  ((PDAC_Write(INDEX_LO,(NV_PDAC_EXT_##reg) & 0xff)),\
  (PDAC_Write(INDEX_HI,((NV_PDAC_EXT_##reg) >> 8) & 0xff)),\
  (PDAC_Write(INDEX_DATA,(value))))

#define CRTC_Write(index,value) outb(CRT_IC,(index));outb(CRT_DC,value)
#define CRTC_Read(index) (outb(CRT_IC,index),inb(CRT_DC))

#define PCRTC_Write(index,value) __svgalib_outcrtc(NV_PCRTC_##index,value)
#define PCRTC_Read(index) __svgalib_incrtc(NV_PCRTC_##index)

#define PCRTC_Def(mask,value)          DEVICE_DEF(PCRTC,mask,value)
#define PCRTC_Val(mask,value)          DEVICE_VALUE(PCRTC,mask,value)
#define PCRTC_Mask(mask)               DEVICE_MASK(PCRTC,mask)

#define SR_Write(index,value) outb(SEQ_I,(index));outb(SEQ_D,value)
#define SR_Read(index) (outb(SEQ_I,index),inb(SEQ_D))

#define PEXTDEV_Read(reg)                   DEVICE_READ(PEXTDEV,reg)

/* These are the variables which actually point at the register blocks */

/*typedef enum {NV1,NV3,NumNVChips} NVChipType;

NVChipType GetChipType(void);
*/

#endif


