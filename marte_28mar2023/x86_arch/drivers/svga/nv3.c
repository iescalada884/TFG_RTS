/*
Riva 128 driver - Matan Ziv-Av matan@svgalib.org
please report problems to me,

This driver is based on the XFREE86 nv3 driver, developed by
David J. Mckay.

I used the file cirrus.c in this directory as a skeleton.

there are still the following problems:
  * no 24bit modes. (maybe the hardware does not support)
  * pageflipping (in threeDKit) does not work. 
  * no acceleration (is there a program that uses it anyway?).
*/

#include <stdlib.h>
#include <stdio.h>		/* for printf */
#include <string.h>		/* for memset */
//#include <sys/mman.h>		//SHARK
#include <fcntl.h>
#include <math.h>
#include <unistd.h>
#include "vga.h"
#include "libvga.h"
#include "driver.h"

/* New style driver interface. */
#include "timing.h"
#include "vgaregs.h"
#include "interface.h"
#include "nv3ref.h"
#include "nvreg.h"
#include "vgapci.h"

#define REG(i) (VGA_TOTAL_REGS+i)
#define NV3_TOTAL_REGS (VGA_TOTAL_REGS + 24 + 40 )

#define P_MIN 0

#define SetBitField(value,from,to) SetBF(to,GetBF(value,from))
#define SetBit(n) (1<<(n))
#define Set8Bits(value) ((value)&0xff)

#define GET15BITCOLOR(r,g,b) ((((r)&0xf8)<<7)|(((g)&0xf8)<<2)|((b)>>3))

/*<MaRTE OS*/
/*#include "nv3io.c"*/

static int __svgalib_nv3_inmisc(void)
{
   return v_readb(NV_PVGA0+MIS_R);
}

static void __svgalib_nv3_outmisc(int i)
{
   v_writeb(i, NV_PVGA0+MIS_W);
}

static int __svgalib_nv3_incrtc(int i)
{
   v_writeb(i, NV_PVGA1+__svgalib_CRT_I);
   return v_readb(NV_PVGA1+__svgalib_CRT_D);
}

static void __svgalib_nv3_outcrtc(int i, int d)
{
   v_writeb(i, NV_PVGA1+__svgalib_CRT_I);
   v_writeb(d, NV_PVGA1+__svgalib_CRT_D);
}

static int __svgalib_nv3_inseq(int index)
{
   v_writeb(index, NV_PVGA0+SEQ_I);
   return v_readb(NV_PVGA0+SEQ_D);
}

static void __svgalib_nv3_outseq(int index, int val)
{
   v_writeb(index, NV_PVGA0+SEQ_I);
   v_writeb(val, NV_PVGA0+SEQ_D);
}

static int __svgalib_nv3_ingra(int index)
{
   v_writeb(index, NV_PVGA0+GRA_I);
   return v_readb(NV_PVGA0+GRA_D);
}

static void __svgalib_nv3_outgra(int index, int val)
{
   v_writeb(index, NV_PVGA0+GRA_I);
   v_writeb(val, NV_PVGA0+GRA_D);
}

static int __svgalib_nv3_inis1(void)
{
    return v_readb(NV_PVGA1+__svgalib_IS1_R);
}

static int __svgalib_nv3_inatt(int index)
{
    __svgalib_nv3_inis1();
    v_writeb(index, NV_PVGA1+ATT_IW);
    return v_readb(NV_PVGA1+ATT_R);
}

static void __svgalib_nv3_outatt(int index, int val)
{
    __svgalib_nv3_inis1();
    v_writeb(index, NV_PVGA1+ATT_IW);
    v_writeb(val, NV_PVGA1+ATT_IW);
}

static void __svgalib_nv3_attscreen(int i)
{
    __svgalib_nv3_inis1();
    v_writeb(i, NV_PVGA1+ATT_IW);
}

static void __svgalib_nv3_inpal(int i, int *r, int *g, int *b)
{
    v_writeb(i, NV_PVGA2+PEL_IR);
    *r=v_readb(NV_PVGA2+PEL_D);
    *g=v_readb(NV_PVGA2+PEL_D);
    *b=v_readb(NV_PVGA2+PEL_D);
}

static void __svgalib_nv3_outpal(int i, int r, int g, int b)
{
    v_writeb(i, NV_PVGA2+PEL_IW);
    v_writeb(r, NV_PVGA2+PEL_D);
    v_writeb(g, NV_PVGA2+PEL_D);
    v_writeb(b, NV_PVGA2+PEL_D);
}

static void nv3_mapio(void)
{
    __svgalib_inmisc=__svgalib_nv3_inmisc;
    __svgalib_outmisc=__svgalib_nv3_outmisc;
    __svgalib_incrtc=__svgalib_nv3_incrtc;
    __svgalib_outcrtc=__svgalib_nv3_outcrtc;
    __svgalib_inseq=__svgalib_nv3_inseq;
    __svgalib_outseq=__svgalib_nv3_outseq;
    __svgalib_ingra=__svgalib_nv3_ingra;
    __svgalib_outgra=__svgalib_nv3_outgra;
    __svgalib_inatt=__svgalib_nv3_inatt;
    __svgalib_outatt=__svgalib_nv3_outatt;
    __svgalib_attscreen=__svgalib_nv3_attscreen;
    __svgalib_inis1=__svgalib_nv3_inis1;
    __svgalib_inpal=__svgalib_nv3_inpal;
    __svgalib_outpal=__svgalib_nv3_outpal;
}

/*MaRTE OS>*/











static int nv3_init(int, int, int);
static void nv3_unlock(void);

static int memory, chip, nvnum;
static int nv3_is_linear;
static CardSpecs *cardspecs;
static int PLL_INPUT_FREQ, MAXVCLOCK, M_MIN, M_MAX, P_MAX;

static unsigned long MMIOBASE, LINEARBASE;

enum {
   Riva128 = 0, RivaTNT, GEFORCE
};

static void nv3_setpage(int page)
{
__svgalib_outcrtc(0x1d,page << 1);
__svgalib_outcrtc(0x1e,page << 1);
}

static int __svgalib_nv3_inlinearmode(void)
{
return nv3_is_linear;
}

/* Fill in chipset specific mode information */

static void nv3_getmodeinfo(int mode, vga_modeinfo *modeinfo)
{
   
   if(modeinfo->colors==16)return;

   modeinfo->maxpixels = memory*1024/modeinfo->bytesperpixel;
   modeinfo->maxlogicalwidth = 4088;
   modeinfo->startaddressrange = memory * 1024 - 1;
   modeinfo->haveblit = 0;
   modeinfo->flags &= ~HAVE_RWPAGE;

   if (modeinfo->bytesperpixel >= 1) {
	modeinfo->flags |= CAPABLE_LINEAR;
        if (__svgalib_nv3_inlinearmode())
	    modeinfo->flags |= IS_LINEAR | LINEAR_MODE;
   }
}

/* Read and save chipset-specific registers */

static int nv3_saveregs(unsigned char regs[])
{
    int i;
    nv3_unlock();		/* May be locked again by other programs (e.g. X) */

    regs[REG(0)] = __svgalib_incrtc(NV_PCRTC_REPAINT0);
    regs[REG(1)] = __svgalib_incrtc(NV_PCRTC_REPAINT1);
    regs[REG(2)] = __svgalib_incrtc(NV_PCRTC_EXTRA);
    regs[REG(3)] = __svgalib_incrtc(NV_PCRTC_PIXEL);
    regs[REG(4)] = __svgalib_incrtc(NV_PCRTC_HORIZ_EXTRA);
    regs[REG(5)] = __svgalib_incrtc(NV_PCRTC_FIFO_CONTROL);
    regs[REG(6)] = __svgalib_incrtc(NV_PCRTC_FIFO);
    regs[REG(7)] = __svgalib_incrtc(NV_PCRTC_SCREEN);

    *(uint32_t *)(regs+REG(8)) = v_readl(NV_PFB_CONFIG_0); 
    *(uint32_t *)(regs+REG(12)) = v_readl(NV_PRAMDAC_VPLL_COEFF); 
    *(uint32_t *)(regs+REG(16)) = v_readl(NV_PRAMDAC_PLL_COEFF_SELECT); 
    *(uint32_t *)(regs+REG(20)) = v_readl(NV_PRAMDAC_GENERAL_CONTROL); 
    *(uint32_t *)(regs+REG(56)) = v_readl(NV_PRAMDAC_GRCURSOR_START_POS); 
    for(i=0x18;i<0x3A;i++)regs[84+i-0x18]=__svgalib_incrtc(i);
    
    return NV3_TOTAL_REGS - VGA_TOTAL_REGS;
}

/* Set chipset-specific registers */

static void nv3_setregs(const unsigned char regs[], int mode)
{ 
    nv3_unlock();		/* May be locked again by other programs (eg. X) */

   __svgalib_outcrtc(NV_PCRTC_REPAINT0,regs[REG(0)]);
   __svgalib_outcrtc(NV_PCRTC_REPAINT1,regs[REG(1)]);
   __svgalib_outcrtc(NV_PCRTC_EXTRA,regs[REG(2)]);
   __svgalib_outcrtc(NV_PCRTC_PIXEL,regs[REG(3)]);
   __svgalib_outcrtc(NV_PCRTC_HORIZ_EXTRA,regs[REG(4)]); 
   __svgalib_outcrtc(NV_PCRTC_FIFO_CONTROL,regs[REG(5)]); 
   __svgalib_outcrtc(NV_PCRTC_FIFO,regs[REG(6)]); 
   if(chip>= GEFORCE)
       __svgalib_outcrtc(NV_PCRTC_SCREEN,regs[REG(7)]); 

   __svgalib_outcrtc(0x1c,regs[88]); /* this enables banking at 0xa0000 */
   __svgalib_outcrtc(0x1d,regs[89]); 
   __svgalib_outcrtc(0x1e,regs[90]); 
   __svgalib_outcrtc(0x30,regs[108]); 
   __svgalib_outcrtc(0x31,regs[109]); 
   __svgalib_outcrtc(0x39,regs[117]); 

   v_writel(*(uint32_t *)(regs+REG(8)), NV_PFB_CONFIG_0);

   v_writel(*(uint32_t *)(regs+REG(12)), NV_PRAMDAC_VPLL_COEFF);
   v_writel(*(uint32_t *)(regs+REG(16)), NV_PRAMDAC_PLL_COEFF_SELECT);
   v_writel(*(uint32_t *)(regs+REG(20)), NV_PRAMDAC_GENERAL_CONTROL);
   v_writel(*(uint32_t *)(regs+REG(56)), NV_PRAMDAC_GRCURSOR_START_POS);
}


/* Return nonzero if mode is available */

static int nv3_modeavailable(int mode)
{
    struct info *info;
    ModeTiming *modetiming;
    ModeInfo *modeinfo;

    printf("Test Mode...\n");
    
    if (IS_IN_STANDARD_VGA_DRIVER(mode))
	return __svgalib_vga_driverspecs.modeavailable(mode);

    info = &__svgalib_infotable[mode];
    if (memory * 1024 < info->ydim * info->xbytes)
	return 0;
    
    modeinfo = __svgalib_createModeInfoStructureForSvgalibMode(mode);

    if(modeinfo->bitsPerPixel==24) {
        free(modeinfo);
        return 0;   
    }

    printf("Test Timing...\n");
    
    modetiming = malloc(sizeof(ModeTiming));
    
    if (__svgalib_getmodetiming(modetiming, modeinfo, cardspecs)) {
        printf("Test Mode Failed !\n");	
	free(modetiming);
	free(modeinfo);
	return 0;
    }

    free(modetiming);
    free(modeinfo);
    return SVGADRV;
}

static int CalcVClock(int clockIn,int *clockOut,int *mOut,
                                         int *nOut,int *pOut);

static int CalculateCRTC(ModeTiming *mode, ModeInfo *modeinfo, unsigned char *moderegs)
{
  int bpp=modeinfo->bitsPerPixel/8,
      horizDisplay    = (mode->CrtcHDisplay/8)   - 1,
      horizStart      = (mode->CrtcHSyncStart/8) - 0,
      horizEnd        = (mode->CrtcHSyncEnd/8)   - 0,
      horizTotal      = (mode->CrtcHTotal/8)	 - 5,
      horizBlankStart = (mode->CrtcHDisplay/8)   - 1,
      horizBlankEnd   = (mode->CrtcHTotal/8)     - 1,
      vertDisplay     = mode->CrtcVDisplay       - 1,
      vertStart       = mode->CrtcVSyncStart	 - 1,
      vertEnd         = mode->CrtcVSyncEnd       - 1,
      vertTotal       = mode->CrtcVTotal         - 2,
      vertBlankStart  =  mode->CrtcVDisplay      - 1,
      vertBlankEnd    =  mode->CrtcVTotal        - 1;

  if(mode->flags & INTERLACED) vertTotal |= 1;

  /* Calculate correct value for offset register */
  moderegs[0x13]=((modeinfo->width/8)*bpp)&0xff;

  /* Extra bits for CRTC offset register */
  moderegs[REG(0)]=
    SetBitField((modeinfo->width/8)*bpp,10:8,7:5);

  /* The NV3 manuals states that for native modes, there should be no 
   * borders. This code should also be tidied up to use symbolic names
   */     
  moderegs[0x0]=Set8Bits(horizTotal);
  moderegs[0x1]=Set8Bits(horizDisplay);
  moderegs[0x2]=Set8Bits(horizBlankStart);
  moderegs[0x3]=SetBitField(horizBlankEnd,4:0,4:0) | SetBit(7);
  moderegs[0x4]=Set8Bits(horizStart);
  moderegs[0x5]=SetBitField(horizBlankEnd,5:5,7:7)|
                     SetBitField(horizEnd,4:0,4:0);
  moderegs[0x6]=SetBitField(vertTotal,7:0,7:0);

  moderegs[0x7]=SetBitField(vertTotal,8:8,0:0)|
		     SetBitField(vertDisplay,8:8,1:1)|
		     SetBitField(vertStart,8:8,2:2)|
		     SetBitField(vertBlankStart,8:8,3:3)|
		     SetBit(4)|
		     SetBitField(vertTotal,9:9,5:5)|
		     SetBitField(vertDisplay,9:9,6:6)|
		     SetBitField(vertStart,9:9,7:7);

  moderegs[0x9]= SetBitField(vertBlankStart,9:9,5:5) | SetBit(6);
  moderegs[0x10]= Set8Bits(vertStart);
  moderegs[0x11]= SetBitField(vertEnd,3:0,3:0) | SetBit(5);
  moderegs[0x12]= Set8Bits(vertDisplay);
  moderegs[0x15]= Set8Bits(vertBlankStart);
  moderegs[0x16]= Set8Bits(vertBlankEnd);

 moderegs[REG(2)]  = SetBitField(horizBlankEnd,6:6,4:4)
                  | SetBitField(vertBlankStart,10:10,3:3)
                  | SetBitField(vertStart,10:10,2:2)
                  | SetBitField(vertDisplay,10:10,1:1)
                  | SetBitField(vertTotal,10:10,0:0);

 moderegs[REG(4)] = SetBitField(horizTotal,8:8,0:0) 
                  | SetBitField(horizDisplay,8:8,1:1)
                  | SetBitField(horizBlankStart,8:8,2:2)
                  | SetBitField(horizStart,8:8,3:3);

 moderegs[REG(7)] = SetBitField(vertTotal,11:11,0:0)
                    | SetBitField(vertDisplay,11:11,2:2)
                    | SetBitField(vertStart,11:11,4:4)
                    | SetBitField(vertBlankStart,11:11,6:6);

  if(mode->flags & DOUBLESCAN) moderegs[0x9]|=0x80;
 
  /* I think this should be SetBitField(horizTotal,8:8,0:0), but this
   * doesn't work apparently. Why 260 ? 256 would make sense.
   */
  if(mode->flags & INTERLACED) {
      horizTotal=(horizTotal>>1)& ~1;
      moderegs[117]=horizTotal & 0xff;      
      moderegs[REG(4)] |= SetBitField(horizTotal,8:8,4:4);
  } else {
      moderegs[117]=255;
  }

  return 1;
}

/* Set a mode */

/* Local, called by nv3_setmode(). */

static void nv3_initializemode(unsigned char *moderegs,
			    ModeTiming * modetiming, ModeInfo * modeinfo, int mode)
{ 
    long k;
    unsigned int config0=0;
    
    int m,n,p;
    int clockIn=modetiming->pixelClock;
    int clockOut;
    int pixelDepth;
  
    nv3_saveregs(moderegs);

    __svgalib_setup_VGA_registers(moderegs, modetiming, modeinfo);

    CalcVClock(clockIn,&clockOut,&m,&n,&p);

    *(unsigned int *)(moderegs+REG(12)) = PRAMDAC_Val(VPLL_COEFF_NDIV,n) | 
        				  PRAMDAC_Val(VPLL_COEFF_MDIV,m) | 
                                          PRAMDAC_Val(VPLL_COEFF_PDIV,p);

    CalculateCRTC(modetiming,modeinfo,moderegs);

    moderegs[REG(1)] = PCRTC_Val(REPAINT1_LARGE_SCREEN,modetiming->CrtcHDisplay<1280);

/* The new xfree driver (from nVidia) calculates those in some
   twisted way, but I leave it for now */
    moderegs[REG(5)]=0x83;
    if(modetiming->pixelClock*modeinfo->bytesPerPixel>720000) {
        moderegs[REG(6)]=0x2f;
    }

  /* PixelFormat controls how many bits per pixel. 
   * There is another register in the 
   * DAC which controls if mode is 5:5:5 or 5:6:5
   */
    pixelDepth=(modeinfo->bitsPerPixel+1)/8;
    if(pixelDepth>3) pixelDepth=3;
    moderegs[REG(3)]=pixelDepth;
    if(modetiming->flags & TVMODE) {
        moderegs[REG(3)]|=0x80;
        if(modetiming->flags & TVPAL) moderegs[REG(3)]|=0x40;
    };

    *(unsigned int *)(moderegs+REG(20))=
        PRAMDAC_Def(GENERAL_CONTROL_IDC_MODE,GAMMA)|
        PRAMDAC_Val(GENERAL_CONTROL_565_MODE,modeinfo->greenWeight==6)|
        PRAMDAC_Def(GENERAL_CONTROL_TERMINATION,37OHM)|
        ((modeinfo->bitsPerPixel>8) ? 
            PRAMDAC_Def(GENERAL_CONTROL_BPC,8BITS) : 
            PRAMDAC_Def(GENERAL_CONTROL_BPC,6BITS)) | PRAMDAC_Def(GENERAL_CONTROL_VGA_STATE,SEL); 
            /* Not sure about this */

    switch(chip){
      case Riva128:
          config0=PFB_Val(CONFIG_0_RESOLUTION,((modeinfo->lineWidth+31)/32))|
                  PFB_Val(CONFIG_0_PIXEL_DEPTH,pixelDepth)|
                  PFB_Def(CONFIG_0_TILING,DISABLED); 
          k=PRAMDAC_Def(PLL_COEFF_SELECT_MPLL_SOURCE,PROG)|
            PRAMDAC_Def(PLL_COEFF_SELECT_VPLL_SOURCE,PROG)|
            PRAMDAC_Def(PLL_COEFF_SELECT_VCLK_RATIO,DB2);
          moderegs[REG(1)] |= PCRTC_Def(REPAINT1_PALETTE_WIDTH,6BITS);
          break;
      case RivaTNT:
          config0=0x1114;
          k=0x10000700;
          break;
      case GEFORCE:
      default:
          config0=*(unsigned int *)(moderegs+REG(8));
          k=0x10000700;
          break;
   };

   *(unsigned int *)(moderegs+REG(16))=k;
   *(unsigned int *)(moderegs+REG(8))=config0;

   moderegs[88]=28;
   moderegs[109]&=0xfe; /* hide cursor */

   nv3_is_linear=0;

return ;
}


int nv3_setmode(int mode, int prv_mode)
{
    unsigned char *moderegs;
    ModeTiming *modetiming;
    ModeInfo *modeinfo;
    int i;

    if (IS_IN_STANDARD_VGA_DRIVER(mode)) {
       
	unsigned int k;
	    
	printf("Standard VGA Mode\n"); 

	if(chip==Riva128)
            v_writel(0x00000100, NV_PRAMDAC_PLL_COEFF_SELECT);
            else v_writel(0x00000500, NV_PRAMDAC_PLL_COEFF_SELECT);
        __svgalib_outcrtc(NV_PCRTC_REPAINT0,0);
        __svgalib_outcrtc(NV_PCRTC_REPAINT1,0x3d);
        __svgalib_outcrtc(NV_PCRTC_EXTRA,0);
        __svgalib_outcrtc(NV_PCRTC_PIXEL,0);
        __svgalib_outcrtc(NV_PCRTC_HORIZ_EXTRA,0);
        __svgalib_outcrtc(NV_PCRTC_FIFO_CONTROL,0x83);
        __svgalib_outcrtc(0x1c,0x18);
        __svgalib_outcrtc(0x1d,0);
        __svgalib_outcrtc(0x1e,0);
        __svgalib_outcrtc(0x30,0);
        __svgalib_outcrtc(0x31,0);
        k =  v_readl(NV_PRAMDAC_GENERAL_CONTROL);
        k &= ~0x00100000;
        v_writel(k, NV_PRAMDAC_GENERAL_CONTROL);

	return __svgalib_vga_driverspecs.setmode(mode, prv_mode);
    }

    if (!nv3_modeavailable(mode))
	return 1;

    printf("Mode Available !\nStart Set Mode...\n");
    
    modeinfo = __svgalib_createModeInfoStructureForSvgalibMode(mode);

    modetiming = malloc(sizeof(ModeTiming));
    if (__svgalib_getmodetiming(modetiming, modeinfo, cardspecs)) {
	free(modetiming);
	free(modeinfo);
	return 1;
    }

    moderegs = malloc(NV3_TOTAL_REGS);

    printf("Start Init Mode...\n");
    
    nv3_initializemode(moderegs, modetiming, modeinfo, mode);
    free(modetiming);

    __svgalib_setregs(moderegs);	/* Set standard regs. */
    nv3_setregs(moderegs, mode);	/* Set extended regs. */
    free(moderegs);

    __svgalib_InitializeAcceleratorInterface(modeinfo);

    for(i=0;i<256;i++)vga_setpalette(i,i,i,i);
    free(modeinfo);
    return 0;
}

/* Unlock chipset-specific registers */

static void nv3_unlock(void)
{
    if(chip!=Riva128) {
        __svgalib_nv3_outcrtc(0x11,__svgalib_incrtc(0x11)&0x7f);
        __svgalib_nv3_outcrtc(0x1f, UNLOCK_EXT_MAGIC);
    } else {
//        __svgalib_outcrtc(0x11,__svgalib_incrtc(0x11)&0x7f);
        __svgalib_outseq(LOCK_EXT_INDEX,UNLOCK_EXT_MAGIC);    
    }
}

/* Relock chipset-specific registers */
/* (currently not used) */

static void nv3_lock(void)
{
    __svgalib_outseq(LOCK_EXT_INDEX,UNLOCK_EXT_MAGIC+1);    

}

/* Indentify chipset, initialize and return non-zero if detected */

int nv3_test(void)
{  
   MMIOBASE=0; /* let nv3_init() find those */
   LINEARBASE=0;
   nv3_init(0,0,0);
   return 1;
}

/* No r/w paging - I guess it's possible, but is it useful? */
static void nv3_setrdpage(int page)
{
}
static void nv3_setwrpage(int page)
{
}

/* Set display start address (not for 16 color modes) */

static void nv3_setdisplaystart(int address)
{  unsigned char byte;
    int pan;

  pan=(address&3)<<1;
  address=address >> 2;
  __svgalib_outcrtc(0x0d,address&0xff);
  __svgalib_outcrtc(0x0c,(address>>8)&0xff);
  byte=__svgalib_incrtc(NV_PCRTC_REPAINT0) & 0xe0;
  __svgalib_outcrtc(NV_PCRTC_REPAINT0,((address>>16)&0x1f)|byte);
  byte=__svgalib_incrtc(0x2D) & ~0x60;
  __svgalib_outcrtc(0x2D,((address>>16)&0x60)|byte);

  byte = __svgalib_inis1();
  __svgalib_outatt(0x13, pan);
}

/* Set logical scanline length (usually multiple of 8) */

static void nv3_setlogicalwidth(int width)
{  int byte ;

  __svgalib_outcrtc(0x13,(width >> 3)&0xff);
  byte=__svgalib_incrtc(NV_PCRTC_REPAINT0) & 0x1f;
  __svgalib_outcrtc(NV_PCRTC_REPAINT0,SetBitField(width,13:11,7:5)|byte);

}

static int nv3_linear(int op, int param)
{
if (op==LINEAR_ENABLE){ nv3_is_linear=1; return 0;}
if (op==LINEAR_DISABLE){ nv3_is_linear=0; return 0;}
if (op==LINEAR_QUERY_BASE) { return LINEARBASE ;}
if (op == LINEAR_QUERY_RANGE || op == LINEAR_QUERY_GRANULARITY) return 0;		/* No granularity or range. */
    else return -1;		/* Unknown function. */
}

static int nv3_cursor( int cmd, int p1, int p2, int p3, int p4, void *p5) {
    unsigned char *b1, *b2;
    unsigned short *b3;
    unsigned int i, j, k, l, c0, c1;
    
    switch(cmd){
        case CURSOR_INIT:
            return 1;
        case CURSOR_HIDE:
            __svgalib_outcrtc(0x31,__svgalib_incrtc(0x31)&0xfe); /* disable cursor */
            break;
        case CURSOR_SHOW:
            __svgalib_outcrtc(0x31,__svgalib_incrtc(0x31)|1); /* enable cursor */
            break;
        case CURSOR_POSITION:
            v_writel(p1+(p2<<16), NV_PRAMDAC_GRCURSOR_START_POS);
            break;
        case CURSOR_SELECT:
            i=memory/2-(p1+1);
            if (chip==Riva128) {
                __svgalib_outcrtc(0x31,(__svgalib_incrtc(0x31)&7)|(((~i)&0x1f)<<3));
            } else {
                __svgalib_outcrtc(0x31,(__svgalib_incrtc(0x31)&3)|(((~i)&0x3f)<<2));
            }
#if 0
            __svgalib_outcrtc(0x30,((~i)&0x3fc0)>>6); 
#else
            __svgalib_outcrtc(0x30,0); 
#endif
            break;
        case CURSOR_IMAGE:
            i=memory/2-(p1+1);
            i=i*2048;
            switch(p2) {
                case 0: /* X11 format, 32x32 */
                    b3=malloc(2048);
                    b1=(unsigned char *)p5;
                    b2=b1+128;
                    c0=0x8000|GET15BITCOLOR((p3>>16)&0xff,(p3>>8)&0xff,p3&0xff);
                    c1=0x8000|GET15BITCOLOR((p4>>16)&0xff,(p4>>8)&0xff,p4&0xff);
                    l=992;
                    for(k=0;k<128;k++) {
                        int cc1=*(b1+k);
                        int cc2=*(b2+k);
                        for(j=0;j<8;j++) {
                            if(!(cc2&0x80)) *(b3+l)=0; else if (cc1&0x80)
                                *(b3+l)=c1; else *(b3+l)=c0;
                            l++;
                            if((l&0x1f)==0)l-=64;
                            cc2<<=1;
                            cc1<<=1;
                        }
                    }
                    memcpy(LINEAR_POINTER+i,b3,2048);
                    free(b3);
                    break;
                case 1: /* nvidia 1555 format  32x32 */
                    memcpy(LINEAR_POINTER+i,p5,2048);
                    break;
            }
            break;
    }
    return 0;
}       

static int nv3_match_programmable_clock(int clock)
{
return clock ;
}
static int nv3_map_clock(int bpp, int clock)
{
return clock ;
}
static int nv3_map_horizontal_crtc(int bpp, int pixelclock, int htiming)
{
return htiming;
}
/* Function table (exported) */

DriverSpecs __svgalib_nv3_driverspecs =
{
    nv3_saveregs,
    nv3_setregs,
    nv3_unlock,
    nv3_lock,
    nv3_test,
    nv3_init,
    nv3_setpage,
    nv3_setrdpage,
    nv3_setwrpage,
    nv3_setmode,
    nv3_modeavailable,
    nv3_setdisplaystart,
    nv3_setlogicalwidth,
    nv3_getmodeinfo,
    0,				/* old blit funcs */
    0,
    0,
    0,
    0,
    0,				/* ext_set */
    0,				/* accel */
    nv3_linear,
    0,				/* accelspecs, filled in during init. */
    0,
    nv3_cursor
};

/* Initialize chipset (called after detection) */

static int nv3_init(int force, int par1, int par2) //SHARK
{   
    char *archs[3]={"Riva128",
                    "RivaTNT", 
                    "GeForce"};
    int flags;
    
    flags=0;
    if(MMIOBASE==0) {
       unsigned long buf[64];
       int found;
      
       printf("Init NVidia card: scan pci bus\n");
       
       found=__svgalib_pci_find_vendor_vga(0x12d2,buf,0);
       if (found) {
          found=__svgalib_pci_find_vendor_vga(0x10de,buf,0);
          if(found) {
	    printf("Error: NVidia card not found\n");
	    return 0;
	  }
       } 
      
       switch((buf[0]>>20)&0xff){
          case 0x1: 
             chip=Riva128; 
             nvnum=3;
             break;
          case 0x2:
          case 0xA:
             chip=RivaTNT;
             switch((buf[0]>>16)&0xff){
                 case 0x20:
                     nvnum=4;
                     break;
                 case 0x2c:
                 case 0x2d:
                 case 0x2e:
                 case 0x2f:
                     nvnum=6;
                     break;
                 default:
                     nvnum=5;
             }
             break;
          case 0x10:
          case 0x15:
             chip=GEFORCE; 
             nvnum=(buf[0]>>20)&0xff;
             break;
          case 0x11:
          case 0x17:
          case 0x1A:
          case 0x20:
          case 0x25:
          default:
             flags = NO_INTERLACE;
             chip=GEFORCE; 
             nvnum=(buf[0]>>20)&0xff;
             break;
       };
       MMIOBASE=buf[4]&0xffffff00;
       LINEARBASE=buf[5]&0xffffff00;
    };
    
    printf("NVidia card found ! BASE_ADDR: %.8lx TYPE: %s (NV%.8x)\n",MMIOBASE,archs[chip],nvnum);     
    if (force) {
	memory = par1;
	chip = par2;
    };

    __svgalib_modeinfo_linearset |= IS_LINEAR;

    __svgalib_mmio_base=MMIOBASE;
    __svgalib_mmio_size=8*1024*1024;

    map_mmio();
    
    if(!force){
       int boot0;
       
       boot0=v_readl(NV_PFB_BOOT_0);
       switch(chip){
          case Riva128:
                 if(boot0&0x20)memory=8192; else memory=1024<<(boot0&3); 
                 if(memory==1024)memory=8192;
                 break;
          case RivaTNT: 
                 memory=2048<<(boot0&3); 
                 if(memory==2048)memory=32768;
                 break;
          case GEFORCE:
              	 memory=(v_readl(NV_PFB_BOOT_10)>>10) & 0x3fc00;
             	 if(memory<8192)memory=8192; // do this later
                 break;
       }
    }

    nv3_mapio();
    nv3_unlock();

    {
       int temp;
       
       temp=v_readl(NV_PEXTDEV_0);
       switch(chip){
          case Riva128:
             PLL_INPUT_FREQ= (temp&0x20) ? 14318 : 13500;
             MAXVCLOCK=256000;
             P_MAX=4;// XFree say 3, but 4 works on my Riva128 
             if(PLL_INPUT_FREQ==13500)M_MAX=12; else M_MAX=13;
             M_MIN=M_MAX-5;
             break;
          case RivaTNT:
             PLL_INPUT_FREQ= (temp&0x40) ? 14318 : 13500;
             MAXVCLOCK=350000;
             P_MAX=4;
             if(PLL_INPUT_FREQ==13500)M_MAX=13; else M_MAX=14;
             M_MIN=M_MAX-6;
             break;
          case GEFORCE:
          default:
             PLL_INPUT_FREQ= (temp&0x40  ) ? 14318 : 13500;
             if(nvnum==0x17 ||  nvnum==0x25) {
                 if(temp&0x400000) PLL_INPUT_FREQ=27000;
             }
             MAXVCLOCK=350000;
             P_MAX=4;
             if(PLL_INPUT_FREQ==13500)M_MAX=13; else M_MAX=14;
             M_MIN=M_MAX-6;
             break;
       };
    };

    cardspecs = malloc(sizeof(CardSpecs));
    cardspecs->videoMemory = memory;
    if(chip==Riva128) {
       cardspecs->maxPixelClock4bpp = 75000;	
       cardspecs->maxPixelClock8bpp = 230000;	
       cardspecs->maxPixelClock16bpp = 230000;	
       cardspecs->maxPixelClock24bpp = 0;
       cardspecs->maxPixelClock32bpp = 230000;
    } else {
       cardspecs->maxPixelClock4bpp = 75000;	
       cardspecs->maxPixelClock8bpp = 350000;	
       cardspecs->maxPixelClock16bpp = 350000;	
       cardspecs->maxPixelClock24bpp = 0;
       cardspecs->maxPixelClock32bpp = 350000;    
    }
    cardspecs->flags = flags | CLOCK_PROGRAMMABLE ;
    cardspecs->maxHorizontalCrtc = 4080;
    cardspecs->maxPixelClock4bpp = 0;
    cardspecs->nClocks =0;
    cardspecs->clocks = NULL;
    cardspecs->mapClock = nv3_map_clock;
    cardspecs->mapHorizontalCrtc = nv3_map_horizontal_crtc;
    cardspecs->matchProgrammableClock=nv3_match_programmable_clock;
    __svgalib_driverspecs = &__svgalib_nv3_driverspecs;
    
    __svgalib_banked_mem_base=0xa0000;
    __svgalib_banked_mem_size=0x10000;
    __svgalib_linear_mem_base=LINEARBASE;
    __svgalib_linear_mem_size=memory*0x400;
    
    printf("NVidia Card Initialized ! LIN_ADDR:%.8lxLIN_SIZE:%ld\n",__svgalib_linear_mem_base,__svgalib_linear_mem_size);          
    sleep(4);
    return 0;
    
}

/*
 * Calculate the Video Clock parameters for the PLL.
 */
static int CalcVClock
(
    int           clockIn,
    int          *clockOut,
    int          *mOut,
    int          *nOut,
    int          *pOut
)
{
    unsigned DeltaNew, DeltaOld;
    unsigned VClk, Freq;
    unsigned M, N, P;
    
    DeltaOld = 0xFFFFFFFF;

    VClk     = (unsigned)clockIn;
    
    for (P = 0; P <= P_MAX; P ++)
    {
        Freq = VClk << P;
        if ((Freq >= 128000) && (Freq <= MAXVCLOCK))
        {
            for (M = M_MIN; M <= M_MAX; M++)
            {
                N    = ((VClk<<P) * M + PLL_INPUT_FREQ/2) / PLL_INPUT_FREQ;
                Freq = (PLL_INPUT_FREQ * N / M) >> P;
                if (Freq > VClk)
                    DeltaNew = Freq - VClk;
                else
                    DeltaNew = VClk - Freq;
                if ((DeltaNew < DeltaOld) && (N<256))
                {
                    *mOut     = M;
                    *nOut     = N;
                    *pOut     = P;
                    *clockOut = Freq;
                    DeltaOld  = DeltaNew;
                }
            }
        }
    }
    return (DeltaOld != 0xFFFFFFFF);
}

