/* SVGA Lib - Linker module to use the low level driver
 * without the vga.c support
 *
*/

#include <stdlib.h>
/* variables used to shift between monchrome and color emulation */

#include <sys/pio.h>


#include "driver.h"
#include <stdint.h>
#include <unistd.h>



//#include <ll/i386/hw-data.h>
//#include <ll/i386/mem.h>
//#include <ll/i386/cons.h>
//#include <ll/sys/ll/ll-func.h>
#include "vga.h"
#include "libvga.h"
#include "vgaversion.h"
#include "vgaio.h"
//#include <ll/i386/x-bios.h>
#include <sys/pci_ids.h>

/*MaRTE OS*/
/*Incluido para la tabla de GDT*/
//#include <oskit/x86/base_gdt.h>
//#include <debug_marte.h>


int __svgalib_CRT_I;			/* current CRT index register address */
int __svgalib_CRT_D;			/* current CRT data register address */
int __svgalib_IS1_R;			/* current input status register address */
static int color_text;			/* true if color text emulation */

uint8_t *BANKED_POINTER=NULL, *LINEAR_POINTER;
uint8_t *MMIO_POINTER;
uint8_t *SPARSE_MMIO;
static int mmio_mapped=0, mem_mapped=0;
static uint8_t *B8000_POINTER=NULL;
unsigned long __svgalib_banked_mem_base, __svgalib_banked_mem_size;
unsigned long __svgalib_mmio_base, __svgalib_mmio_size=0;
unsigned long __svgalib_linear_mem_base=0, __svgalib_linear_mem_size=0;

/*MaRTE OS*/
unsigned char *buf_rgb;

extern int init_vgapci(unsigned int vendor,unsigned int device);
extern int nv3_test(void);
extern int savage_test(void);
extern int r128_test(void);
extern int neo_test(void);
extern int vga_test(void);

/*MaRTE OS*/
//extern int vesa_test(void);

/* If == 0 then nothing is defined by the user... */
int __svgalib_default_mode = 10;

struct info infotable[] =
{
  {80, 25, 16, 160, 0},	/* VGAlib VGA modes */
  {320, 200, 16, 40, 0},
  {640, 200, 16, 80, 0},
  {640, 350, 16, 80, 0},
  {640, 480, 16, 80, 0},
  {320, 200, 256, 320, 1},
  {320, 240, 256, 80, 0},
  {320, 400, 256, 80, 0},
  {360, 480, 256, 90, 0},
  {640, 480, 2, 80, 0},
  
  {640, 480, 256, 640, 1},	/* VGAlib SVGA modes */
  {800, 600, 256, 800, 1},
  {1024, 768, 256, 1024, 1},
  {1280, 1024, 256, 1280, 1},

  {320, 200, 1 << 15, 640, 2},	/* Hicolor/truecolor modes */
  {320, 200, 1 << 16, 640, 2},
  {320, 200, 1 << 24, 320 * 3, 3},
  {640, 480, 1 << 15, 640 * 2, 2},
  {640, 480, 1 << 16, 640 * 2, 2},
  {640, 480, 1 << 24, 640 * 3, 3},
  {800, 600, 1 << 15, 800 * 2, 2},
  {800, 600, 1 << 16, 800 * 2, 2},
  {800, 600, 1 << 24, 800 * 3, 3},
  {1024, 768, 1 << 15, 1024 * 2, 2},
  {1024, 768, 1 << 16, 1024 * 2, 2},
  {1024, 768, 1 << 24, 1024 * 3, 3},
  {1280, 1024, 1 << 15, 1280 * 2, 2},
  {1280, 1024, 1 << 16, 1280 * 2, 2},
  {1280, 1024, 1 << 24, 1280 * 3, 3},
  
  {800, 600, 16, 100, 0},	/* SVGA 16-color modes */
  {1024, 768, 16, 128, 0},
  {1280, 1024, 16, 160, 0},
  
  {720, 348, 2, 90, 0},	/* Hercules emulation mode */
  
  {320, 200, 1 << 24, 320 * 4, 4},
  {640, 480, 1 << 24, 640 * 4, 4},
  {800, 600, 1 << 24, 800 * 4, 4},
  {1024, 768, 1 << 24, 1024 * 4, 4},
  {1280, 1024, 1 << 24, 1280 * 4, 4},
  
  {1152, 864, 16, 144, 0},
  {1152, 864, 256, 1152, 1},
  {1152, 864, 1 << 15, 1152 * 2, 2},
  {1152, 864, 1 << 16, 1152 * 2, 2},
  {1152, 864, 1 << 24, 1152 * 3, 3},
  {1152, 864, 1 << 24, 1152 * 4, 4},
  
  {1600, 1200, 16, 200, 0},
  {1600, 1200, 256, 1600, 1},
  {1600, 1200, 1 << 15, 1600 * 2, 2},
  {1600, 1200, 1 << 16, 1600 * 2, 2},
  {1600, 1200, 1 << 24, 1600 * 3, 3},
  {1600, 1200, 1 << 24, 1600 * 4, 4},
  
  {320, 240, 256, 320, 1},	
  {320, 240, 1<<15, 320*2, 2},
  {320, 240, 1<<16, 320*2, 2},
  {320, 240, 1<<24, 320*3, 3},
  {320, 240, 1<<24, 320*4, 4},
  
  {400, 300, 256, 400, 1},
  {400, 300, 1<<15, 400*2, 2},
  {400, 300, 1<<16, 400*2, 2},
  {400, 300, 1<<24, 400*3, 3},
  {400, 300, 1<<24, 400*4, 4},
  
  {512, 384, 256, 512, 1},		
  {512, 384, 1<<15, 512*2, 2},
  {512, 384, 1<<16, 512*2, 2},
  {512, 384, 1<<24, 512*3, 3},
  {512, 384, 1<<24, 512*4, 4},
  
  {960, 720, 256, 960, 1},		
  {960, 720, 1<<15, 960*2, 2},
  {960, 720, 1<<16, 960*2, 2},
  {960, 720, 1<<24, 960*3, 3},
  {960, 720, 1<<24, 960*4, 4},
  
  {1920, 1440, 256, 1920, 1},		
  {1920, 1440, 1<<15, 1920*2, 2},
  {1920, 1440, 1<<16, 1920*2, 2},
  {1920, 1440, 1<<24, 1920*3, 3},
  {1920, 1440, 1<<24, 1920*4, 4},
  
  {320, 400, 1<<8,  320,   1},
  {320, 400, 1<<15, 320*2, 2},
  {320, 400, 1<<16, 320*2, 2},
  {320, 400, 1<<24, 320*3, 3},
  {320, 400, 1<<24, 320*4, 4},

  {640, 400, 256, 640, 1},
  {640, 400, 1<<15, 640*2, 2},
  {640, 400, 1<<16, 640*2, 2},
  {640, 400, 1<<24, 640*3, 3},
  {640, 400, 1<<24, 640*4, 4},

  {320, 480, 256, 320, 1},
  {320, 480, 1<<15, 320*2, 2},
  {320, 480, 1<<16, 320*2, 2},
  {320, 480, 1<<24, 320*3, 3},
  {320, 480, 1<<24, 320*4, 4},

  {720, 540, 256, 720, 1},
  {720, 540, 1<<15, 720*2, 2},
  {720, 540, 1<<16, 720*2, 2},
  {720, 540, 1<<24, 720*3, 3},
  {720, 540, 1<<24, 720*4, 4},

  {848, 480, 256, 848, 1},
  {848, 480, 1<<15, 848*2, 2},
  {848, 480, 1<<16, 848*2, 2},
  {848, 480, 1<<24, 848*3, 3},
  {848, 480, 1<<24, 848*4, 4},

  {1072, 600, 256, 1072, 1},
  {1072, 600, 1<<15, 1072*2, 2},
  {1072, 600, 1<<16, 1072*2, 2},
  {1072, 600, 1<<24, 1072*3, 3},
  {1072, 600, 1<<24, 1072*4, 4},

  {1280, 720, 256, 1280, 1},
  {1280, 720, 1<<15, 1280*2, 2},
  {1280, 720, 1<<16, 1280*2, 2},
  {1280, 720, 1<<24, 1280*3, 3},
  {1280, 720, 1<<24, 1280*4, 4},

  {1360, 768, 256, 1360, 1},
  {1360, 768, 1<<15, 1360*2, 2},
  {1360, 768, 1<<16, 1360*2, 2},
  {1360, 768, 1<<24, 1360*3, 3},
  {1360, 768, 1<<24, 1360*4, 4},

  {1800, 1012, 256, 1800, 1},
  {1800, 1012, 1<<15, 1800*2, 2},
  {1800, 1012, 1<<16, 1800*2, 2},
  {1800, 1012, 1<<24, 1800*3, 3},
  {1800, 1012, 1<<24, 1800*4, 4},

  {1920, 1080, 256, 1920, 1},
  {1920, 1080, 1<<15, 1920*2, 2},
  {1920, 1080, 1<<16, 1920*2, 2},
  {1920, 1080, 1<<24, 1920*3, 3},
  {1920, 1080, 1<<24, 1920*4, 4},

  {2048, 1152, 256, 2048, 1},
  {2048, 1152, 1<<15, 2048*2, 2},
  {2048, 1152, 1<<16, 2048*2, 2},
  {2048, 1152, 1<<24, 2048*3, 3},
  {2048, 1152, 1<<24, 2048*4, 4},

  {2048, 1536, 256, 2048, 1},
  {2048, 1536, 1<<15, 2048*2, 2},
  {2048, 1536, 1<<16, 2048*2, 2},
  {2048, 1536, 1<<24, 2048*3, 3},
  {2048, 1536, 1<<24, 2048*4, 4},

  {512, 480, 256, 512, 1},		
  {512, 480, 1<<15, 512*2, 2},
  {512, 480, 1<<16, 512*2, 2},
  {512, 480, 1<<24, 512*3, 3},
  {512, 480, 1<<24, 512*4, 4},

  {400, 600, 256, 400, 1},
  {400, 600, 1<<15, 400*2, 2},
  {400, 600, 1<<16, 400*2, 2},
  {400, 600, 1<<24, 400*3, 3},
  {400, 600, 1<<24, 400*4, 4},

  {400, 300, 256, 100, 0},
  {320, 200, 256, 320, 1},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0},
  {0, 0, 0, 0, 0}
};

#define MAX_MODES (sizeof(infotable) / sizeof(struct info))

//void (*__svgalib_go_to_background) (void) = 0;
//void (*__svgalib_come_from_background) (void) = 0;
//static int release_acquire=0;

unsigned long __svgalib_graph_base = GRAPH_BASE;

unsigned char __svgalib_novga = 0;     /* Does not have VGA circuitry on board */
unsigned char __svgalib_vesatext = 0;
unsigned char __svgalib_textprog = 0;  /* run a program when returning to text mode */
unsigned char __svgalib_secondary = 0; /* this is not the main card with VC'S ) */
unsigned char __svgalib_emulatepage = 0; /* don't use 0xa0000 memory */
unsigned char __svgalib_novccontrol = 0; /* this is not the main card with VC'S  */
unsigned char __svgalib_ragedoubleclock = 0;
unsigned char __svgalib_simple = 0;

/* default palette values */

static const unsigned char default_red[256]
=
{0, 0, 0, 0, 42, 42, 42, 42, 21, 21, 21, 21, 63, 63, 63, 63,
 0, 5, 8, 11, 14, 17, 20, 24, 28, 32, 36, 40, 45, 50, 56, 63,
 0, 16, 31, 47, 63, 63, 63, 63, 63, 63, 63, 63, 63, 47, 31, 16,
 0, 0, 0, 0, 0, 0, 0, 0, 31, 39, 47, 55, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 55, 47, 39, 31, 31, 31, 31, 31, 31, 31, 31,
 45, 49, 54, 58, 63, 63, 63, 63, 63, 63, 63, 63, 63, 58, 54, 49,
 45, 45, 45, 45, 45, 45, 45, 45, 0, 7, 14, 21, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 21, 14, 7, 0, 0, 0, 0, 0, 0, 0, 0,
 14, 17, 21, 24, 28, 28, 28, 28, 28, 28, 28, 28, 28, 24, 21, 17,
 14, 14, 14, 14, 14, 14, 14, 14, 20, 22, 24, 26, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 26, 24, 22, 20, 20, 20, 20, 20, 20, 20, 20,
 0, 4, 8, 12, 16, 16, 16, 16, 16, 16, 16, 16, 16, 12, 8, 4,
 0, 0, 0, 0, 0, 0, 0, 0, 8, 10, 12, 14, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 14, 12, 10, 8, 8, 8, 8, 8, 8, 8, 8,
 11, 12, 13, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 15, 13, 12,
 11, 11, 11, 11, 11, 11, 11, 11, 0, 0, 0, 0, 0, 0, 0, 0};
static const unsigned char gray_red[256]
=
  {0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,
   11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,
   19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,
   27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31,32,32,32,32,33,33,33,33,34,34,34,34,
   35,35,35,35,36,36,36,36,37,37,37,37,38,38,38,38,39,39,39,39,40,40,40,40,41,41,41,41,42,42,42,42,
   43,43,43,43,44,44,44,44,45,45,45,45,46,46,46,46,47,47,47,47,48,48,48,48,49,49,49,49,50,50,50,50,
   51,51,51,51,52,52,52,52,53,53,53,53,54,54,54,54,55,55,55,55,56,56,56,56,57,57,57,57,58,58,58,58,
   59,59,59,59,60,60,60,60,61,61,61,61,62,62,62,62,63,63,63,63};

static const unsigned char gray_green[256]
=
  {0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,
   11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,
   19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,
   27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31,32,32,32,32,33,33,33,33,34,34,34,34,
   35,35,35,35,36,36,36,36,37,37,37,37,38,38,38,38,39,39,39,39,40,40,40,40,41,41,41,41,42,42,42,42,
   43,43,43,43,44,44,44,44,45,45,45,45,46,46,46,46,47,47,47,47,48,48,48,48,49,49,49,49,50,50,50,50,
   51,51,51,51,52,52,52,52,53,53,53,53,54,54,54,54,55,55,55,55,56,56,56,56,57,57,57,57,58,58,58,58,
   59,59,59,59,60,60,60,60,61,61,61,61,62,62,62,62,63,63,63,63};

static const unsigned char default_green[256]
=
{0, 0, 42, 42, 0, 0, 21, 42, 21, 21, 63, 63, 21, 21, 63, 63,
 0, 5, 8, 11, 14, 17, 20, 24, 28, 32, 36, 40, 45, 50, 56, 63,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 16, 31, 47, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 47, 31, 16, 31, 31, 31, 31, 31, 31, 31, 31,
 31, 39, 47, 55, 63, 63, 63, 63, 63, 63, 63, 63, 63, 55, 47, 39,
 45, 45, 45, 45, 45, 45, 45, 45, 45, 49, 54, 58, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 58, 54, 49, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 7, 14, 21, 29, 28, 28, 28, 28, 28, 28, 28, 28, 21, 14, 7,
 14, 14, 14, 14, 14, 14, 14, 14, 14, 17, 21, 24, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 24, 21, 17, 20, 20, 20, 20, 20, 20, 20, 20,
 20, 22, 24, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 26, 24, 22,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 8, 12, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 12, 8, 4, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 10, 12, 14, 16, 16, 16, 16, 16, 16, 16, 16, 16, 14, 12, 10,
 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 13, 15, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 15, 13, 12, 0, 0, 0, 0, 0, 0, 0, 0};

static const unsigned char gray_blue[256]
=
  {0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,
   11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,
   19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,
   27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31,32,32,32,32,33,33,33,33,34,34,34,34,
   35,35,35,35,36,36,36,36,37,37,37,37,38,38,38,38,39,39,39,39,40,40,40,40,41,41,41,41,42,42,42,42,
   43,43,43,43,44,44,44,44,45,45,45,45,46,46,46,46,47,47,47,47,48,48,48,48,49,49,49,49,50,50,50,50,
   51,51,51,51,52,52,52,52,53,53,53,53,54,54,54,54,55,55,55,55,56,56,56,56,57,57,57,57,58,58,58,58,
   59,59,59,59,60,60,60,60,61,61,61,61,62,62,62,62,63,63,63,63};

static const unsigned char default_blue[256]
=
{0, 42, 0, 42, 0, 42, 0, 42, 21, 63, 21, 63, 21, 63, 21, 63,
 0, 5, 8, 11, 14, 17, 20, 24, 28, 32, 36, 40, 45, 50, 56, 63,
 63, 63, 63, 63, 63, 47, 31, 16, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 16, 31, 47, 63, 63, 63, 63, 63, 63, 63, 63, 63, 55, 47, 39,
 31, 31, 31, 31, 31, 31, 31, 31, 31, 39, 47, 55, 63, 63, 63, 63,
 63, 63, 63, 63, 63, 58, 54, 49, 45, 45, 45, 45, 45, 45, 45, 45,
 45, 49, 54, 58, 63, 63, 63, 63, 28, 28, 28, 28, 28, 21, 14, 7,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 14, 21, 28, 28, 28, 28,
 28, 28, 28, 28, 28, 24, 21, 17, 14, 14, 14, 14, 14, 14, 14, 14,
 14, 17, 21, 24, 28, 28, 28, 28, 28, 28, 28, 28, 28, 26, 24, 22,
 20, 20, 20, 20, 20, 20, 20, 20, 20, 22, 24, 26, 28, 28, 28, 28,
 16, 16, 16, 16, 16, 12, 8, 4, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 4, 8, 12, 16, 16, 16, 16, 16, 16, 16, 16, 16, 14, 12, 10,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 10, 12, 14, 16, 16, 16, 16,
 16, 16, 16, 16, 16, 15, 13, 12, 11, 11, 11, 11, 11, 11, 11, 11,
 11, 12, 13, 15, 16, 16, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0};


static const unsigned char gray_blue16[256]=
  {0,3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,
   11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,
   19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,
   27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31,32,32,32,32,33,33,33,33,34,34,34,34,
   35,35,35,35,36,36,36,36,37,37,37,37,38,38,38,38,39,39,39,39,40,40,40,40,41,41,41,41,42,42,42,42,
   43,43,43,43,44,44,44,44,45,45,45,45,46,46,46,46,47,47,47,47,48,48,48,48,49,49,49,49,50,50,50,50,
   51,51,51,51,52,52,52,52,53,53,53,53,54,54,54,54,55,55,55,55,56,56,56,56,57,57,57,57,58,58,58,58,
   59,59,59,59,60,60,60,60,61,61,61,61,62,62,62,62,63,63,63,63};

static const unsigned char gray_red16[256]=
  {0,3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,
   11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,
   19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,
   27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31,32,32,32,32,33,33,33,33,34,34,34,34,
   35,35,35,35,36,36,36,36,37,37,37,37,38,38,38,38,39,39,39,39,40,40,40,40,41,41,41,41,42,42,42,42,
   43,43,43,43,44,44,44,44,45,45,45,45,46,46,46,46,47,47,47,47,48,48,48,48,49,49,49,49,50,50,50,50,
   51,51,51,51,52,52,52,52,53,53,53,53,54,54,54,54,55,55,55,55,56,56,56,56,57,57,57,57,58,58,58,58,
   59,59,59,59,60,60,60,60,61,61,61,61,62,62,62,62,63,63,63,63};

static const unsigned char gray_green16[256]=
  {0,3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,
   11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,
   19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,
   27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31,32,32,32,32,33,33,33,33,34,34,34,34,
   35,35,35,35,36,36,36,36,37,37,37,37,38,38,38,38,39,39,39,39,40,40,40,40,41,41,41,41,42,42,42,42,
   43,43,43,43,44,44,44,44,45,45,45,45,46,46,46,46,47,47,47,47,48,48,48,48,49,49,49,49,50,50,50,50,
   51,51,51,51,52,52,52,52,53,53,53,53,54,54,54,54,55,55,55,55,56,56,56,56,57,57,57,57,58,58,58,58,
   59,59,59,59,60,60,60,60,61,61,61,61,62,62,62,62,63,63,63,63};

static unsigned char text_regs[MAX_REGS];	/* VGA registers for saved text mode */

char *__svgalib_TextProg_argv[16]; /* should be enough */
char *__svgalib_TextProg;

/* saved text mode palette values */
static unsigned char text_red[256];
static unsigned char text_green[256];
static unsigned char text_blue[256];

/* saved graphics mode palette values */
/*MaRTE OS*/
//static unsigned char graph_red[256];
//static unsigned char graph_green[256];
//static unsigned char graph_blue[256];

static int prv_mode = TEXT;	/* previous video mode      */
//static int flip_mode = TEXT;	/* flipped video mode       */

int CM = TEXT;			/* current video mode       */
int CP;                         /* current palette->0 default, 1->gray  */
struct info CI;			/* current video parameters */
int COL;			/* current color            */

static int initialized = 0;	/* flag: initialize() called ?  */
static int flip = 0;		/* flag: executing vga_flip() ? */
//static int background_fd = -1;

/* svgalib additions: */
int __svgalib_chipset = VIDEO_UNDEFINED;
int __svgalib_vendor = VIDEO_UNDEFINED;
int __svgalib_driver_report = 1;
/* report driver used after chipset detection */
int __svgalib_videomemoryused = -1;
int __svgalib_modeX = 0;	/* true after vga_setmodeX() */
int __svgalib_modeflags = 0;	/* copy of flags for current mode */
int __svgalib_critical = 0;	/* indicates blitter is busy */
int __svgalib_screenon = 1;	/* screen visible if != 0 */
int __svgalib_vgacolormode = 1; /* assume color for now. needs to be 
    				   config file option */

//static int __svgalib_savemem=0;

RefreshRange __svgalib_horizsync =
{31500U, 60000U};			/* horz. refresh (Hz) min, max */
RefreshRange __svgalib_vertrefresh =
{50U, 70U};			/* vert. refresh (Hz) min, max */
int __svgalib_bandwidth=50000;  /* monitor maximum bandwidth (kHz) */
int __svgalib_grayscale = 0;	/* grayscale vs. color mode */
int __svgalib_modeinfo_linearset = 0;	/* IS_LINEAR handled via extended vga_modeinfo */
const int __svgalib_max_modes = MAX_MODES;	/* Needed for dynamical allocated tables in mach32.c */
/*MaRTE OS*/
/*static unsigned __svgalib_maxhsync[] =
{
  31500, 35100, 35500, 37900, 48300, 56000, 60000
};*/

static int lastmodenumber = __GLASTMODE;	/* Last defined mode */
//static int my_pid = 0;		/* process PID, used with atexit() */
static int __svgalib_currentpage;
static int vga_page_offset;	/* offset to add to all vga_set*page() calls */
static int currentlogicalwidth;
static int currentdisplaystart;
//static int mouse_support = 0;
//int mouse_open = 0;
//static int mouse_mode = 0;
//static int mouse_type = -1;
//static int mouse_modem_ctl = 0;
//char *__svgalib_mouse_device = "/dev/mouse";
//int __svgalib_mouse_flag;
//static char *mem_device = "/dev/svga";
//static int __svgalib_oktowrite = 1;
static int modeinfo_mask = ~0;

int __svgalib_mem_fd = -1;	/* /dev/svga file descriptor  */
int __svgalib_tty_fd = -1;	/* /dev/tty file descriptor */
int __svgalib_nosigint = 0;	/* Don't generate SIGINT in graphics mode */
/*MaRTE OS*/
//int __svgalib_runinbackground = 0;
//int __svgalib_startup_vc = -1;
//static int __svgalib_security_revokeallprivs = 1;
static unsigned fontbufsize = 8192; /* compatibility */

/* Dummy buffer for mmapping grahics memory; points to 64K VGA framebuffer. */
unsigned char *GM;
/* Exported variable (read-only) is shadowed from internal variable, for */
/* better shared library performance. */
unsigned char *graph_mem;

void *__svgalib_physaddr;
int __svgalib_linear_memory_size;

/*MaRTE OS*/
//static unsigned long graph_buf_size = 0;
//static unsigned char *graph_buf = NULL;		/* saves graphics data during flip */
//static int inbackground=0;

static unsigned char *font_buf1;	/* saved font data - plane 2 */
static unsigned char *font_buf2;	/* saved font data - plane 3 */
static unsigned char *text_buf1=NULL;	/* saved text data - plane 0 */
static unsigned char *text_buf2;	/* saved text data - plane 1 */

int __svgalib_flipchar = '\x1b';		/* flip character - initially  ESCAPE */

/* Chipset specific functions */

DriverSpecs *__svgalib_driverspecs = &__svgalib_vga_driverspecs;

/*MaRTE OS*/
//static void (*__svgalib_setpage) (int);	/* gives little faster vga_setpage() */
//static void (*__svgalib_setrdpage) (int);
//static void (*__svgalib_setwrpage) (int);

int  (*__svgalib_inmisc)(void);
void (*__svgalib_outmisc)(int);
int  (*__svgalib_incrtc)(int);
void (*__svgalib_outcrtc)(int,int);
int  (*__svgalib_inseq)(int);
void (*__svgalib_outseq)(int,int);
int  (*__svgalib_ingra)(int);
void (*__svgalib_outgra)(int,int);
int  (*__svgalib_inatt)(int);
void (*__svgalib_outatt)(int,int);
void (*__svgalib_attscreen)(int);
void (*__svgalib_inpal)(int,int*,int*,int*);
void (*__svgalib_outpal)(int,int,int,int);
int  (*__svgalib_inis1)(void);

void map_banked(void);
void map_banked_fix(void);
inline void vga_setpage(int p);

DriverSpecs *__svgalib_driverspecslist[] =
{
  NULL,			/* chipset undefined */
  &__svgalib_vga_driverspecs,
#ifdef INCLUDE_ET4000_DRIVER
  &__svgalib_et4000_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_CIRRUS_DRIVER
  &__svgalib_cirrus_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_TVGA_DRIVER
  &__svgalib_tvga8900_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_OAK_DRIVER
  &__svgalib_oak_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_EGA_DRIVER
  &__svgalib_ega_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_S3_DRIVER
  &__svgalib_s3_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_ET3000_DRIVER
  &__svgalib_et3000_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_MACH32_DRIVER
  &__svgalib_mach32_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_GVGA6400_DRIVER
  &__svgalib_gvga6400_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_ARK_DRIVER
  &__svgalib_ark_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_ATI_DRIVER
  &__svgalib_ati_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_ALI_DRIVER
  &__svgalib_ali_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_MACH64_DRIVER
  &__svgalib_mach64_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_CHIPS_DRIVER
  &__svgalib_chips_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_APM_DRIVER
  &__svgalib_apm_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_NV3_DRIVER
  &__svgalib_nv3_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_ET6000_DRIVER
  &__svgalib_et6000_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_VESA_DRIVER
  &__svgalib_vesa_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_MX_DRIVER
  &__svgalib_mx_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_PARADISE_DRIVER
  &__svgalib_paradise_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_RAGE_DRIVER
  &__svgalib_rage_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_BANSHEE_DRIVER
  &__svgalib_banshee_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_SIS_DRIVER
  &__svgalib_sis_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_I740_DRIVER
  &__svgalib_i740_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_NEO_DRIVER
  &__svgalib_neo_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_LAGUNA_DRIVER
  &__svgalib_laguna_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_FBDEV_DRIVER
  &__svgalib_fbdev_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_G400_DRIVER
  &__svgalib_g400_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_R128_DRIVER
  &__svgalib_r128_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_SAVAGE_DRIVER
  &__svgalib_savage_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_MILLENNIUM_DRIVER
  &__svgalib_mil_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_I810_DRIVER
  &__svgalib_i810_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_TRIDENT_DRIVER
  &__svgalib_trident_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_RENDITION_DRIVER
  &__svgalib_rendition_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_G450C2_DRIVER
  &__svgalib_g450c2_driverspecs,
#else
  NULL,
#endif
#ifdef INCLUDE_PM2_DRIVER
  &__svgalib_pm2_driverspecs,
#else
  NULL,
#endif
};

/*MaRTE OS*/
/*static char *driver_names[] =
{
  "", 
  "VGA", 
  "ET4000", 
  "Cirrus", 
  "TVGA", 
  "Oak", 
  "EGA", 
  "S3",
  "ET3000", 
  "Mach32", 
  "GVGA6400",
  "ARK",
  "ATI",
  "ALI",
  "Mach64", 
  "C&T",
  "APM",
  "NV3",
  "ET6000",
  "VESA",
  "MX",
  "PARADISE",
  "RAGE",
  "BANSHEE", 
  "SIS",
  "I740",
  "NEOMAGIC",
  "LAGUNA",
  "FBDev",
  "G400",
  "R128",
  "SAVAGE",
  "MILLENNIUM",
  "I810",
  "TRIDENT",
  "RENDITION",
  "G450C2",
  "PM2",
  NULL};*/

/* Chipset drivers */

/* vgadrv       Standard VGA (also used by drivers below) */
/* et4000       Tseng ET4000 (from original vgalib) */
/* cirrus       Cirrus Logic GD542x */
/* tvga8900     Trident TVGA 8900/9000 (derived from tvgalib) */
/* oak          Oak Technologies 037/067/077 */
/* egadrv       IBM EGA (subset of VGA) */
/* s3           S3 911 */
/* mach32       ATI MACH32 */
/* ark          ARK Logic */
/* gvga6400     Genoa 6400 (old SVGA) */
/* ati          ATI */
/* ali          ALI2301 */
/* mach64	ATI MACH64 */
/* chips	chips & technologies*/
/* et6000       Tseng ET6000 */         /* DS */

/*MaRTE OS*/
point_t conv_to_point(unsigned int x, unsigned int y)
{
  point_t exit;
  exit.x=x;
  exit.y=y;
  return (exit);
}



/*<MaRTE OS*/
unsigned short int rgb15(int r,int g,int b)
{
  return((((WORD)(r & 0xF8)>>3) << 10) | (((WORD)(g & 0xF8)>>3) << 5) | ((WORD)(b & 0xF8)>>3));
}

unsigned short int rgb16(int r,int g,int b)
{
  return((((WORD)(r & 0xF8)>>3) << 11) | (((WORD)(g & 0xFC)>>2) << 5) | ((WORD)(b & 0xF8)>>3));
}


unsigned int rgb24(int r,int g,int b)
{
  return(((DWORD)(r & 0xFF) << 16) | ((DWORD)(g & 0xFF) << 8) | (DWORD)(b & 0xFF));
}

unsigned int rgb32(int r,int g,int b)
{
  return(((DWORD)(r & 0xFF) << 16) | ((DWORD)(g & 0xFF) << 8) | (DWORD)(b & 0xFF));
}

unsigned int rgba(int r,int g,int b)
{
  return(((DWORD)(b & 0xFF) << 16) | ((DWORD)(g & 0xFF) << 8) | (DWORD)(r & 0xFF));
}

unsigned short int conversor_32_a_16(unsigned int rgb)
{
  return( (WORD)((rgb & 0xF8)>>3) | (WORD) ((rgb& 0xFC00)>>5 ) | (WORD)((rgb & 0xF80000)>>8));
}

unsigned short int conversor_24_a_16(unsigned int rgb)
{
  return( ( (WORD)(rgb & 0xF8)>>3) | ( (WORD) ((rgb>>5)& 0x7E0) ) | ((WORD)((rgb>>8) & 0xF800)));
}

unsigned short int conversor_32_a_15(unsigned int rgb)
{
  return( ( (WORD)(rgb & 0xF8)>>3) | ( (WORD) ((rgb>>5)& 0x3E0) ) | ((WORD)((rgb>>9) & 0x7C00)));
}

unsigned short int conversor_24_a_15(unsigned int rgb)
{
  return( ( (WORD)(rgb & 0xF8)>>3) | ( (WORD) ((rgb>>5)& 0x3E0) ) | ((WORD)((rgb>>9) & 0x7C00)));
}



/*<MaRTE OS*/
/*Traido de OSKIT*/

void slowcpy(unsigned char *dest, unsigned char *src, unsigned bytes)
{
  while (bytes-- > 0)
    *(dest++) = *(src++);
}
/*MaRTE OS>*/


inline void vga_setpage(int p)
{
  p += vga_page_offset;
  if (p == __svgalib_currentpage)
    return;

  __svgalib_currentpage = p;
}

int __svgalib_setregs(const unsigned char *regs)
{
  int i;

  if(__svgalib_novga) return 1;

  if (__svgalib_chipset == EGA) {
    /* Enable graphics register modification */
    port_out(0x00, GRA_E0);
    port_out(0x01, GRA_E1);
  }
  /* update misc output register */
  __svgalib_outmisc(regs[MIS]);

  /* synchronous reset on */
  __svgalib_outseq(0x00,0x01);

  /* write sequencer registers */
  __svgalib_outseq(0x01,regs[SEQ + 1] | 0x20);
  for (i = 2; i < SEQ_C; i++) {
    __svgalib_outseq(i,regs[SEQ + i]);
  }

  /* synchronous reset off */
  __svgalib_outseq(0x00,0x03);

  if (__svgalib_chipset != EGA) {
    /* deprotect CRT registers 0-7 */
    __svgalib_outcrtc(0x11,__svgalib_incrtc(0x11)&0x7f);
  }
  /* write CRT registers */
  for (i = 0; i < CRT_C; i++) {
    __svgalib_outcrtc(i,regs[CRT + i]);
  }

  /* write graphics controller registers */
  for (i = 0; i < GRA_C; i++) {
    __svgalib_outgra(i,regs[GRA+i]);
  }

  /* write attribute controller registers */
  for (i = 0; i < ATT_C; i++) {
    __svgalib_outatt(i,regs[ATT+i]);
  }

  return 0;
}
/*<MaRTE OS*/
static void prepareforfontloading(void)
{
  if (__svgalib_chipset == CIRRUS) {
    outb(0x3c4, 0x0f);
    /* Disable CRT FIFO Fast-Page mode. */
    outb(0x3c5, inb(0x3c5) | 0x40);
  }
}

static void fontloadingcomplete(void)
{
  if (__svgalib_chipset == CIRRUS) {
    outb(0x3c4, 0x0f);
    /* Re-enable CRT FIFO Fast-Page mode. */
    outb(0x3c5, inb(0x3c5) & 0xbf);
  }
}
/*MaRTE OS>*/

int vga_screenon(void)
{
  int tmp = 0;

  SCREENON = 1;
  if(__svgalib_novga) return 0; 
  if (__svgalib_driverspecs->emul && __svgalib_driverspecs->emul->screenon) {
    tmp = __svgalib_driverspecs->emul->screenon();
  } else {
    /* turn screen back on */
    if ((CHIPSET != EGA) && !__svgalib_novga) {
      __svgalib_outseq(0x01,__svgalib_inseq(0x01) & 0xdf);
    }
    /* #ifdef DISABLE_VIDEO_OUTPUT */
    /* enable video output */
    __svgalib_attscreen(0x20);
    /* #endif */
  }

  return 0;
}

int vga_claimvideomemory(int m)
{
  vga_modeinfo *modeinfo;
  int cardmemory;

  modeinfo = vga_getmodeinfo(CM);
  if (m < VMEM)
    return 0;
  if (modeinfo->colors == 16)
    cardmemory = modeinfo->maxpixels / 2;
  else
    cardmemory = (modeinfo->maxpixels * modeinfo->bytesperpixel
		  + 2) & 0xffff0000;
  /* maxpixels * bytesperpixel can be 2 less than video memory in */
  /* 3 byte-per-pixel modes; assume memory is multiple of 64K */
  if (m > cardmemory)
    return -1;
  VMEM = m;
  return 0;
}

void map_banked() {
    
  if(__svgalib_emulatepage){
    BANKED_POINTER=(void *)__svgalib_linear_mem_base;
  } else {
    BANKED_POINTER=(void *)__svgalib_banked_mem_base;
  }

}

void map_mmio() {

  if(mmio_mapped) return;

  if(__svgalib_mmio_size) {
    mmio_mapped=1;
    MMIO_POINTER=(void *)__svgalib_mmio_base;
  } else {
    MMIO_POINTER=NULL;
    SPARSE_MMIO=NULL;
  }
}

void map_mem() {
    
  if(mem_mapped) return;
  
  if(__svgalib_banked_mem_size==0) __svgalib_banked_mem_size = 0x10000;

  map_banked();
    
  if(__svgalib_linear_mem_size) {
    map_linear(__svgalib_linear_mem_base, __svgalib_linear_mem_size);
  };

  B8000_POINTER = (void *)0xb8000;

  mem_mapped = 1;

}

static void __vga_map(void)
{
  GM = (unsigned char *) BANKED_POINTER;
  graph_mem = GM;	/* Exported variable. */
}

void __svgalib_emul_setpage(int page)
{
  static int oldpage = -2;

  if (page != oldpage)
    {
      oldpage = page;
    }
}

static void map_vgaio(void)
{
  __svgalib_inmisc=__svgalib_vga_inmisc;
  __svgalib_outmisc=__svgalib_vga_outmisc;
  __svgalib_incrtc=__svgalib_vga_incrtc;
  __svgalib_outcrtc=__svgalib_vga_outcrtc;
  __svgalib_inseq=__svgalib_vga_inseq;
  __svgalib_outseq=__svgalib_vga_outseq;
  __svgalib_ingra=__svgalib_vga_ingra;
  __svgalib_outgra=__svgalib_vga_outgra;
  __svgalib_inatt=__svgalib_vga_inatt;
  __svgalib_outatt=__svgalib_vga_outatt;
  __svgalib_attscreen=__svgalib_vga_attscreen;
  __svgalib_inpal=__svgalib_vga_inpal;
  __svgalib_outpal=__svgalib_vga_outpal;
  __svgalib_inis1=__svgalib_vga_inis1;
}

int __svgalib_saveregs(unsigned char *regs)
{
  int i;

  if (__svgalib_chipset == EGA || __svgalib_novga) {
    /* Special case: Don't save standard VGA registers. */
    return chipset_saveregs(regs);
  }
  /* save VGA registers */
  for (i = 0; i < CRT_C; i++) {
    regs[CRT + i] = __svgalib_incrtc(i);
  }
  for (i = 0; i < ATT_C; i++) {
    regs[ATT + i] = __svgalib_inatt(i);
  }
  for (i = 0; i < GRA_C; i++) {
    regs[GRA + i] = __svgalib_ingra(i);
  }

  for (i = 0; i < SEQ_C; i++) {
    regs[SEQ + i] = __svgalib_inseq(i);
  }

  regs[MIS] = __svgalib_inmisc();

  i = chipset_saveregs(regs);	/* save chipset-specific registers */
  /* i : additional registers */
  if (!SCREENON) {		/* We turned off the screen */
    __svgalib_attscreen(0x20);
  }
  return CRT_C + ATT_C + GRA_C + SEQ_C + 1 + i;
}

int vga_setcolor(int color)
{
  switch (CI.colors) {
  case 2:
    if (color != 0)
      color = 15;
  case 16:			/* update set/reset register */
    __svgalib_outgra(0x00,color&0x0f);
    break;
  default:
    COL = color;
    break;
  }
  return 0;
}

vga_modeinfo *vga_getmodeinfo(int mode)
{
  static vga_modeinfo modeinfo;
  int is_modeX = (CM == mode) && MODEX;

  modeinfo.linewidth = infotable[mode].xbytes;
  __svgalib_getchipset();
  if (mode > vga_lastmodenumber())
    return NULL;
  modeinfo.width = infotable[mode].xdim;
  modeinfo.height = infotable[mode].ydim;
  modeinfo.bytesperpixel = infotable[mode].bytesperpixel;
  modeinfo.colors = infotable[mode].colors;
  if (is_modeX) {
    modeinfo.linewidth = modeinfo.width / 4;
    modeinfo.bytesperpixel = 0;
  }
  if (mode == TEXT) {
    modeinfo.flags = HAVE_EXT_SET;
    return &modeinfo;
  }
  modeinfo.flags = 0;
  if ((STDVGAMODE(mode) && mode != G320x200x256) || is_modeX)
    __svgalib_vga_driverspecs.getmodeinfo(mode, &modeinfo);
  else
    /* Get chipset specific info for SVGA modes and */
    /* 320x200x256 (chipsets may support more pages) */
    chipset_getmodeinfo(mode, &modeinfo);

  if (modeinfo.colors == 256 && modeinfo.bytesperpixel == 0)
    modeinfo.flags |= IS_MODEX;
  if (mode > __GLASTMODE)
    modeinfo.flags |= IS_DYNAMICMODE;

  /* Maskout CAPABLE_LINEAR if requested by config file */
  modeinfo.flags &= modeinfo_mask;

  /* Many cards have problems with linear 320x200x256 mode */
  if(mode==G320x200x256)modeinfo.flags &= (~CAPABLE_LINEAR) & (~IS_LINEAR) ;

  /* If all needed info is here, signal if linear support has been enabled */
  if ((modeinfo.flags & (CAPABLE_LINEAR | EXT_INFO_AVAILABLE)) ==
      (CAPABLE_LINEAR | EXT_INFO_AVAILABLE)) {
    modeinfo.flags |= __svgalib_modeinfo_linearset;
  }

  return &modeinfo;
}

char *__svgalib_token(char **ptr) 
{
  char *p;
  p=*ptr;
  while(*p==' ')p++;
    
  if(*p != '\0' ) {
    char *t;
    t=p;
    while((*t != '\0') && (*t != ' '))t++;
    if(*t==' ') {
      *t='\0';
      t++;
    }
    *ptr=t;
    return p;
  } else {
    *ptr=NULL;
    return NULL; 
  }
}

int vga_lastmodenumber(void)
{
  //    __svgalib_getchipset();
  return lastmodenumber;
}

int __svgalib_getchipset(void)
{

  /* SHARK: Supported Graphics Drivers
   *
   * NV3 (NVIDIA: GEFORCE/TNT/TNT2)
   * SAVAGE (S3: VIRGE/SAVAGE
   * R128 (ATI: RAGE 128/RADEON)
   * NEOMAGIC (NEOMAGIC CARD)
   * VGA  
   *
   */
	
  if(!initialized) {
	
    map_vgaio();
    init_vgapci(VENDOR,CHIPSET);
	
    switch(VENDOR) {
    case PCI_VENDOR_ID_NVIDIA:
      nv3_test();
      break;
    case PCI_VENDOR_ID_S3:
      savage_test();
      break;
    case PCI_VENDOR_ID_ATI:
      r128_test();
      break;
    case PCI_VENDOR_ID_NEOMAGIC:
      neo_test();
      break;
    case VGA:
      vga_test();
      break;
      /*MaRTE OS*/
      /*		case VESA:
			vesa_test();
			break;
      */
    }
	
  }

  return CHIPSET;

}

int vga_getxdim(void)
{
  return CI.xdim;
}


int vga_getydim(void)
{
  return CI.ydim;
}


int vga_getcolors(void)
{
  return CI.colors;
}

int vga_white(void)
{
  switch (CI.colors) {
  case 2:
  case 16:
  case 256:
    return 15;
  case 1 << 15:
    return 32767;
  case 1 << 16:
    return 65535;
  case 1 << 24:
    return (1 << 24) - 1;
  }
  return CI.colors - 1;
}

void __svgalib_delay(void)
{
  int i;
  for (i = 0; i < 10; i++);
}

int vga_screenoff(void)
{
  int tmp = 0;
  
  SEQ01=__svgalib_inseq(0x01);
  SCREENON = 0;

  if(__svgalib_novga) return 0; 

  if (__svgalib_driverspecs->emul && __svgalib_driverspecs->emul->screenoff) {
    tmp = __svgalib_driverspecs->emul->screenoff();
  } else {
    /* turn off screen for faster VGA memory acces */
    if ((CHIPSET != EGA) && !__svgalib_novga) {
      __svgalib_outseq(0x01,__svgalib_inseq(0x01) | 0x20);
    }
    /* Disable video output */
#ifdef DISABLE_VIDEO_OUTPUT
    __svgalib_attscreen(0);
#endif
  }

  return tmp;
}

static void setcoloremulation(void)
{
  /* shift to color emulation */
  __svgalib_CRT_I = CRT_IC;
  __svgalib_CRT_D = CRT_DC;
  __svgalib_IS1_R = IS1_RC;
  __svgalib_vgacolormode=1;
    /*MaRTE OS
   if (CHIPSET != EGA && !__svgalib_novga)  
       __svgalib_outmisc(__svgalib_inmisc()|0x01);*/
}

void map_linear(unsigned long base, unsigned long size) {
    
  LINEAR_POINTER= (void *)base;

}

static void savepalette(unsigned char *red, unsigned char *green,
			unsigned char *blue)
{
  int i;

  if (__svgalib_driverspecs->emul && __svgalib_driverspecs->emul->savepalette) 
    return (__svgalib_driverspecs->emul->savepalette(red, green, blue));

  if (CHIPSET == EGA || __svgalib_novga) 
    return;

  /* save graphics mode palette */
    /* save graphics mode palette - first select palette index 0 */
    outb(PEL_IR,0);

    /* read RGB components - index is autoincremented */
    for (i = 0; i < 256; i++) {
	__svgalib_delay();
	*(red++) = port_in(PEL_D);
	__svgalib_delay();
	*(green++) = port_in(PEL_D);
	__svgalib_delay();
	*(blue++) = port_in(PEL_D);
    }

/*    for (i = 0; i < 256; i++) { */
/*      int r,g,b; */
/*      __svgalib_inpal(i,&r,&g,&b); */
/*      *(red++) = r; */
/*      *(green++) = g; */
/*      *(blue++) = b; */
/*    } */
}

static void restorepalette(const unsigned char *red,
			   const unsigned char *green, const unsigned char *blue)
{
  int i;

  if (__svgalib_driverspecs->emul && __svgalib_driverspecs->emul->restorepalette) 
    return (__svgalib_driverspecs->emul->restorepalette(red, green, blue));

  if (CHIPSET == EGA || __svgalib_novga)
    return;

  /* restore saved palette */
  /* read RGB components - index is autoincremented */
  for (i = 0; i < 256; i++) {
    __svgalib_outpal(i,*(red++),*(green++),*(blue++));
  }
}

int vga_getcurrentmode(void)
{
  return CM;
}


static void initialize()
{
  int i;

  //printk("Initialize\n");

  //    __svgalib_disable_interrupt();	/* Is reenabled later by set_texttermio */
  /*MaRTE OS*/
  setcoloremulation();
  __svgalib_getchipset();

  chipset_unlock();

  /* mmap graphics memory */
  map_mem();
  map_mmio();

  __vga_map();

  /* disable video */
  vga_screenoff();

  /* Sanity check: (from painful experience) */
  i = __svgalib_saveregs(text_regs);
  if (i > MAX_REGS) {
    printf("svgalib: FATAL internal error:\n");
    /*	printk("Set MAX_REGS at least to %d in src/driver.h and recompile everything.\n",
	i);*/
  }

  /* save text mode palette */
  savepalette(text_red, text_green, text_blue);

  /* shift to color emulation */
  setcoloremulation();

  /*<MaRTE OS*/
  /*Traido de oskit.c*/
  /* save font data - first select a 16 color graphics mode */
  if (__svgalib_driverspecs->emul && __svgalib_driverspecs->emul->savefont) {
    __svgalib_driverspecs->emul->savefont();
  } else if(!__svgalib_novga) {
    __svgalib_driverspecs->setmode(GPLANE16, prv_mode);
  }
  
  /* Allocate space for textmode font. */
  
  /*MaRTE OS*/
  font_buf1 = malloc(FONT_SIZE * 2);

  font_buf2 = font_buf1 + FONT_SIZE;

  /* save font data in plane 2 */
  port_out(0x04, GRA_I);
  port_out(0x02, GRA_D);
  slowcpy(font_buf1, GM, FONT_SIZE);

  /* save font data in plane 3 */
  port_out(0x04, GRA_I);
  port_out(0x03, GRA_D);
  slowcpy(font_buf2, GM, FONT_SIZE);
  /*MaRTE OS>*/
  
  initialized = 1;
    
  /* vga_unlockvc(); */

}

void vga_gettextfont(void *font)
{
  unsigned int getsize;

  getsize = fontbufsize;
  if (getsize > FONT_SIZE)
    getsize = FONT_SIZE;
  memcpy(font, font_buf1, getsize);
  if (fontbufsize > getsize)
    memset(((char *)font) + getsize, 0, (size_t)(fontbufsize - getsize));
}

void vga_puttextfont(void *font)
{
  unsigned int putsize;

  putsize = fontbufsize;
  if (putsize > FONT_SIZE)
    putsize = FONT_SIZE;
  memcpy(font_buf1, font, putsize);
  memcpy(font_buf2, font, putsize);
  if (putsize < FONT_SIZE) {
    memset(font_buf1 + putsize, 0, (size_t)(FONT_SIZE - putsize));
    memset(font_buf2 + putsize, 0, (size_t)(FONT_SIZE - putsize));
  }

}

/* inline void vuelve_texto(void) */

/* { */
/*   struct x86_desc *x; */
/*   x=base_gdt; */
/*   //        asm volatile("movl %0,%%eax": :"a"(x) :"eax"); */
/*   asm volatile("movw $0x28,%bx"); */
/*   asm volatile("movw %bx,%ds"); */
/*   asm volatile("movl %cr0,%eax"); */
/*   asm volatile("andl $0xfe,%eax"); */
/*   asm volatile("movl %eax,%cr0"); */
/*   asm volatile("jmp salir */
/*                       salir: */
/*                       nop"); */

/*   /\*	asm volatile("xorl %ebx,%ebx"); */
/* 	asm volatile("movw %cs,%bx"); */
/* 	asm volatile("andw $0xfff8,%ax"); */
/* 	//        asm volatile("shll $16,%ebx"); */
/*         asm volatile("addl %ebx,%eax"); */
/* 	asm volatile("addl $7,%eax"); */
/*         asm volatile("movb (%eax),%bh"); */
/* 	asm volatile("subl $3,%eax"); */
/*         asm volatile("movb (%eax),%bl"); */
/* 	asm volatile("shll $16,%ebx"); */
/* 	asm volatile("subl $2,%eax"); */
/*         asm volatile("movw (%eax),%bx"); */
/*   *\/ */
/*   /\*	asm volatile("pushl %eax"); */
	
/* 	asm volatile("movb %eax"); */













/* 	asm volatile("xorl %ebx,%ebx"); */
/*         asm volatile("movl %ds,%ebx");         */

/*         asm volatile("shr $3,%ebx"); */

/* 	[] */
/*         asm volatile("movl %cs,%eax");         */
/*         asm volatile("andl $0x7,%eax"); */



/* 	asm volatile("movl %cr0,%eax"); */
/* 	asm volatile("and $0xFE,%al"); */
/* 	asm volatile("movl %eax,%cr0"); */
/* 	asm volatile("int $0x10" : : "a" (0x0003));*\/ */
	
/* } */


void restorepalette_default(void){
  CP=0;
  restorepalette(default_red, default_green, default_blue);
}

void restoregray_palette(void){
  CP=1;
  switch(CI.colors){
  case 16:
    restorepalette(gray_red, gray_green, gray_blue);
    return;
  case 256:
    restorepalette(gray_red16, gray_green16, gray_blue16);
    return;
  }
}

#define TEXT_SIZE 65536

static void restore_text(void)
{
  __svgalib_outseq(0x02,0x01);
  slowcpy(GM, text_buf1, TEXT_SIZE);
  
  __svgalib_outseq(0x02,0x02);
  slowcpy(GM, text_buf2, TEXT_SIZE);
};

int vga_setmode(int mode)
{
  int modeflags=mode&0xfffff000;
  //    struct LRMI_regs vesa_r;
  /*MaRTE OS*/
  int respuesta;
 
  printf("Setmode %i from %i\n", mode, CM);
  
  if(mode==-1)return vga_version;
    
  mode&=0xfff;
  
  //  init_serial_communication_with_gdb (SERIAL_PORT_1);
  //set_break_point_here;
  if (!initialized) {
    if (mode == TEXT) return 0;
    initialize();
  }
  respuesta=chipset_modeavailable(mode);
  if (mode != TEXT && !respuesta) {
    return -1;
  }
  
  //    __svgalib_disable_interrupt();
  
  prv_mode = CM;
  CM = mode;
  
  /* disable video */
  vga_screenoff();
  
  if (mode == TEXT) {
    
    if (buf_rgb!=NULL)
      free(buf_rgb);
    
    if (SVGAMODE(prv_mode))
      vga_setpage(0);		
    /* The extended registers are restored either by the */
    /* chipset setregs function, or the chipset setmode function. */
    
    /* restore font data - first select a 16 color graphics mode */
    /* Note: this should restore the old extended registers if */
    /* setregs is not defined for the chipset. */
    
    if(__svgalib_novga) 
      __svgalib_driverspecs->setmode(TEXT, prv_mode);
    
    if (__svgalib_driverspecs->emul && __svgalib_driverspecs->emul->restorefont) {
      __svgalib_driverspecs->emul->restorefont(); 
      chipset_setregs(text_regs, mode);
      
    } else if(!__svgalib_novga) {
      __svgalib_driverspecs->setmode(GPLANE16, prv_mode);
      
      if (CHIPSET != EGA)
	/* restore old extended regs */
	chipset_setregs(text_regs, mode);
      
      /* disable Set/Reset Register */
      port_out(0x01, GRA_I);
      port_out(0x00, GRA_D);
      
      prepareforfontloading();
      restore_text();
      
      /* restore font data in plane 2 - necessary for all VGA's */
      port_out(0x02, SEQ_I);
      port_out(0x04, SEQ_D);
      
      slowcpy(GM, font_buf1, FONT_SIZE);
      
      /* restore font data in plane 3 - necessary for Trident VGA's */
      port_out(0x02, SEQ_I);
      port_out(0x08, SEQ_D);
      
      slowcpy(GM, font_buf2, FONT_SIZE);
      
      fontloadingcomplete();
      
      /*MaRTE OS*/
      color_text=1;
      /* change register adresses if monochrome text mode */
      /* EGA is assumed to use color emulation. */
      if (!color_text) {
	__svgalib_CRT_I = CRT_IM;
	__svgalib_CRT_D = CRT_DM;
	__svgalib_IS1_R = IS1_RM;
	port_out(port_in(MIS_R) & 0xFE, MIS_W);
      }
    } else chipset_setregs(text_regs, mode);
    /* restore saved palette */
    restorepalette(text_red, text_green, text_blue);
    
    /* restore text mode VGA registers */
    __svgalib_setregs(text_regs);
    
    /* Set VMEM to some minimum value .. probably pointless.. */
    {
      vga_claimvideomemory(12);
    }
    
    
    /*       enable text output - restores the screen contents */ 
    /*      if (!__svgalib_secondary) */
    /*        ioctl(__svgalib_tty_fd, KDSETMODE, KD_TEXT); */
    /*MaRTE OS*/
    /*Limpiamos la pantalla*/

    printf("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");

    /* enable video */
    vga_screenon();
    
    //    if (!flip)
      /* restore text mode termio */
    // set_texttermio();
  
  



  } else {
    /* Setting a graphics mode. */



    //Guardar registros que se van a tocar




    if (SVGAMODE(prv_mode)) {
      /* The current mode is an SVGA mode, and we now want to */
      /* set a standard VGA mode. Make sure the extended regs */
      /* are restored. */
      /* Also used when setting another SVGA mode to hopefully */
      /* eliminate lock-ups. */
      vga_setpage(0);
      chipset_setregs(text_regs, mode);
      /* restore old extended regs */
    }
    /* shift to color emulation */
    setcoloremulation();

    CI.xdim = infotable[mode].xdim;
    CI.ydim = infotable[mode].ydim;
    CI.colors = infotable[mode].colors;
    CI.xbytes = infotable[mode].xbytes;
    CI.bytesperpixel = infotable[mode].bytesperpixel;

    chipset_setmode(mode, prv_mode);
    MODEX = 0;

    /* Set default claimed memory (moved here from initialize - Michael.) */
    if (mode == G320x200x256)
      VMEM = 65536;
    else if (STDVGAMODE(mode))
      VMEM = 256 * 1024;	/* Why always 256K ??? - Michael */
    else {
      vga_modeinfo *modeinfo;

      modeinfo = vga_getmodeinfo(mode);
      VMEM = modeinfo->linewidth * modeinfo->height;
      CI.xbytes = modeinfo->linewidth;
    }

    if (!flip) {
      /* set default palette */
      if (CI.colors <= 256)
	restorepalette(default_red, default_green, default_blue);

      /* clear screen (sets current color to 15) */
      __svgalib_currentpage = -1;

      if(!(modeflags&0x8000))vga_clear();

      if (SVGAMODE(CM))
	vga_setpage(0);
    }
    __svgalib_currentpage = -1;
    currentlogicalwidth = CI.xbytes;
    currentdisplaystart = 0;

    /*MaRTE OS*/
    /* enable video */
//    if (!flip)
//      vga_screenon();

    {
      vga_modeinfo *modeinfo;
      modeinfo = vga_getmodeinfo(mode);
      MODEX = ((MODEFLAGS = modeinfo->flags) & IS_MODEX);

    }//	__svgalib_enable_interrupt();
  }

  return 0;

}

inline void copy_videomem_32to16(void *src, void *dst, unsigned long int memdiv2)
{
  __asm__ __volatile__(
		       "push %%esi\n"
				"push %%edi\n"
			  	"movl %1, %%esi\n"
				"movl %2, %%edi\n"
			        "1:\n"
				"movl (%%esi), %%edx\n"
				"xorl %%ebx, %%ebx\n"
			  	"xorw %%ax, %%ax\n"
				"movb %%dl, %%al\n"
				"shrl $0x8, %%edx\n"
				"shrw $0x3, %%ax\n"
				"orw  %%ax, %%bx\n"
				"shll $0x6, %%ebx\n"
				"xorw %%ax, %%ax\n"
				"movb %%dl, %%al\n"
				"shrl $0x8, %%edx\n"
				"shrw $0x2, %%ax\n"
				"orw  %%ax, %%bx\n"
				"shll $0x5, %%ebx\n"
				"xorw %%ax, %%ax\n"
				"movb %%dl, %%al\n"
				"shrw $0x3, %%ax\n"
				"orw  %%ax, %%bx\n"
				"shll $0x5, %%ebx\n"
				"incl %%esi\n"
				"incl %%esi\n"
				"incl %%esi\n"
				"incl %%esi\n"
				"movl (%%esi), %%edx\n"
				"xorw %%ax, %%ax\n"
				"movb %%dl, %%al\n"
				"shrl $0x8,%%edx\n"
				"shrw $0x3, %%ax\n"
				"orw  %%ax, %%bx\n"
				"shll $0x6, %%ebx\n"
				"xorw %%ax, %%ax\n"
				"movb %%dl, %%al\n"
				"shrl $0x8, %%edx\n"
				"shrw $0x2, %%ax\n"
				"orw  %%ax, %%bx\n"
				"shll $0x5, %%ebx\n"
				"xorw %%ax, %%ax\n"
				"movb %%dl, %%al\n"
				"shrw $0x3, %%ax\n"
				"orw  %%ax, %%bx\n"
				"rorl $0x10, %%ebx\n"
				"movl %%ebx, (%%edi)\n"
				"incl %%esi\n"
				"incl %%esi\n"
				"incl %%esi\n"
				"incl %%esi\n"
				"incl %%edi\n"
				"incl %%edi\n"
				"incl %%edi\n"
				"incl %%edi\n"
				"loop 1b\n"
				"pop %%edi\n"
				"pop %%esi" 
		       : 
		       : "c" (memdiv2), "a" (src), "b" (dst));

}

inline void copy_videomem_16to16(void *src, void *dst, unsigned long int memdiv4)
{
  __asm__ __volatile__("push %%esi\n"
				"push %%edi\n"
			  	"movl %1, %%esi\n"
				"movl %2, %%edi\n"
				"cld\n"
				"rep\n"
				"movsl\n"
			        
				"pop %%edi\n"
				"pop %%esi" 
		       : 
		       : "c" (memdiv4), "a" (src), "b" (dst));

}

#include "vgamem.h"

void refresh_screen(void){
  
  int i,j,k;
  vga_modeinfo *minf;
  
  minf = vga_getmodeinfo(CM);
  if(!IS_IN_STANDARD_VGA_DRIVER(CM))
    mcpy(buf_rgb, graph_mem,minf->width*minf->height*minf->bytesperpixel);
  else if (MODEX) {
/*     for(i=0;i<minf->width;i++) */
/*       for(j=0;j<minf->height;j++){ */
/* 	/\* select plane *\/ */
/* 	__svgalib_outseq(0x02,1 << (i & 3)); */
/* 	/\* write color to pixel *\/ */
/* 	*(GM+j * CI.xbytes + (i >> 2))=*(buf_rgb+j*CI.xbytes +j); */
/*       } */
    for(k=0;k<4;k++){
      /* select plane */
      __svgalib_outseq(0x02,1 << k);


      for (i=0;i<minf->width/4;i++)

	for(j=0;j<minf->height;j++)

	  *(GM+j * CI.xbytes + (((i*4)+k) >> 2))=*(buf_rgb+j*minf->width +(i*4)+k);	  
      
    }
  }
  else
    mcpy(buf_rgb, graph_mem,minf->width*minf->height);
}



/*MaRTE OS*/
void refresh_screen_16(void){
  vga_modeinfo *minf;
  
  minf = vga_getmodeinfo(CM);
  copy_videomem_16to16(buf_rgb, graph_mem,minf->width*minf->height*2 );
}

/*MaRTE OS*/
void refresh_slice_16(unsigned long int x1,unsigned long int y1,unsigned long int x2,unsigned long int y2){
  
  vga_modeinfo *minf;
  
  minf = vga_getmodeinfo(CM);
  copy_rect_videomem_16to16(buf_rgb, graph_mem,x1,y1,x2,y2);
}


/*MaRTE OS*/
/*Copia una parte del buffer de la memoria RAM a la memoria de la tarjeta en las coordenadas de pantalla.
  El buffer de lectura es de 24 o 32 bpp
  (x1,y1)	
  ¡-------¡
		¡	¡
		¡_______¡
		(x2,y2)
*/
void copy_rect_videomem_16to16(void *src, void *dst, unsigned long int x1,unsigned long int y1,unsigned long int x2,unsigned long int y2)
{
  int i;
  int width;
	
  width=vga_getxdim();
	
  for(i=0;i<y2-y1;i++)
    {
      copy_videomem_16to16(src+(x1+width*(i+y1))*2,dst+(x1+width*(i+y1))*2,(x2-x1)/2);
    }
}

/*MaRTE OS*/
/*Copia una parte del buffer de la memoria RAM a la memoria de la tarjeta en las coordenadas de pantalla.
  El buffer de lectura es de 16 bpp
  (x1,y1)	
  ¡-------¡
		¡	¡
		¡_______¡
		(x2,y2)
*/
void copy_rect_videomem_32to16(void *src, void *dst, unsigned long int x1,unsigned long int y1,unsigned long int x2,unsigned long int y2)
{
  int i;
  int width;
	
  width=vga_getxdim();
	
  for(i=0;i<y2-y1;i++)
    {
      copy_videomem_32to16(src+(x1+width*(i+y1))*4,dst,(x2-x1));
    }
}

/*MaRTE OS*/
/*Es la funcion que ha de llamarse para iniciar la tarjeta de video "card"
  fabricada por "vendor" en el modo "mode". La parte "con_asociada" se refiere
  al hecho de que se requiere una zona de memoria asociada, que se volcara a
  la tarjeta con las instrucciones "copy_to_"*/

int init_vga_with_associated_mem(int mode,int vendor,int card)
{
  vga_modeinfo *minf;
  unsigned char * memoria;
  	
  VENDOR=vendor;
  CHIPSET=card;

  vga_setmode(mode);
  minf = vga_getmodeinfo(mode);

  if(!IS_IN_STANDARD_VGA_DRIVER(mode))
    if(! (minf->flags & CAPABLE_LINEAR)){
    vga_setmode(TEXT);
    printf("Estoy en modo texto");
    return 1;
  }
  vga_setpage(0);
  if(!IS_IN_STANDARD_VGA_DRIVER(mode))
    if(vga_setlinearaddressing() == -1) {
      vga_setmode(TEXT);
      return 1;
    }
  memoria=vga_getgraphmem();
  //  memset(memoria, 0,minf->width*minf->height*minf->bytesperpixel);
  if(!IS_IN_STANDARD_VGA_DRIVER(mode)){
    buf_rgb = malloc(minf->width*minf->height*minf->bytesperpixel);
    grx_setbuffer(buf_rgb, minf->bytesperpixel,  minf->width, minf->height);
                                     //Init of RGBA version of grx functions
    memset(buf_rgb, 0 ,minf->width*minf->height*minf->bytesperpixel);
  }
  else{
    buf_rgb = malloc(minf->width*minf->height);
    grx_setbuffer(buf_rgb, 1,  minf->width, minf->height);
                          //Init of RGBA version of grx functions
    memset(buf_rgb, 0 ,minf->width*minf->height);
  }
  vga_screenon();
  return 0;
}

/*MaRTE OS*/
/*Es la funcion que ha de llamarse para iniciar la tarjeta de video "card"
  fabricada por "vendor" en el modo "mode". A diferencia de la asociada, esta
  requiere las funciones "vga_..."*/

int init_vga(int mode,int vendor,int card)
{
  vga_modeinfo *minf;
  unsigned char * memoria;

  VENDOR=vendor;
  CHIPSET=card;
  vga_setmode(mode);
  minf = vga_getmodeinfo(mode);

  if(!IS_IN_STANDARD_VGA_DRIVER(mode))
    if(! (minf->flags & CAPABLE_LINEAR)){ 
      vga_setmode(TEXT); 
      printf("Estoy en modo texto"); 
      return 1; 
    };
  vga_setpage(0);
  if(!IS_IN_STANDARD_VGA_DRIVER(mode))
    if(vga_setlinearaddressing() == -1) {
      vga_setmode(TEXT);
      return 1;
    };
  CP=0;
  memoria=vga_getgraphmem();
  memset(memoria,0,minf->width*minf->height*minf->bytesperpixel);
  vga_screenon();
  return 0;
}
