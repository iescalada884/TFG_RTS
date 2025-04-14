/*---------------------------------------------------------------------------
--                              VGA driver                                 -- 
-----------------------------------------------------------------------------
--                                                                         --
--  My VGA driver for MaRTE OS                                             --
--                                                                         --
--  author:   Álvaro García                                                --
--  website:  www.binarynonsense.com                                       --
--                                                                         --
--  file: vgacore.c                                                        --
--                                                                         --
--                                                                         --
-----------------------------------------------------------------------------
--                      last update: 01 May 2009                           --
---------------------------------------------------------------------------*/

#include <sys/pio.h> //inb,outb,outw
#include <string.h>

#include "vgaregisters.h"
#include "vgacore.h"
//main references:
//http://forum.osdev.org/viewtopic.php?f=1&t=19158&start=0
//http://files.osdev.org/mirrors/geezer/osd/graphics/modes.c
//http://www.brackeen.com/vga/source/djgpp20/modes.c.html

//textmode: http://www.cs.usfca.edu/~cruse/cs686f05/backward.cpp

//the vga identifiers
unsigned int vga_width;
unsigned int vga_height;
unsigned int vga_bpp;
unsigned char *vga_buffer;
unsigned char planes[4*64*1024];//256k mem


void vga_init_registers(unsigned char *regs)
{
   unsigned i;
   /* write MISCELLANEOUS reg */
   outb(VGA_MISC_WRITE, *regs);
   regs++;
   /* write SEQUENCER regs */
   for(i = 0; i < VGA_NUM_SEQ_REGS; i++){
      outb(VGA_SEQ_INDEX, i);
      outb(VGA_SEQ_DATA, *regs);
      regs++;
   }
   /* unlock CRTC registers */
   outb(VGA_CRTC_INDEX, 0x03);
   outb(VGA_CRTC_DATA, inb(VGA_CRTC_DATA) | 0x80);
   outb(VGA_CRTC_INDEX, 0x11);
   outb(VGA_CRTC_DATA, inb(VGA_CRTC_DATA) & ~0x80);
   /* make sure they remain unlocked */
   regs[0x03] |= 0x80;
   regs[0x11] &= ~0x80;
   /* write CRTC regs */
   for(i = 0; i < VGA_NUM_CRTC_REGS; i++){
      outb(VGA_CRTC_INDEX, i);
      outb(VGA_CRTC_DATA, *regs);
      regs++;
   }
   /* write GRAPHICS CONTROLLER regs */
   for(i = 0; i < VGA_NUM_GC_REGS; i++){
      outb(VGA_GC_INDEX, i);
      outb(VGA_GC_DATA, *regs);
      regs++;
   }
   /* write ATTRIBUTE CONTROLLER regs */
   for(i = 0; i < VGA_NUM_AC_REGS; i++){
      inb(VGA_INSTAT_READ);
      outb(VGA_AC_INDEX, i);
      outb(VGA_AC_WRITE, *regs);
      regs++;
   }   
   /* lock 16-color palette and unblank display */
   inb(VGA_INSTAT_READ);
   outb(VGA_AC_INDEX, 0x20);
}

void vga_set_mode_x(int width, int height)
{

    word i;
    dword *ptr=(dword *)vga_buffer;
    
    /* set mode 13 */
//     vga_width=320;
//     vga_height=200;
//     vga_bpp=6;
//     vga_init_registers(mode_320x200x256);
    
    /* turn off chain-4 mode */
    word_out(VGA_SEQ_INDEX , MEMORY_MODE,0x06);
    
    /* set map mask to all 4 planes for screen clearing */
    word_out(VGA_SEQ_INDEX , MAP_MASK, 0xff);
    
    /* clear all 256K of memory */
//     for(i=0;i<0x4000;i++)
//         *ptr++ = 30;
    
    /* turn off long mode */
    word_out(VGA_CRTC_INDEX, UNDERLINE_LOCATION, 0x00);
    
    /* turn on byte mode */
    word_out(VGA_CRTC_INDEX, MODE_CONTROL, 0xe3);
    
    
    vga_width=320;
    vga_height=200;
    
    if (width==360)
    {
        /* turn off write protect */
        word_out(VGA_CRTC_INDEX, V_RETRACE_END, 0x2c);
    
        outb(VGA_MISC_WRITE, 0xe7);
        word_out(VGA_CRTC_INDEX, H_TOTAL, 0x6b);
        word_out(VGA_CRTC_INDEX, H_DISPLAY_END, 0x59);
        word_out(VGA_CRTC_INDEX, H_BLANK_START, 0x5a);
        word_out(VGA_CRTC_INDEX, H_BLANK_END, 0x8e);
        word_out(VGA_CRTC_INDEX, H_RETRACE_START, 0x5e);
        word_out(VGA_CRTC_INDEX, H_RETRACE_END, 0x8a);
        word_out(VGA_CRTC_INDEX, OFFSET, 0x2d);
    
        /* set vertical retrace back to normal */
        word_out(VGA_CRTC_INDEX, V_RETRACE_END, 0x8e);
    
        vga_width=360;
    }
    else
    {
        outb(VGA_MISC_WRITE, 0xe3);
    }
    
    if (height==240 || height==480)
    {
        /* turn off write protect */
        word_out(VGA_CRTC_INDEX, V_RETRACE_END, 0x2c);
    
        word_out(VGA_CRTC_INDEX, V_TOTAL, 0x0d);
        word_out(VGA_CRTC_INDEX, OVERFLOW, 0x3e);
        word_out(VGA_CRTC_INDEX, V_RETRACE_START, 0xea);
        word_out(VGA_CRTC_INDEX, V_RETRACE_END, 0xac);
        word_out(VGA_CRTC_INDEX, V_DISPLAY_END, 0xdf);
        word_out(VGA_CRTC_INDEX, V_BLANK_START, 0xe7);
        word_out(VGA_CRTC_INDEX, V_BLANK_END, 0x06);
        vga_height=height;
    }
    
    if (height==400 || height==480)
    {
        word_out(VGA_CRTC_INDEX, MAX_SCAN_LINE, 0x40);
        vga_height=height;
    }

}

// Updates the hardware cursor.
void vga_text_clear()
{
    unsigned short *text_memory = (unsigned short *)0xB8000;
    //move cursor:
    unsigned char cursor_x = 0;
    unsigned char cursor_y = 0;
    // Make an attribute byte for the default colours
    unsigned char attributeByte = (0 /*black*/ << 4) | (15 /*white*/ & 0x0F);
    unsigned short blank = 0x20 /* space */ | (attributeByte << 8);

    int i;
    for (i = 0; i < 80*25; i++)
    {
        text_memory[i] = blank;
    }

    // Move the hardware cursor back to the start.
    cursor_x = 0;
    cursor_y = 0;
    //move cursor
    // The screen is 80 characters wide...
    unsigned short cursorLocation = cursor_y * 80 + cursor_x;
    outb(VGA_CRTC_INDEX, 14);                  // Tell the VGA board we are setting the high cursor byte.
    outb(VGA_CRTC_DATA, 0); // Send the high cursor byte.
    outb(VGA_CRTC_INDEX, 15);                  // Tell the VGA board we are setting the low cursor byte.
    outb(VGA_CRTC_DATA, 0);      // Send the low cursor byte.
}

void vga_text_planes(int action)
{
    
    if(action ==0){//read
        outw(VGA_SEQ_INDEX,0x0100);  // enter synchronous reset
        outw(VGA_SEQ_INDEX,0x0402);  // write only to map 2
        outw(VGA_SEQ_INDEX,0x0704);  // use sequential addressing
        outw(VGA_SEQ_INDEX,0x0300);  // leave synchronous reset
        outw(VGA_GC_INDEX,0x0204);  // select map 2 for reads
        outw(VGA_GC_INDEX,0x0005);  // disable odd-even addressing
        outw(VGA_GC_INDEX,0x0006);  // map starts at 0xA000:0000  
        memcpy(planes+2*64*1024,vga_buffer,64*1024);
      
        outw(VGA_GC_INDEX,0x0004);  // select map 0 for reads
        outw(VGA_GC_INDEX,0x0005);  // disable odd-even addressing
        outw(VGA_GC_INDEX,0x0006);  // map starts at 0xA000:0000 
        memcpy(planes,vga_buffer,64*1024);
    
        outw(VGA_GC_INDEX,0x0104);  // select map 1 for reads
        outw(VGA_GC_INDEX,0x0005);  // disable odd-even addressing
        outw(VGA_GC_INDEX,0x0006);  // map starts at 0xA000:0000 
        memcpy(planes+64*1024,vga_buffer,64*1024);
    
        outw(VGA_SEQ_INDEX,0x0100);  // enter synchronous reset
        outw(VGA_SEQ_INDEX,0x0302);  // write to maps 0 and 1
        outw(VGA_SEQ_INDEX,0x0304);  // use odd-even addressing
        outw(VGA_SEQ_INDEX,0x0300);  // leave synchronous reset
        outw(VGA_GC_INDEX,0x0004);  // select map 0 for reads
        outw(VGA_GC_INDEX,0x1005);  // enable odd-even addressing
        outw(VGA_GC_INDEX,0x0E06);  // map starts at 0xB800:0000
    }else{//write
        outw(VGA_SEQ_INDEX,0x0100);  // enter synchronous reset
        outw(VGA_SEQ_INDEX,0x0402);  // write only to map 2
        outw(VGA_SEQ_INDEX,0x0704);  // use sequential addressing
        outw(VGA_SEQ_INDEX,0x0300);  // leave synchronous reset
        outw(VGA_GC_INDEX,0x0005);  // disable odd-even addressing
        outw(VGA_GC_INDEX,0x0006);  // map starts at 0xA000:0000  
        memcpy(vga_buffer,planes+2*64*1024,64*1024);
      
        outw(VGA_SEQ_INDEX,0x0002);  // write only to map 2
        outw(VGA_GC_INDEX,0x0005);  // disable odd-even addressing
        outw(VGA_GC_INDEX,0x0006);  // map starts at 0xA000:0000 
        memcpy(vga_buffer,planes,64*1024);
    
        outw(VGA_SEQ_INDEX,0x0102);  // write only to map 2
        outw(VGA_GC_INDEX,0x0005);  // disable odd-even addressing
        outw(VGA_GC_INDEX,0x0006);  // map starts at 0xA000:0000 
        memcpy(vga_buffer,planes+64*1024,64*1024);
    
        outw(VGA_SEQ_INDEX,0x0100);  // enter synchronous reset
        outw(VGA_SEQ_INDEX,0x0302);  // write to maps 0 and 1
        outw(VGA_SEQ_INDEX,0x0304);  // use odd-even addressing
        outw(VGA_SEQ_INDEX,0x0300);  // leave synchronous reset
        outw(VGA_GC_INDEX,0x0004);  // select map 0 for reads
        outw(VGA_GC_INDEX,0x1005);  // enable odd-even addressing
        outw(VGA_GC_INDEX,0x0E06);  // map starts at 0xB800:0000
    }

}

void vga_set_text_planes(unsigned char *buf)
{
    
    outw(VGA_SEQ_INDEX,0x0100);  // enter synchronous reset
    outw(VGA_SEQ_INDEX,0x0402);  // write only to map 2
    outw(VGA_SEQ_INDEX,0x0704);  // use sequential addressing
    outw(VGA_SEQ_INDEX,0x0300);  // leave synchronous reset
    outw(VGA_GC_INDEX,0x0204);  // select map 2 for reads
    outw(VGA_GC_INDEX,0x0005);  // disable odd-even addressing
    outw(VGA_GC_INDEX,0x0006);  // map starts at 0xA000:0000
    
    // modify the contents of the character-generator ram
    // (256 character-glyphs, each is 32 bytes in length) 
//     for (int i = 0; i < 256*32; i++)
//     {
//         unsigned char   orig, revs = 0;
//         orig = vram[ i ];       // fetch the byte
//         for (int j = 0; j < 8; j++)     // rearrange bits
//             if ( orig & (1<<j) ) revs |= 1<<(7-j);
//         vga_buffer[ i ] = revs;       // write new byte
//     }
    memcpy(vga_buffer,buf,4096);

    // epilogue
    outw(VGA_SEQ_INDEX,0x0100);  // enter synchronous reset
    outw(VGA_SEQ_INDEX,0x0302);  // write to maps 0 and 1
    outw(VGA_SEQ_INDEX,0x0304);  // use odd-even addressing
    outw(VGA_SEQ_INDEX,0x0300);  // leave synchronous reset
    outw(VGA_GC_INDEX,0x0004);  // select map 0 for reads
    outw(VGA_GC_INDEX,0x1005);  // enable odd-even addressing
    outw(VGA_GC_INDEX,0x0E06);  // map starts at 0xB800:0000

}

void vga_pixel(int x,int y,int color)
{

  dword offset;

  outb(VGA_SEQ_INDEX, MAP_MASK);          /* select plane */
  outb(VGA_SEQ_DATA,  1 << (x&3) );

  offset=((dword)y*vga_width+x) >> 2;

  vga_buffer[(word)offset]=color;
}

void vga_clear_screen()
{
   unsigned int x=0;
   unsigned int y=0;

   for(y=0; y<vga_height; y++){
      for(x=0; x<vga_width; x++){
         vga_buffer[vga_width*y+x]=0;
      }
   }
}

void vga_clear_screen_modex()
{
   unsigned int x=0;
   unsigned int y=0;

   for(y=0; y<vga_height; y++){
      for(x=0; x<vga_width; x++){
         vga_pixel(x,y,0);
      }
   }
}

void vga_setpalette(int number, int r, int g, int b)
{

    outb(VGA_DAC_WRITE_INDEX,number);
    outb(VGA_DAC_DATA,r);
    outb(VGA_DAC_DATA,g);
    outb(VGA_DAC_DATA,b);
}

/*****************************************************************************
write font to plane P4 (assuming planes are named P1, P2, P4, P8)
*****************************************************************************/
static void set_plane(unsigned p)
{
    unsigned char pmask;

    p &= 3;
    pmask = 1 << p;
    /* set read plane */
    outb(VGA_GC_INDEX, 4);
    outb(VGA_GC_DATA, p);
    /* set write plane */
    outb(VGA_SEQ_INDEX, 2);
    outb(VGA_SEQ_DATA, pmask);
}

static void write_font(unsigned char *buf, unsigned font_height)
{
    unsigned char seq2, seq4, gc4, gc5, gc6;
    unsigned i;

/* save registers
    set_plane() modifies GC 4 and SEQ 2, so save them as well */
    outb(VGA_SEQ_INDEX, 2);
    seq2 = inb(VGA_SEQ_DATA);

    outb(VGA_SEQ_INDEX, 4);
    seq4 = inb(VGA_SEQ_DATA);
/* turn off even-odd addressing (set flat addressing)
    assume: chain-4 addressing already off */
    outb(VGA_SEQ_DATA, seq4 | 0x04);

    outb(VGA_GC_INDEX, 4);
    gc4 = inb(VGA_GC_DATA);

    outb(VGA_GC_INDEX, 5);
    gc5 = inb(VGA_GC_DATA);
    /* turn off even-odd addressing */
    outb(VGA_GC_DATA, gc5 & ~0x10);

    outb(VGA_GC_INDEX, 6);
    gc6 = inb(VGA_GC_DATA);
    /* turn off even-odd addressing */
    outb(VGA_GC_DATA, gc6 & ~0x02);
    /* write font to plane P4 */
    set_plane(2);
    /* write font 0 */
//     for(i = 0; i < 256; i++)
//     {
//         vmemwr(16384u * 0 + i * 32, buf, font_height);
//         buf += font_height;
//     }
    memcpy(vga_buffer,buf,4096);
#if 0
    /* write font 1 */
    for(i = 0; i < 256; i++)
    {
    vmemwr(16384u * 1 + i * 32, buf, font_height);
    buf += font_height;
}
#endif
    /* restore registers */
    outb(VGA_SEQ_INDEX, 2);
    outb(VGA_SEQ_DATA, seq2);
    outb(VGA_SEQ_INDEX, 4);
    outb(VGA_SEQ_DATA, seq4);
    outb(VGA_GC_INDEX, 4);
    outb(VGA_GC_DATA, gc4);
    outb(VGA_GC_INDEX, 5);
    outb(VGA_GC_DATA, gc5);
    outb(VGA_GC_INDEX, 6);
    outb(VGA_GC_DATA, gc6);
}


void vga_init(int mode)
{
    
    vga_buffer=(unsigned char *)0xA0000;//pointer to video memory
    
    switch (mode)
    {
        case TEXT_MODE:
            
            vga_init_registers(mode_80x25_text);
            //vga_set_text_planes(g_8x16_font);
            vga_text_planes(1);
            vga_text_clear();
            vga_setpalette(0, 0, 0, 0);
            vga_setpalette(1, 63, 63, 63);
            vga_setpalette(15, 0, 0, 0);
            //write_font(g_8x16_font, 16);
            //init font
            break;
            
        case G320x200x256:
            
            //setup the vga struct
            vga_text_planes(0);
            vga_width=320;
            vga_height=200;
            vga_bpp=6;
            vga_init_registers(mode_320x200x256);
            break;
            
        case G320x240x256:
            
            //setup the vga struct
            vga_text_planes(0);
            vga_width=320;
            vga_height=200;
            vga_bpp=6;
            vga_init_registers(mode_320x200x256);
            vga_set_mode_x(320,240);
            break;

    }
   
}

// unsigned char *double_buffer;
// 
// ...
// 
// double_buffer = (unsigned char *) malloc(320*200);
// if (double_buffer==NULL)
// {
//   printf("Not enough memory for double buffer.\n");
//   exit(1);
// }

// /* plot a pixel in the double buffer */
// double_buffer[(y<<8) + (y<<6) + x] = color;

// while ((inp(INPUT_STATUS_1) & VRETRACE));
// while (!(inp(INPUT_STATUS_1) & VRETRACE));
// memcpy(VGA,double_buffer,320*200);



