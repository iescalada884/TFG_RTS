#include "glib.h"

#include "vga.h" 
#include "font.h"
#include "vgamem.h"
#include <stdlib.h>

#define fontaddr       0xffa6eL     /* indirizzo set caratteri         */

uint8_t * flbaddr;
uint8_t BPP;
WORD  bpr;
WORD  height, width;

static void circlepixels(WORD x, WORD y, WORD sx, WORD sy, DWORD c)
{
    grx_pixel(sx + x, sy + y, c);
    grx_pixel(sx - x, sy + y, c);
    grx_pixel(sx + x, sy - y, c);
    grx_pixel(sx - x, sy - y, c);
    grx_pixel(sx + y, sy + x, c);
    grx_pixel(sx - y, sy + x, c);
    grx_pixel(sx + y, sy - x, c);
    grx_pixel(sx - y, sy - x, c);
}

void grx_circumference(WORD sx, WORD sy, WORD r, DWORD c)
{
    int x, y, d;
    
	if (r < 1) {
	grx_pixel(sx, sy, c);
	return;
    }
    x = 0;
    y = r;
    d = 1 - r;
    circlepixels(x, y, sx, sy, c);
    while (x < y) {
	if (d < 0)
	    d += x * 2 + 3;
	else {
	    d += x * 2 - y * 2 + 5;
	    y--;
	}
	x++;
	circlepixels(x, y, sx, sy, c);
    }
}

/* grx_disc by Massy */

static __inline__ void discpixels(WORD x, WORD y, WORD sx, WORD sy, DWORD c)
{
    grx_line(sx + x, sy + y, sx + x, sy - y, c);
    grx_line(sx - x, sy + y, sx - x, sy - y, c);
    grx_line(sx + y, sy + x, sx + y, sy - x , c);
    grx_line(sx - y, sy + x, sx - y, sy - x , c);
}

void grx_circle(WORD sx, WORD sy, WORD r, DWORD c)
{
  int x, y, d;
  
  if (r < 1) {
    grx_pixel(sx, sy, c);
    return;
  }
  x = 0;
  y = r;
  d = 1 - r;
  discpixels(x, y, sx, sy, c);
  while (x < y) {
    if (d < 0)
      d += x * 2 + 3;
    else {
      d += x * 2 - y * 2 + 5;
      y--;
    }
    x++;	
    discpixels(x, y, sx, sy, c);
  }
}

int grx_setbuffer(BYTE *vbuf, BYTE bpp, WORD w, WORD h)
{

  //This functions are designed to work only with 16 bpp
  //Now, finally works with 8, 16 and 32 bpp
	
    flbaddr = vbuf;
    width = w;
    height = h;
    BPP = bpp;
    bpr = BPP * w;
	
    return 1;
    
}

void grx_clear(DWORD color)
{

    grx_rectangle_fill(0, 0, width, height, color);

}

void grx_putimage(WORD x1, WORD y1, WORD x2, WORD y2, BYTE *buf)
{
    
}

void grx_rectangle_fill(WORD x1, WORD y1, WORD x2, WORD y2, DWORD color)
{
    uint8_t * addr;
    int y,d;

    addr = flbaddr + x1*BPP  + bpr * y1;
    d = x2 - x1 + 1;

    for (y = y1; y <= y2; y++) {
	mcolor(addr,color,BPP,d);
	addr += bpr;
    }
}

void grx_rectangle(WORD x1, WORD y1, WORD x2, WORD y2, DWORD color)
{
    uint8_t * addr;
    int d, y, x;
    
    addr = flbaddr + x1 * BPP + bpr * y1;
    d = (x2 - x1)*BPP;

    for (y = y1 ; y <= y2; y++) {
      switch (BPP){
      case 1:
	*(uint8_t *) addr = (uint8_t)color;
	*(uint8_t *) (addr+d) = (uint8_t)color;
	break;
      case 2:
	*(uint16_t *) addr = (uint16_t)color;
	*(uint16_t *) (addr+d) = (uint16_t)color;
	break;
      case 4:
	*(uint32_t *) addr = (uint32_t)color;
	*(uint32_t *) (addr+d) = (uint32_t)color;
	break;
      }
      addr += bpr;
    }

    addr = flbaddr + bpr * y1;
    d=(y2-y1)*bpr;

    for (x = x1 ; x <= x2; x++) {
      switch (BPP){
      case 1:
	*(uint8_t *) (addr+x) = (uint8_t)color;
	*(uint8_t *) (addr+x+d) = (uint8_t)color;
	break;
      case 2:
	*(uint16_t *) (addr+x) = (uint16_t)color;
	*(uint16_t *) (addr+x+d) = (uint16_t)color;
	break;
      case 4:
	*(uint32_t *) (addr+x) = (uint32_t)color;
	*(uint32_t *) (addr+x+d) = (uint32_t)color;
	break;
      }
    }
}

void grx_text(char *text, WORD x, WORD y, DWORD fg, DWORD bg)
{
  BYTE * fp;
  BYTE * addr;
  int r, c, bits;
  
  addr = flbaddr;
  while (*text) {
    fp = (BYTE *)&(font_table[*(BYTE *)text][0]);
    for (r=0; r<8; r++) {
      bits = *(BYTE *)(fp++);
      for (c=0; c<8; c++)
	if (bits & (0x80>>c)){
	  switch (BPP){
	  case 1:
	    *(uint8_t *)(addr+(y+r)*bpr+(x+c)*BPP) = (uint8_t)fg;
	    break;
	  case 2:
	    *(uint16_t *)(addr+(y+r)*bpr+(x+c)*BPP) = (uint16_t)fg;
	    break;
	  case 4:
	    *(uint32_t *)(addr+(y+r)*bpr+(x+c)*BPP) = (uint32_t)fg;
	  }          
	}else{
	  switch (BPP){
	  case 1:
	    *(uint8_t *)(addr+(y+r)*bpr+(x+c)*BPP) = (uint8_t)bg;
	    break;
	  case 2:
	    *(uint16_t *)(addr+(y+r)*bpr+(x+c)*BPP) = (uint16_t)bg;
	    break;
	  case 4:
	    *(uint32_t *)(addr+(y+r)*bpr+(x+c)*BPP) = (uint32_t)bg;
	  }          
	}
    }
    text++;
    x += 8;
  }
}
  
void grx_line(WORD x1, WORD y1, WORD x2, WORD y2, DWORD color)
{
  register int t, distance;
  BYTE * addr;
  int xerr=0, yerr=0, deltax, deltay;
  int incx, incy;
  
  addr = flbaddr;;
  deltax = x2 - x1;			/* compute both distances */
  deltay = y2 - y1;
  
  if (deltax > 0)			/* compute increments */
    incx = 1;
  else if (deltax == 0)
    incx = 0;
  else
    incx = -1;
  
  if (deltay > 0)
    incy = 1;
  else if (deltay == 0)
    incy = 0;
  else
    incy = -1;
  
  deltax = abs(deltax);		/* determine greater distance */
  deltay = abs(deltay);
  if (deltax > deltay)
    distance = deltax;
  else
    distance = deltay;
  
  for (t=0; t<=distance+1; t++) {	/* draw the line */
    switch (BPP){
    case 1:
      *(uint8_t *)(addr+y1*bpr+x1*BPP) = (uint8_t)color;
      break;
    case 2:
      *(uint16_t *)(addr+y1*bpr+x1*BPP) = (uint16_t)color;
      break;
    case 4:
      *(uint32_t *)(addr+y1*bpr+x1*BPP) = (uint32_t)color;
    }          
    xerr += deltax;
    yerr += deltay;
    if (xerr > distance) {
      xerr -= distance;
      x1 += incx;
    }
    if (yerr > distance) {
      yerr -= distance;
      y1 += incy;
    }
  }
}

void grx_pixel(WORD x, WORD y, DWORD color)
{
  switch (BPP){
  case 1:
    *(uint8_t *)(flbaddr+y*bpr+x*BPP) = (uint8_t)color;
    break;
  case 2:
    *(uint16_t *)(flbaddr+y*bpr+x*BPP) = (uint16_t)color;
    break;
  case 4:
    *(uint32_t *)(flbaddr+y*bpr+x*BPP) = (uint32_t)color;
  }          
}

DWORD grx_getpixel(WORD x, WORD y)
{
  switch (BPP){
  case 1:
    return((DWORD)(*(uint8_t *) (flbaddr+y*bpr+x*BPP)));
  case 2:
    return((DWORD)(*(uint16_t *) (flbaddr+y*bpr+x*BPP)));
  case 4:
    return((DWORD)(*(uint32_t *) (flbaddr+y*bpr+x*BPP)));
  }          
  return 0;
}
