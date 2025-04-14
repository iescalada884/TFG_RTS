#include <stdint.h>
#include "vga.h"

void mcpy (void * from, void * to , uint32_t bytes){

  uint8_t *source, *dest;
  uint32_t four_bytes;

  if (bytes<=0)
    return;
  
  source=from;
  dest=to;
  four_bytes=bytes;

  while(four_bytes>=4){
    *(uint32_t *) dest = *(uint32_t *) source;
    source=from+4;
    dest=to+4;
    four_bytes=four_bytes+4;
  }
  while (four_bytes>0){
    *dest = *source;
    four_bytes--;
  }
}


void mcolor (void * to, uint32_t color, uint8_t bpp, uint32_t pixels){

  uint8_t *dest;
  int i;

  if (pixels<=0)
    return;
  
  dest=to;
  
  for(i=0;i<pixels;i++)
    switch (bpp){
    case 1:
      *(uint8_t *) dest = (uint8_t)color;
      dest+=1;
      break;
    case 2:
      *(uint16_t *) dest = (uint16_t)color;
      dest+=2;
      break;
    case 4:
      *(uint32_t *) dest = (uint32_t)color;
      dest+=4;
    }          
  
  dest=to+bpp;
}
