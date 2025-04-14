#include <stdint.h>

void mcpy (void * from, void * to , unsigned int bytes){

  uint8_t *source, *dest;
  unsigned int four_bytes;

  if (bytes<=0)
    return;
  
  source=to;
  dest=from;
  four_bytes=bytes;

  while(four_bytes>=4){
    (uint32_t) *dest = (uint32_t) *source;
    source=to+4;
    dest=from+4;
    four_bytes-=4;
  }
}
