
#include <stdint.h>
#include <stdio.h>

extern void see_memory_c (uint8_t *);

void see_memory_c (uint8_t *ptr) {
   uint8_t *pointer;
   int i, j, base;
   base = ptr;
   for (j=0; j<16; j++){
      for (i=base+j*16; i<base+j*16+16; i++) {
         pointer = i;
         printf("%X ", *pointer);
      }
      printf("\n");
   }
}
