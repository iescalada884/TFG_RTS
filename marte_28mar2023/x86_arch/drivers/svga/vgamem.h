#include <stdint.h>

/*My owner copies to memory*/
void mcpy (void * from, void * to , uint32_t bytes);
void mcolor (void * to, uint32_t color , uint8_t bpp, uint32_t pixels);
