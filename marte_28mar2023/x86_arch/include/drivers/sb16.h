#ifndef _MARTE_SB16_H_
#define _MARTE_SB16_H_

#include <sys/cpp_macros.h>
#include <stdint.h>
CPP_BEGIN_DECLS

//  1)  Types
typedef enum {SB16_BITS8 = 0, SB16_BITS16 = 1} sb16_bits_t;
typedef enum {SB16_MONO = 0, SB16_STEREO = 1} sb16_tracks_t;

//  2)  functions
int sb16_probe();

int sb16_init();

int sb16_program(uint16_t rate, //  from 5000 to 44100 hz
                 sb16_bits_t bits, //  8 or 16 bits
                 sb16_tracks_t tracks, // mono, stereo
                 uint8_t *buffer, // < 16mbytes
                 uint16_t length);

int sb16_wait_for_irq();

int sb16_free();

CPP_END_DECLS
#endif /* _MARTE_SB16_H_ */
