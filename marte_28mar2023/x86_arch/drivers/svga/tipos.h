#include <stdint.h>
#ifndef MIS_TIPOS
#define MIS_TIPOS

typedef void *LIN_ADDR;
typedef uint8_t BYTE;
typedef uint16_t WORD;
typedef uint32_t DWORD;
typedef uint64_t QWORD;

#define MAX_DWORD	0xFFFFFFFF
#define MAX_WORD	0xFFFF
#define MAX_BYTE	0xFF
#define MAX_TIME	MAX_DWORD

#ifndef TRUE
    #define TRUE	1
#endif

#ifndef FALSE
    #define FALSE	0
#endif

/*Define un punto*/
typedef struct{
  unsigned int x;
  unsigned int y;
}point_t;

/*Define un struct de puntos*/
typedef struct{
  unsigned int n_points;
  point_t points[0];
}array_points_t;

/*Define un area de pantalla*/
typedef struct{
  unsigned int high;
  unsigned int width;
  char * buffer;
}slice_t;
#endif
