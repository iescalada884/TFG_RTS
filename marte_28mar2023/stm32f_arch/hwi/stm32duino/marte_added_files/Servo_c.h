 #ifdef __cplusplus
 #  define EXTERNC extern "C"
 #else
 #  define EXTERNC
 #endif

#include <inttypes.h>

/* Alias for your object in C that hides the implementation */
//typedef void* mylibraryname_mytype_t;
typedef void* Servo_t;


/* Creates the object using the first constructor */
//EXTERNC mylibraryname_mytype_t mylibraryname_create_mytype_with_int(int val)


EXTERNC Servo_t servo_create();

EXTERNC void servo_write(Servo_t servo,int value);

EXTERNC uint8_t servo_attach(Servo_t servo,int val);

EXTERNC bool servo_attached(Servo_t servo);


