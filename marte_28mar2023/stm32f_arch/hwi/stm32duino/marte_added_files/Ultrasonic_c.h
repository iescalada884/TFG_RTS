/*
 * C wrapper for Ultrasonic.h
 *
 *  A library for the "Grove - ultrasonic ranger"
 *  https://wiki.seeedstudio.com/Grove-Ultrasonic_Ranger/
 */

#ifdef __cplusplus
#  define EXTERNC extern "C"
#else
#  define EXTERNC
#endif


/* Alias for your object in C that hides the implementation */
//typedef void* mylibraryname_mytype_t;
typedef void* Ultrasonic_t;


/* Creates the object using the first constructor */
//EXTERNC mylibraryname_mytype_t mylibraryname_create_mytype_with_int(int val)


EXTERNC Ultrasonic_t ultrasonic_create(int val);

EXTERNC long ultrasonic_measure_in_cm(Ultrasonic_t ultrasonic);







