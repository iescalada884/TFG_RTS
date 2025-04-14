#include <Ultrasonic_c.h>
#include <Ultrasonic.h>

/*

 EXTERNC mylibraryname_mytype_t create_mytype_with_int(int val) NOTHROW {
   try {
     return static_cast<mylibraryname_mytype_t>(new MyType(val));
   }
   catch (...) {
       return nullptr;
   }
 }

*/

EXTERNC Ultrasonic_t ultrasonic_create(int pin) {
   
     return static_cast<Ultrasonic_t>(new Ultrasonic(pin));
  
   
 }

EXTERNC long ultrasonic_measure_in_cm(Ultrasonic_t ultrasonic) {
	
	Ultrasonic* con = static_cast<Ultrasonic*>(ultrasonic);
	return con->MeasureInCentimeters();

}

