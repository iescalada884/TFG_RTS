#include <Servo_c.h>
#include <Servo.h>

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

EXTERNC Servo_t servo_create() {
   
     return static_cast<Servo_t>(new Servo());
  
   
 }

EXTERNC void servo_write(Servo_t servo,int value) {
	
	Servo* con = static_cast<Servo*>(servo);
	return con->write(value);

}

EXTERNC uint8_t servo_attach(Servo_t servo,int val){

	Servo* con = static_cast<Servo*>(servo);
	return con->attach(val);
	
}
EXTERNC bool servo_attached(Servo_t servo){

	Servo* con = static_cast<Servo*>(servo);
	return con->attached();

}


