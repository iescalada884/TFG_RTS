/*

*/
#include "phat.h"

typedef enum
{
	BASE = OUT1, 
	CODO = OUT2,
	PINZA = OUT3
}servo_t;

#define CEROG	500000
#define	NOVENTAG	1500000	
#define CIENTOOCHENTAG	2500000
#define INCREMENTO 11111.1111

 
int hs422_init();

void hs422_changePosition(servo_t servo, int grados);

void hs422_start(servo_t servo);

void hs422_stop(servo_t servo);
