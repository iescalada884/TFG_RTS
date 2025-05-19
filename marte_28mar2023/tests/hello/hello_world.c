//  Test for all architectures

#include <stdio.h>

//#include <misc/debug_marte.h>
#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define TEST_NAME "hello test"


int main ()
{
  

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  printf ("  --------------- "TEST_NAME" ---------------  \n");

  //init_serial_communication_with_gdb (SERIAL_PORT_1);
  //set_break_point_here;

  printf("Hi from c to MARTE OS ^^\n");
  return 0;
}
