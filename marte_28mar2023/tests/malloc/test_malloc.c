//  Test for all architectures
#include <stdio.h>
#include <malloc.h>
#include <assert.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define SIZE_A1 100
#define SIZE_A2 1000

int main()
{
   char *a1, *a2;
   int i;

#if MARTE_ARCHITECTURE == ARCH_X86
   SERIAL_CONSOLE_INIT();
#endif

   printf("test for malloc\n");

   // request memory for array a1
   a1 = (char *) malloc(SIZE_A1);
   assert(a1 != NULL);
    
   // request memory for array a2     
   a2 = (char *) malloc(SIZE_A2);
   assert(a2 != NULL);

   // assign value to a1 and a2
   for(i=0; i<SIZE_A1; i++)
      a1[i]=1;
   for(i=0; i<SIZE_A2; i++)
      a2[i]=2;
      
   // make sure values are OK
   for(i=0; i<SIZE_A1; i++)
      assert(a1[i]==1);
   for(i=0; i<SIZE_A2; i++)
      assert(a2[i]==2);
   
   // End of test  
   printf("Test OK\n");
   return 0;
}
