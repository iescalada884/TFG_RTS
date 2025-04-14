//  Test for all architectures

#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>
//#include <misc/debug_marte.h>
#include <misc/error_checks.h>
#include <sys/marte_configuration_parameters.h>

#if MARTE_ARCHITECTURE == ARCH_X86
#include <drivers/console_switcher.h>
#endif

#define TEST_NAME "pthread_once test"

volatile int pthread_once_count = 0;
volatile int pthread_once_invocation_count = 0;

pthread_once_t my_once = PTHREAD_ONCE_INIT;
volatile int untouched[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
//  untouched to check PTHREAD_ONCE_INIT doesn write out of the
//  pthread_once_t object

void called_once ()
{
  printf("in called_once A\n");
  sleep (2);
  pthread_once_count++;
  printf("in called_once B\n");
}

void * thread_body (void * arg)
{
  printf("before called_once %d\n", *(int*)arg);
  pthread_once_invocation_count++;
  CHK( pthread_once (&my_once, called_once) );
  printf("after called_once %d\n", *(int*)arg);
  return NULL;
}


int main ()
{
  pthread_t t1, t2, t3;
  pthread_attr_t attr;
  int arg1 = 1, arg2 = 2, arg3 = 3;

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

  printf ("  --------------- "TEST_NAME" ---------------  \n");

  //init_serial_communication_with_gdb (SERIAL_PORT_1);
  //set_break_point_here;

  CHK( pthread_attr_init (&attr) );
  CHK( pthread_create (&t1, &attr, thread_body, &arg1) );
  CHK( pthread_create (&t2, &attr, thread_body, &arg2) );
  CHK( pthread_create (&t3, &attr, thread_body, &arg3) );

  CHK( pthread_join (t1, NULL) );
  CHK( pthread_join (t2, NULL) );
  CHK( pthread_join (t3, NULL) );

  assert(pthread_once_count == 1);
  assert(pthread_once_invocation_count == 3);

  printf("Test OK\n");
  return 0;
}
