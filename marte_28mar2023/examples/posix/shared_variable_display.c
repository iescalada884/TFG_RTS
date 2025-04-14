// shared_variable_display.c
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <misc/load.h>
//#include <debug_marte.h>

struct shared_data {
  pthread_mutex_t mutex;
  int a,b,c,i;
};

#define LOOPS 20

/* Thread that increments the shared variables defined above */
void *incrementer (void *arg)
{
  int i=0;
  struct shared_data *data = (struct shared_data *)arg;

  for (i=0; i<LOOPS; i++) { 
    // remove mutex lock and unlock to see the results when shared
    // data is not protected
    pthread_mutex_lock (&data->mutex);
    data->a++; eat (0.1);
    data->b++; eat (0.1);
    data->c++; eat (0.1);
    data->i++;
    pthread_mutex_unlock (&data->mutex);
    eat (0.1);
  }
  pthread_exit ((void *)0);
  return NULL; // never reached, just to avoid warning
}

/* Thread that displays the values of the shared variables */
void *printer (void *arg)
{
  int i=0;
  struct shared_data *data = (struct shared_data *)arg; // MaRTe

  do {
    pthread_mutex_lock (&data->mutex);
    printf ("a=%d, b=%d, c=%d\n", data->a, data->b, data->c);
    i = data->i;
    pthread_mutex_unlock (&data->mutex);
    eat (0.2);
  } while (i<LOOPS);
  pthread_exit ((void *)0);
  return NULL; // never reached, just to avoid warning
}

/* Main program, that creates the mutex and the two threads */
int main()
{

  pthread_mutexattr_t mutexattr;
  pthread_t t1,t2;
  pthread_attr_t attr;
  struct shared_data data;
  int ret;

  adjust ();

  // For Debugging in x86
  // init_serial_communication_with_gdb (SERIAL_PORT_1);
  // set_break_point_here;

  data.a = 0; data.b = 0; data.c = 0; data.i = 0;
  pthread_mutexattr_init (&mutexattr);
  if ((ret = pthread_mutex_init (&data.mutex,&mutexattr)))
    printf ("error in mutex init:%s\n", strerror (ret));

  // Create round-robin threads
  if ((ret = pthread_attr_init (&attr))) 
    printf ("error in pthread_attr_init:%s\n", strerror (ret));
  if ((ret = pthread_attr_setschedpolicy (&attr, SCHED_RR))) 
    printf ("error in pthread_attr_setpolicy:%s\n", strerror (ret));

  if ((ret = pthread_create (&t1, &attr, incrementer, &data))) {
    printf ("error in pthread_create:%s\n", strerror (ret));
    exit (1);
  } 
  if ((ret = pthread_create (&t2, &attr, printer, &data))) {
    printf ("error in pthread_create:%s\n", strerror (ret));
    exit (1);
  } 

  if ((ret = pthread_join (t1, NULL))) 
    printf ("error in join:%s\n", strerror (ret));
  if ((ret = pthread_join (t2, NULL)))
    printf ("error in join:%s\n", strerror (ret));

  exit (0);
}
