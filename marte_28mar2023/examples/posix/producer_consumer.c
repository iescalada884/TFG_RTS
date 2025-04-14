// producer_consumer.c

#include <pthread.h>
#include <stdio.h>
#include <misc/error_checks.h>
#include <misc/load.h>

/* A buffer is defined to store the produced data */
/* (up to a maximum of 20) */

struct buffer {
  int num_consumed;
  int num_produced;
  pthread_cond_t cond;
  pthread_mutex_t mutex;
  int data[20]; 
};

void * producer (void *arg)
{
  struct buffer *my_buffer;
  int i;

  my_buffer = (struct buffer *)arg;
  for (i=40; i>20; i--) {
    eat (0.3);
    printf ("producer produces i=%d\n", i);
    CHK( pthread_mutex_lock (&my_buffer->mutex) );
    my_buffer->num_produced++;
    my_buffer->data[my_buffer->num_produced-1] = i;
    CHK( pthread_cond_signal (&my_buffer->cond) );
    CHK( pthread_mutex_unlock (&my_buffer->mutex) );
  }
  pthread_exit (NULL);
  return NULL;
}

void * consumer (void *arg)
{
  struct buffer *my_buffer;
  int i,consumed;

  my_buffer = (struct buffer *)arg;
  for (i=0; i<20; i++) {
    eat (0.5);
    CHK( pthread_mutex_lock (&my_buffer->mutex) );
    while (my_buffer->num_produced <= my_buffer->num_consumed) {
      CHK( pthread_cond_wait (&my_buffer->cond, &my_buffer->mutex) );
    }
    my_buffer->num_consumed++;
    consumed = my_buffer->data[my_buffer->num_consumed - 1];
    CHK( pthread_mutex_unlock (&my_buffer->mutex) );
    printf ("consumer consumes i=%d\n", consumed);
  }
  pthread_exit (NULL);
  return NULL;
}

int main ()
{
  pthread_mutexattr_t mutexattr;
  pthread_condattr_t condattr;
  pthread_attr_t pthreadattr;
  pthread_t t1, t2;
  struct buffer my_buffer;

  adjust ();

  // Init buffer
  my_buffer.num_produced = 0;
  my_buffer.num_consumed = 0;
  CHK( pthread_mutexattr_init (&mutexattr) );
  CHK( pthread_mutex_init (&my_buffer.mutex, &mutexattr) );
  
  CHK( pthread_condattr_init (&condattr) );
  CHK( pthread_cond_init (&my_buffer.cond, &condattr) );
  

  // Create threads
  CHK( pthread_attr_init (&pthreadattr) );
  CHK( pthread_attr_setschedpolicy (&pthreadattr, SCHED_RR) );

  CHK( pthread_create (&t1, &pthreadattr, producer, &my_buffer) );
  CHK( pthread_create (&t2, &pthreadattr, consumer, &my_buffer) );
  
  CHK( pthread_join (t1, NULL) );
  CHK( pthread_join (t2, NULL) );

  return 0;
}
