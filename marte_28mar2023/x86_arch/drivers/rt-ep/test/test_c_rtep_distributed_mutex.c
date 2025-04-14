#include <pthread.h>
#include <stdio.h>
#include <stdint.h>
#include <misc/error_checks.h>
#include <unistd.h>
#include "rtep.h"

static rtep_mutex_id_t mutex_id = 3;

void job1 (void)
{
   rtep_priority_t prio = 7;

   printf ("job1\n");
   while (1) {
      sleep (1);
      printf ("Lock\n");
      rtep_lock_distributed_mutex (mutex_id, prio);
      printf ("Mutex FOR ME!\n");
      sleep (2);
      rtep_unlock_distributed_mutex (mutex_id, prio);
      printf ("Mutex RELEASED!\n");
      sleep (4);
   }
}

void job2 (void)
{
   rtep_priority_t prio = 3;

   printf ("job2\n");
   while (1) {
      sleep (1);
      printf ("Lock\n");
      rtep_lock_distributed_mutex (mutex_id, prio);
      printf ("Mutex FOR ME!\n");
      sleep (2);
      rtep_unlock_distributed_mutex (mutex_id, prio);
      printf ("Mutex RELEASED!\n");
      sleep (4);
   }
}

void job3 (void)
{
   rtep_priority_t prio = 5;

   printf ("job3\n");
   while (1) {
      sleep (1);
      printf ("Lock\n");
      rtep_lock_distributed_mutex (mutex_id, prio);
      printf ("Mutex FOR ME!\n");
      sleep (2);
      rtep_unlock_distributed_mutex (mutex_id, prio);
      printf ("Mutex RELEASED!\n");
      sleep (4);
   }
}

int main ()
{
   rtep_station_id_t me;

   /* initialization */
   rtep_adainit();
   rtep_init_comm();

   /* Init the distributed mutex */
   if (rtep_init_distributed_mutex (mutex_id) != 0) {
      printf ("ERROR rtep_init_distributed_mutex\n");
      return -1;
   }

   // call the appropiate procedure
   me = rtep_get_station_id();
   switch (me) {
      case 1:
         job1 ();
         break;
      case 2:
         job2 ();
         break;
      case 3:
         job3 ();
         break;
   };

   // finalization
   rtep_adafinal();
   return 0;
}
