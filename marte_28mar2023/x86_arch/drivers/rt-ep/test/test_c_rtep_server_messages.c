#include <pthread.h>
#include <stdio.h>
#include <stdint.h>
#include <misc/error_checks.h>
#include <unistd.h>
#include <assert.h>
#include "rtep.h"

void job1 (void);
void job2 (void);
void job3 (void);

int main ()
{
   rtep_station_id_t me;

   if (pthread_setschedprio (pthread_self(), 5) != 0) {
           printf ("ERROR while changing self priority\n");
           return -1;
   }

   // initialization
   rtep_adainit();
   rtep_init_comm();

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
      default:
         sleep (100000);
   };

   // finalization
   rtep_adafinal();
   return 0;
}

void job1 (void)
{
   rtep_station_id_t station;
   uint8_t buffer[20];
   size_t last;
   rtep_priority_t prio;

   printf ("job1\n");

   while (1) {
      rtep_recv_info (&station, 4, buffer, sizeof(buffer), &last, &prio);
      printf ("received data from: %u  with prio: %u last: %d data: ",
              station, prio, last);
      printf ("%s\n", buffer);
   }
}

void job2 (void)
{
   int err = 0;
   rtep_station_id_t dest_station;
   char dest_station_name[] = "broadcast";
   char msg_broadcast[] = "Hi All!";
   char msg_to_3[] = "Hi job3!";
   rtep_budget_t max_allocated_budget = 2; /* packets */
   int server_period_secs = 10; /* 10 secs */
   rtep_priority_t prio = 15; /* high prio */
   rtep_server_id_t server = 34;

   printf ("job2\n");

   dest_station = rtep_get_station_id_by_name
         ((uint8_t *)dest_station_name, sizeof(dest_station_name)-1);

   err = rtep_server_create
            (max_allocated_budget, server_period_secs, 0, prio, &server);
   assert (err == 0);

   while (1) {
      printf ("Sending data to 3\n");
      rtep_server_send_info
            (3, 4, (uint8_t *)msg_to_3,
             sizeof(msg_to_3), server, 0);

      printf ("Sending data to 3\n");
      rtep_server_send_info
            (3, 4, (uint8_t *)msg_to_3,
             sizeof(msg_to_3), server, 0);

      printf ("Sending data to broadcast\n");
      rtep_server_send_info
            (dest_station, 4, (uint8_t *)msg_broadcast,
             sizeof(msg_broadcast), server, 0);

      printf ("Sending Low Prio broadcast\n");
      rtep_send_info
            (dest_station, 4, (uint8_t *)msg_broadcast,
             sizeof(msg_broadcast), 7);
      sleep (8);
   }
}

void job3 (void)
{
   rtep_station_id_t station;
   uint8_t buffer[20];
   size_t last;
   rtep_priority_t prio;

   printf ("job3\n");

   while (1) {
      rtep_recv_info (&station, 4, buffer, sizeof(buffer), &last, &prio);
      printf ("received data from: %u  with prio: %u last: %d data: ",
              station, prio, last);
      printf ("%s\n", buffer);
   }
}
