// --------------------------------------------
// Module: test_rt61
// Abstract: tests for the rt61 module. You can
// select a specific test by defining RT61_TEST
// --------------------------------------------
// Available tests:
#define  RX_TEST_BLOCK  1
#define  RX_TEST_NONBLOCK  2
#define  PROMISC_TEST 3
#define  TX_TEST  4
#define  LEDS_TEST  5
#define  ACK_TEST  6
// RT61_TEST: The test to do this time
#define  RT61_TEST  TX_TEST
// --------------------------------------------
//  1) HEADERS
// --------------------------------------------
#include <stdio.h>
#include <unistd.h>
#include "rt61-interface.h"
// --------------------------------------------
//  2) SIMPLE FUNCTIONS
// --------------------------------------------
static void pause(){
   char key;
   printf(" press Enter...");
   key = getchar();
}
// -----------------------------------------------------------------------
static void msg(char *s){
   printf(s);
   pause();
}
// --------------------------------------------
//  3) RX BLOCKING TEST
// --------------------------------------------
#if RT61_TEST == RX_TEST_BLOCK
int main()
{
   wifi_frame_t frame;
   int i;
   unsigned char buff [MAX_DATA_SIZE];
   mac_address addr;
   int read_bytes;

   msg ("RX TEST BLOCKING for RT61 Module");

   if (rt61_init (5, 27))
      return -1;

   while (1) {
      if (!rt61pci_recv (&frame, NULL)) {
         rt61pci_frame_getSourceMac (&frame, addr);
         read_bytes = rt61pci_frame_getData (&frame, buff, MAX_DATA_SIZE);
         printf ("RECEIVED DATA: ");
         for (i=0; i<read_bytes; i++)
            printf ("%.2X ", buff [i]);
         printf ("  FROM: ");
         for (i=0; i<6; i++)
            printf ("%.2X ", addr [i]);
         printf ("\n");
      }
      sleep (4); // in this time received packets are enqueued
   }
   return 0;
}
// --------------------------------------------
//  4) RX NON BLOCKING TEST
// --------------------------------------------
#elif RT61_TEST == RX_TEST_NONBLOCK
int main()
{
   wifi_frame_t frame;
   int i;
   struct timespec timeout = {1, 0};
   unsigned char buff [MAX_DATA_SIZE];
   mac_address addr;
   int read_bytes;

   msg ("RX TEST NON BLOCKING for RT61 Module");

   if (rt61_init (5, 27))
      return -1;

   while (1) {
      if (clock_gettime (CLOCK_REALTIME, &timeout))
         printf ("error: clock_realtime\n");
      timeout.tv_sec += 1;

      if (!rt61pci_recv (&frame, &timeout)) {
         rt61pci_frame_getSourceMac (&frame, addr);
         read_bytes = rt61pci_frame_getData (&frame, buff, MAX_DATA_SIZE);
         printf ("RECEIVED DATA: ");
         for (i=0; i<read_bytes; i++)
            printf ("%.2X ", buff [i]);
         printf ("  FROM: ");
         for (i=0; i<6; i++)
            printf ("%.2X ", addr [i]);
         printf ("\n");
      }
      sleep (4); // in this time received packets are enqueued
   }
   return 0;
}
// --------------------------------------------------
//  5) PROMISC TEST (please set CONFIG_RT2X00_DEBUG)
// --------------------------------------------------
#elif RT61_TEST == PROMISC_TEST
int main()
{
   msg ("PROMISC TEST for RT61 Module");

   if (rt61_init (5, 27))
      return -1;

   while (1) {
      msg ("Enable promisc mode");
      rt61pci_enable_promisc ();
      msg ("Disable promisc mode");
      rt61pci_disable_promisc ();
   }

   return 0;
}
// --------------------------------------------
//  6) TX TEST
// --------------------------------------------
#elif RT61_TEST == TX_TEST
int main()
{
   unsigned char buff [5] = {0x12, 0x34, 0x56, 0x78, 0x00};
   mac_address mac_dest = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};

   msg ("TX TEST for RT61 Module");

   if (rt61_init (5, 27))
      return -1;

   while (1) {
      msg ("Transmit a packet");

      buff [4] = buff [4] + 1; // to distinguish the packets
      if (rt61pci_send (buff, 5, mac_dest) < 0)
         return -1;
   }
   return 0;
}
// ---------------------------------------------------------------
//  7) CHECK IF I RECEIVE an ACK (please set CONFIG_RT2X00_DEBUG)
// ---------------------------------------------------------------
#elif RT61_TEST == ACK_TEST
int main()
{
   wifi_frame_t frame;
   int i;
   unsigned char buff [MAX_DATA_SIZE];
   mac_address addr;
   mac_address mac_dest = {0x00, 0x80, 0x5A, 0x39, 0x88, 0xAE};
   int read_bytes;

   msg ("ACK TEST for RT61 Module (CHECK IF I RECEIVE an ACK)");

   if (rt61_init (5, 27))
      return -1;

   while (1) {
      if (!rt61pci_recv (&frame, NULL)) {
         rt61pci_frame_getSourceMac (&frame, addr);
         read_bytes = rt61pci_frame_getData (&frame, buff, MAX_DATA_SIZE);
         printf ("RECEIVED DATA: ");
         for (i=0; i<read_bytes; i++)
            printf ("%.2X ", buff [i]);
         printf ("  FROM: ");
         for (i=0; i<6; i++)
            printf ("%.2X ", addr [i]);
         printf ("\n");

      	if (rt61pci_send (buff, 5, mac_dest) < 0)
         	return -1;
      }
      sleep (4); // in this time received packets are enqueued
   }
   return 0;
}
#endif

