// --------------------------------------------
// Module: rt61-interface.h
// Abstract: this is the interface to use the
// rt61 driver.
// Supported chipsets: RT2561
// --------------------------------------------
//  INDEX:
//  1) HEADERS
//  2) CONSTANTS
//  3) TYPES
//  4) FUNCTIONS
// --------------------------------------------
#ifndef _MARTE_RT61_INTERFACE_H_
#define _MARTE_RT61_INTERFACE_H_
// --------------------------------------------
//  1) HEADERS
// --------------------------------------------
#include <time.h>  // for struct timespec
// --------------------------------------------
//  2) CONSTANTS
// --------------------------------------------
// DEBUG FLAG (0=no  1=yes, print debug messages)
#define CONFIG_RT2X00_DEBUG  0
// Ralink PCI devices demand the Frame size to be a multiple of 128 bytes.
#define HEADER_SIZE 24
#define MAX_DATA_SIZE 2408
// Size of the RX queue
#define RX_QUEUE_SIZE	12
// TX Power constants
#define MIN_TXPOWER	0
#define MAX_TXPOWER	31
#define DEFAULT_TXPOWER	24
// --------------------------------------------
//  3) TYPES
// --------------------------------------------
// A 802.11 frame and the SNR it arrived with
typedef struct {
   unsigned char info [HEADER_SIZE + MAX_DATA_SIZE];
   unsigned short len;
   int ssi;
} wifi_frame_t;
// A mac address consist of 6 bytes
typedef unsigned char mac_address [6];
// type channel is range 1 .. 11; (NOTE: Freq (GHz) = 2.412 + (chan - 1)*5)
// type txpower is range MIN_TXPOWER .. MAX_TXPOWER
// --------------------------------------------
//  4) FUNCTIONS
// --------------------------------------------
//  a) Initialization
extern int rt61_init (int channel, int txpower);
//  b) Configuration
extern void rt61pci_reconfig_channel_txpower (int channel, int txpower);
extern int rt61pci_radio_on (void);
extern int rt61pci_radio_off (void);
extern void rt61pci_enable_promisc (void);
extern void rt61pci_disable_promisc (void);
extern void rt61pci_led_on (void);
extern void rt61pci_led_off (void);
//  c) Receive
extern int rt61pci_recv (wifi_frame_t *frame, const struct timespec *abs_timeout); // NULL blocks
extern int rt61pci_frame_getSourceMac (const wifi_frame_t *frame, mac_address addr);
extern int rt61pci_frame_getData (const wifi_frame_t *frame, unsigned char *buff, const unsigned short nbytes);
//  d) Send
extern int rt61pci_send (const unsigned char *buff, const int nbytes, const mac_address to);

#endif // _MARTE_RT61_INTERFACE_H_

