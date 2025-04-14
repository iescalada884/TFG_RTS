/*!
 * @file board_adlink7841.h
 *
 * @brief driver for the adlink pci7841 can bus board
 *
 * @version 0.01
 *
 * @date 11-Feb-2008
 *
 * @author
 *      Daniel Sangorrin
 *
 * @comments
 *
 * This module contains the implementation of a driver for the card
 * Adlink PCI-7841, which contains two SJA1000 chips for the CAN BUS.
 *
 * @license
 *
 * See MaRTE OS license
 *
 */

#ifndef _MARTE_BOARD_ADLINK7841_H_
#define _MARTE_BOARD_ADLINK7841_H_

#include <drivers/can.h>

#define ADLINK7841_PCI_VENDOR_ID   0x144A
#define ADLINK7841_PCI_DEVICE_ID   0x7841
#define ADLINK7841_OCR_DEFAULT_STD 0xFA // lincan's value 0xFA, juan's was 0x1A

extern int adlink7841_init (const struct pci_device *dev);

#endif // _MARTE_BOARD_ADLINK7841_H_
