/*!
 * @file board_adlink7841.c
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

#include <stdlib.h> // malloc
#include <drivers/can.h>
#include "can_debug.h"
#include "board_adlink7841.h"
#include "chip_sja1000.h"

/**
 * adlink7841_init
 *
 * @dev: the pci device data
 *
 * The driver has found an adlink7841 board and we receive a pci_device
 * structure already initialized. We have to create the structures for the
 * two sja1000 chips contained in the board.
 */

int adlink7841_init (const struct pci_device *dev)
{
        struct can_chip_t *chips;
        struct sj1000_chip_data_t *chip_data;

        // allocate memory for the two sja1000 chips
        chips = (struct can_chip_t *)malloc(2 * sizeof(struct can_chip_t));
        if (chips == NULL) {
                ERROR("not enough memory for chips\n");
                return -1;
        }

        chip_data = (struct sj1000_chip_data_t *)
                        malloc(2 * sizeof(struct sj1000_chip_data_t));
        if (chip_data == NULL) {
                ERROR("not enough memory for chip_data\n");
                return -1;
        }

        // initialize the chip data with appropiate values
        chips[0].base_addr = dev->pci_region[2].base_addr;
        chips[0].irq       = dev->irq;
        chips[0].rate      = 1000;
        chips[0].chip_type = SJA1000;
        chips[0].chip_data = (void *)&chip_data[0];
        chips[0].ops = the_sja1000_ops;

        chip_data[0].sja1000_cdr_reg        = sjaCDR_CBP | sjaCDR_CLK_OFF;
        chip_data[0].sja1000_ocr_reg        = ADLINK7841_OCR_DEFAULT_STD;

        chips[1]     = chips[0];
        chip_data[1] = chip_data[0];

        chips[1].base_addr = chips[1].base_addr + 128;
        chips[1].chip_data = (void *)&chip_data[1];

        // add the chips to the_chip_list
        list_add_tail(&chips[0].chip_list, &the_chip_list.chip_list);
        list_add_tail(&chips[1].chip_list, &the_chip_list.chip_list);

        DEBUG(ADLINK7841_ENABLE_DEBUG, "Adlink7841 initialized\n");
        return 0;
}
