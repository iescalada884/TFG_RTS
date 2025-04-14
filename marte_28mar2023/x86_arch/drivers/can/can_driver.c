/*!
 * @file can_driver.c
 *
 * @brief can driver for MaRTE OS
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
 * This module contains the MaRTE OS filesystem functions to install
 * the driver in MaRTE OS. CAN bus chips are distinguised by using the
 * minor number of the device files.
 *
 * @license
 *
 * See MaRTE OS license
 *
 */

#include <drivers/can.h>
#include "can_debug.h"
#include "can_driver.h"
#include <drivers/drivers_marte.h> // get_minor
#include <sys/marte_configuration_parameters.h> // MINOR_NUMBER_MX
#include <sys/pci.h> // pci_find_device, pci_device, pci_id ...
#include <misc/linux_list.h> // list_xxx

#include "board_adlink7841.h"
#include "chip_sja1000.h"

/**
 * the_boards - list of supported boards
 */

#define CAN_MX_BOARDS 1
static struct can_board_t the_boards[CAN_MX_BOARDS] = {
        {.id = {.vendor = ADLINK7841_PCI_VENDOR_ID,
                .dev_id = ADLINK7841_PCI_DEVICE_ID,
                .name   = "Adlink7841"},
         .init = adlink7841_init},
};

/**
 * the_minors - map minor numbers to chips
 */

static struct can_chip_t *the_minors[MINOR_NUMBER_MX];

/**
 * can_driver_create ()
 *
 * For each supported board, try to find them with pci_find_device. Once
 * we find one call the init function for that kind of board. These functions
 * should allocate the necessary chips.
 *
 * Finally we will traverse the list of detected chips and associate them
 * with a minor number.
 */

int can_driver_create ()
{
        int i;
        int chip_minor;
        struct pci_device *from, dev;
        struct can_chip_t *chip;

        DEBUG(CAN_DRIVER_ENABLE_DEBUG, "Initializing CAN devices\n");

        // initialize the chip lists
        INIT_LIST_HEAD(&the_chip_list.chip_list);
        for (i=0; i<MX_IRQ_LEVELS; i++) {
                INIT_LIST_HEAD(&the_irq_list[i].irq_list);
        }

        // find the present boards with pci_find_device and initialize them
        for(i=0; i<CAN_MX_BOARDS; i++) {
                from = NULL;
                while(pci_find_device(the_boards[i].id.vendor,
                                      the_boards[i].id.dev_id,
                                      from, &dev) == 0) {
                        from = &dev;
                        if (the_boards[i].init (&dev) != 0) {
                                ERROR("%s found but initialization failed\n",
                                      the_boards[i].id.name);
                                continue;
                        }

                        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
                              "%s initialized\n", the_boards[i].id.name);
                }
        }

        if (list_empty(&the_chip_list.chip_list)) {
                ERROR("no chip detected\n");
                return -1;
        }

        // fill the_minors array, which associates minor numbers and chips
        chip_minor = 0;
        list_for_each_entry(chip, &the_chip_list.chip_list, chip_list) {
                the_minors[chip_minor] = chip;
                chip->minor = chip_minor;
                chip_minor++;
        }

        for (i=chip_minor; i < MINOR_NUMBER_MX; i++) {
                the_minors[chip_minor] = NULL;
        }

        // initialize the frames pool
        if (can_framespool_init() != 0) {
                ERROR("could not initialize the frames pool\n");
                return -1;
        }

        return 0;
}

/**
 * can_driver_remove ()
 *
 * TODO: implement it (although maybe it's useless in a bare OS like MaRTE OS)
 *
 **/

int can_driver_remove ()
{
        return 0;
}

/**
 * can_driver_open ()
 *
 * First, we have to get the "minor" number from the file descriptor. This
 * number will tell us what chip to initialize (the one in that position if
 * we go through all boards and chips in order, "chip_number"). When
 * we found it, we call the init functions of that chip.
 *
 * TODO: add a mutex and a "initialized" flag
 *
 **/

int can_driver_open (int fd, int mode)
{
        int ret;
        int chip_minor;
        struct can_chip_t *chip;

        chip_minor = get_minor (fd);
        DEBUG(CAN_DRIVER_ENABLE_DEBUG, "opening device can%d\n", chip_minor);

        // first, we have to find the chip corresponding to the minor number.
        chip = the_minors[chip_minor];
        if (chip == NULL) {
                ERROR("no chip for minor %d\n", chip_minor);
                return -1;
        }

        // now, we call the initialization function of that chip
        if (chip->ops.init == NULL) {
                WARNING("no init function for can%d\n", chip_minor);
                return 0;
        }

        ret = chip->ops.init(chip);
        if (ret != 0) {
                ERROR("initialization of can%d failed\n", chip_minor);
                return -1;
        }

        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
              "device can%d initialized ok\n", chip_minor);
        return 0;
}

int can_driver_close (int fd)
{
        return 0;
}

int can_driver_ioctl (int fd, int request, void *argp)
{
        int ret;
        int chip_minor;
        struct can_chip_t  *chip;
        struct ioctl_filters_t *filters;

        // first, we have to find the chip corresponding to the minor number.
        chip_minor = get_minor (fd);
        chip = the_minors[chip_minor];
        if (chip == NULL) {
                ERROR("no chip for minor %d\n", chip_minor);
                return -1;
        }

        // depending on the request we do the appropiate actions
        switch((can_ioctl_options_t)request) {
                case CAN_IOCTL_SET_FILTERS:
                        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
                              "CAN_IOCTL_SET_FILTERS\n");
                        if (chip->ops.set_acceptance_filters == NULL) {
                                ERROR("no set_acceptance_filters for can%d\n",
                                      chip_minor);
                                return -1;
                        }
                        filters = (struct ioctl_filters_t *)argp;

                        ret = chip->ops.set_acceptance_filters(chip,
                                                               filters->filters,
                                                               filters->len);
                        if (ret != 0) {
                                ERROR("set acceptance filters can%d\n",
                                      chip_minor);
                                return -1;
                        }
                        break;

                case CAN_IOCTL_ABORT_FRAME:
                        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
                              "CAN_IOCTL_ABORT_FRAME\n");
                        if (chip->ops.abort_frame == NULL) {
                                ERROR("no abort_frame hook for can%d\n",
                                      chip_minor);
                                return -1;
                        }

                        ret = chip->ops.abort_frame(chip);
                        if (ret != 0) {
                                ERROR("abort_frame can%d\n", chip_minor);
                                return -1;
                        }
                        break;

                case CAN_IOCTL_SET_RX_HOOK:
                        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
                              "CAN_IOCTL_SET_RX_HOOK\n");
                        chip->ops.hook_irq_frame_received = argp;
                        break;

                case CAN_IOCTL_SET_TX_HOOK:
                        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
                              "CAN_IOCTL_SET_TX_HOOK\n");
                        chip->ops.hook_irq_frame_sent = argp;
                        break;

                case CAN_IOCTL_SET_AB_HOOK:
                        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
                              "CAN_IOCTL_SET_AB_HOOK\n");
                        chip->ops.hook_irq_frame_aborted = argp;
                        break;

                default:
                        ERROR("ioctl request %d is not valid\n", request);
                        return -1;
        }

        return 0;
}

ssize_t can_driver_read (int fd, void *buffer, size_t bytes)
{
        return 0;
}

ssize_t can_driver_write (int fd, void *buffer, size_t bytes)
{
        int ret;
        int chip_minor;
        struct can_chip_t  *chip;
        struct can_frame_t *frame;

        // first, we have to find the chip corresponding to the minor number.
        chip_minor = get_minor (fd);
        chip = the_minors[chip_minor];
        if (chip == NULL) {
                ERROR("no chip for minor %d\n", chip_minor);
                return -1;
        }

        // now, we call the send function of that chip
        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
              "sending frame trough can%d\n", chip_minor);

        if (chip->ops.send_frame == NULL) {
                ERROR("no send_frame function for can%d\n", chip_minor);
                return -1;
        }

        if (bytes != sizeof(struct can_frame_t)) {
                ERROR("buffer has different size than a can frame\n");
                return -1;
        }

        frame = (struct can_frame_t *)buffer;

        ret = chip->ops.send_frame(chip, frame);
        if (ret != 0) {
                ERROR("could not send frame through can%d\n", chip_minor);
                return -1;
        }

        DEBUG(CAN_DRIVER_ENABLE_DEBUG,
              "frame sent through can%d ok\n", chip_minor);
        return (ssize_t)bytes;
}
