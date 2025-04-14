/*!
 * @file can.h
 *
 * @brief main structures of the can driver for MaRTE OS
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
 * This module contains the most important structures of the can driver.
 *
 * @license
 *
 * See MaRTE OS license
 *
 */

#ifndef _MARTE_CAN_H_
#define _MARTE_CAN_H_

#include <stdint.h>  // uint32_t, uint16_t, uint8_t
#include <stdbool.h> // bool
#include <sys/pci.h> // struct pci_device
#include <intr.h>    // intr_t
#include <misc/linux_list.h> // list_xxx

#define CAN_MX_FRAMES 200
#define CAN_MTU_BYTES 8

/**
 * struct can_frame_t - structure representing a single CAN frame
 *
 * @is_extended_format: if the frame is in extended or standard CAN format
 * @is_rtr: if the frame is a remote transmission request
 * @id: the can identifier
 * @dlc: data lenght code
 * @data: the data
 * @pool_pos: this is for alloc/free frames from a static pool
 *
 * In this structure we store the data contained in a CAN frame, including
 * its id, rtr, dlc and data.
 */

struct can_frame_t {
        int       is_extended_format;
        int       is_rtr;
        uint32_t  id;
        uint8_t   dlc;
        uint8_t   data[8];
        int       pool_pos;
};

/**
 * enum can_chip_type_t - CAN chip types
 */

#define CAN_MX_CHIP_TYPES 1
enum can_chip_type_t {
        SJA1000 = 0,
};

/**
 * struct can_filter_t - filter to select which can frames I want to receive
 */

struct can_filter_t {
        uint32_t mask;
        uint32_t code;
};

/**
 * struct can_chip_ops_t - CAN chip operations structure
 *
 * @init: initializes the chip and prepares it to send and receive frames
 * @send_frame: sends a frame through the chip
 * @abort_frame: sends a command to the chip to abort the frame in the buffer.
 *               If the frame is aborted (if it was being transmited it won't
 *               be aborted) the hook_irq_frame_aborted will be called
 * @set_acceptance_filters: set the acceptance filters
 * @hook_irq_frame_received: called from IRQ when a frame arrives
 * @hook_irq_frame_sent: called from IRQ when a frame has been sent
 * @hook_irq_frame_aborted: called from IRQ when a frame has been aborted
 *
 * Structure that stores pointers to the functions that manage a certain
 * type of CAN bus chip.
 */

struct can_chip_t;

struct can_chip_ops_t {
        int (*init)(struct can_chip_t *chip);

        int (*send_frame)(const struct can_chip_t *chip,
                          const struct can_frame_t *frame);

        int (*abort_frame)(const struct can_chip_t *chip);

        int (*set_acceptance_filters)(const struct can_chip_t *chip,
                                      const struct can_filter_t *filters,
                                      uint32_t len);

        int (*hook_irq_frame_received)(const struct can_chip_t *chip,
                                       struct can_frame_t *frame);

        int (*hook_irq_frame_sent)(const struct can_chip_t *chip);

        int (*hook_irq_frame_aborted)(const struct can_chip_t *chip);
};

/**
 * struct can_chip_t - CAN chip structure
 *
 * @base_addr: chip base address in the CPU IO or memory space
 * @irq: the interrupt associated to this chip
 * @rate: 10, 25, 50, 100, 125, 250, 500, 1000 (default) Kbps
 * @chip_type: the type of the chip (SJA1000, I82527, ...)
 * @chip_data: private data dependent on the chip_type
 * @chip_list: list of chips detected
 * @irq_list: list of chips associated to a certain IRQ level
 * @ops: the chip operations. Not a pointer because hooks may be installed
 *
 * Structure that stores the information associated to a CAN bus chip in
 * a CAN bus board or card.
 */

struct can_chip_t {
        int minor;
        uint32_t base_addr;
        intr_t   irq;
        uint32_t rate;
        enum can_chip_type_t chip_type;
        void *chip_data;
        struct list_head chip_list;
        struct list_head irq_list;
        struct can_chip_ops_t ops;
};

#define MX_IRQ_LEVELS 15
extern struct can_chip_t the_chip_list;
extern struct can_chip_t the_irq_list[MX_IRQ_LEVELS];

/**
 * struct can_board_t - structure representing a supported board
 *
 * @id: the pci vendor, id and a string
 * @init: the function that initializes that type of board
 */

struct can_board_t {
        struct pci_id id;
        int (*init)(const struct pci_device *dev);
};

/**
 * can_framespool_init
 *
 * Initializes a pool of frames that will be managed internally
 */

extern int can_framespool_init();

/**
 * can_framespool_alloc
 *
 * Allocates a frame from the pool of frames. On error it returns NULL
 */

extern struct can_frame_t *can_framespool_alloc();

/**
 * can_framespool_free
 *
 * Frees a frame and returns it to the pool of frames.
 */

extern int can_framespool_free(struct can_frame_t *frame);

/**
 * IOCTL values for the CAN bus
 *
 * CAN_IOCTL_SET_FILTERS: set the acceptance filters of the chip (argument for
 *                        the ioctl: ioctl_filters_t)
 * TODO: explain the ioctls
 */

typedef enum {
        CAN_IOCTL_SET_AB_HOOK = 1<<4,  // set hook "a frame was aborted"
        CAN_IOCTL_SET_TX_HOOK = 1<<3,  // set hook "a frame was transmited"
        CAN_IOCTL_SET_RX_HOOK = 1<<2,  // set hook "a frame was received"
        CAN_IOCTL_ABORT_FRAME = 1<<1,  // abort frame
        CAN_IOCTL_SET_FILTERS = 1      // set filters (struct ioctl_filters_t)
} can_ioctl_options_t;

struct ioctl_filters_t {
        struct can_filter_t *filters;
        uint32_t len;
};

#endif // _MARTE_CAN_H_
