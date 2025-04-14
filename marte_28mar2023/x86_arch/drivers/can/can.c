/*!
 * @file can.c
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

#include <drivers/can.h>
#include <misc/freelist.h>
#include <stdlib.h> // NULL
#include "can_debug.h"

struct can_chip_t the_chip_list;
struct can_chip_t the_irq_list[MX_IRQ_LEVELS];

static struct can_frame_t the_frames_pool[CAN_MX_FRAMES];
static freelist_t the_frames_pool_freelist;

#ifdef CAN_FRAMESPOOL_ENABLE_DEBUG
        static int allocated_total = 0;
#endif

/**
 * can_framespool_init - initializes the frames pool
 *
 * the frames pool is common for all the CAN buses installed. If we wanted
 * them to be only for a chip we should put the structures in the can_chip_t
 * structure, but so far I prefer to have a global frame pool.
 */

int can_framespool_init()
{
        DEBUG(CAN_FRAMESPOOL_ENABLE_DEBUG, "initialize freelist\n");
        return freelist_init(&the_frames_pool_freelist, CAN_MX_FRAMES);
}

/**
 * can_framespool_alloc - allocates a frame
 */

struct can_frame_t *can_framespool_alloc()
{
        int pos;

        pos = freelist_alloc(&the_frames_pool_freelist);
        if (pos == -1) return NULL;

#ifdef CAN_FRAMESPOOL_ENABLE_DEBUG
        allocated_total++;
#endif

        DEBUG(CAN_FRAMESPOOL_ENABLE_DEBUG,
              "allocating frame, pos:%d, total:%d\n", pos, allocated_total);

        the_frames_pool[pos].pool_pos = pos; // to know how to free it
        return &the_frames_pool[pos];
}

/**
 * can_framespool_free - free a frame
 */

int can_framespool_free(struct can_frame_t *frame)
{
#ifdef CAN_FRAMESPOOL_ENABLE_DEBUG
        allocated_total--;
#endif
        DEBUG(CAN_FRAMESPOOL_ENABLE_DEBUG,
              "freeing frame, pos:%d, total:%d\n",
              frame->pool_pos, allocated_total);

        return freelist_free(&the_frames_pool_freelist, frame->pool_pos);
}
