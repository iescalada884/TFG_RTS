/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *   Copyright (C) 2003-2005   Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
 *
 *  MaRTE OS  is free software; you can  redistribute it and/or  modify it
 *  under the terms of the GNU General Public License  as published by the
 *  Free Software Foundation;  either  version 2, or (at  your option) any
 *  later version.
 *
 *  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 *  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 *  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 *  General Public License for more details.
 *
 *  You should have received  a  copy of  the  GNU General Public  License
 *  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 *  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 *  02111-1307, USA.
 *
 *  As a  special exception, if you  link this  unit  with other  files to
 *  produce an   executable,   this unit  does  not  by  itself cause  the
 *  resulting executable to be covered by the  GNU General Public License.
 *  This exception does  not however invalidate  any other reasons why the
 *  executable file might be covered by the GNU Public License.
 *----------------------------------------------------------------------------
 *
 *                            'r x _ q u e u e . h'
 *
 *                                     C
 *
 *                                               
 * File 'rx_queue.h'                                                  By Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>
 * Body fuctions on 'rx_queue.c'

 *---------------------------------------------------------------------------*/

#ifndef _MARTE_RX_QUEUE_H
#define _MARTE_RX_QUEUE_H


/* This module implements a RING ring queue. It is intended */
/* to be a RING reception queue with a ring storage behavior*/

#include "eth_defs.h"
#include <semaphore.h>

#define RING_EMPTY -1
#define RING_FULL  NULL

typedef struct {
  unsigned char eth_frame[ETH_FRAME_LEN];
  unsigned int info_length; 
// "unsigned int" is impossed by packetlen in nic.h
}eth_frame_t; 

typedef struct {
  volatile unsigned short int free_pos; /*The position to be filled.*/
  unsigned short int current_pos; /*The first position to be read.*/
  eth_frame_t ring_buffer[RING_BUFFER_LEN];
  /* blocking_read is used to decide if the read operation is blocking*/
  /* de default is blocking. */
  boolean blocking_read;
} ring_queue_t;

sem_t sem;

/*init_ring : This function inicialices the RING queue.*/
/*            you MUST call it BEFORE any access to the queue*/
extern void init_ring(ring_queue_t *F);

/*is_ring_empty : Checks if the queue is empty. Returns FALSE if*/
/*                thereis any frame to be read and TRUE if the  */
/*                queue is empty. */
extern boolean is_ring_empty(ring_queue_t *F);



/* inset_ring : Stores E in the ring buffer F. The free_pos & frames_num */
/*              variables are updated.                                   */ 
/*              Return 0 if OK.                                          */
/*              RING_FULL if no room in buffer.                          */

extern int insert_ring(ring_queue_t *F, eth_frame_t *E);


/* alloc_ring_element : Will return a free eth_frame_t possition on the  */
/*                      ring buffer to write a frame. It is used to loss */
/*                      one memory copy.                                 */
/*                      You have to signal the semaphore in order to     */
/*                      unblock the read_ring operation                  */
extern eth_frame_t *alloc_ring_element(ring_queue_t *F );


/*read_ring : Copies in E the the first-in-frame in the buffer.*/
/*            Returns the number of rx bytes. Updates current_pos */
/*            variable.It works under interrupts inhibited. */
/*            this function is blocking y default. If it is configured*/
/*            non blocking QUEUE_EMPTY is returned when empty.*/
extern int read_ring(ring_queue_t *F, char *E );



#endif /*_MARTE_RX_QUEUE_H*/
