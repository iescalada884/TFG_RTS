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
 *
 *---------------------------------------------------------------------------
 *
 *                            'r x _ q u e u e . c'
 *
 *                                     C
 *
 *                                               
 * File 'rx_queue.c'                                                  By Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>
 * Body fuctions of the ring queue (see description). 
 *---------------------------------------------------------------------------*/

/* This module implements a ring buffer. Concurrent access is avoid if     */
/* exactly one producer and exactly one consumer are accesing. The reading */
/* process is waiting to consume data that is produced at interrupt time.  */
/* An important consideration of the ring buffer is that we consider the   */
/* buffer empty when the head (current_pos) is == tail (free_pos), and full*/
/* when the (tail + 1) % RING_BUFFER_LEN == head (we waste one element)    */

#include "rx_queue.h"
#include "pio.h"
#include <unistd.h>
#include <time.h>
#include <string.h>
//static pthread_cond_t the_condition;
//static pthread_mutex_t the_mutex;

void init_ring(ring_queue_t *F){

  //  pthread_mutexattr_t attr;

  F->free_pos=0;
  F->current_pos=0;
  /*We assume that gcc initialices the array to 0s*/
  F->blocking_read=TRUE;
  // We inicialize the semaphore.
  sem_init(&sem, 0,0);

}//end function init_ring.


boolean is_ring_empty(ring_queue_t *F){

  return (F->free_pos==F->current_pos);

}

int insert_ring(ring_queue_t *F, eth_frame_t *E){

  if((F->free_pos +1) % RING_BUFFER_LEN == F->current_pos){
/*     return RING_FULL; */
    return -1;
  }else {
    memcpy(F->ring_buffer[F->free_pos].eth_frame,E->eth_frame,E->info_length);
    F->ring_buffer[F->free_pos].info_length=E->info_length;
    F->free_pos=(F->free_pos +1) % RING_BUFFER_LEN;
    sem_post(&sem);
    return 0;
  } //end if-else clause


}

eth_frame_t *alloc_ring_element(ring_queue_t *F){

  eth_frame_t *ret;

  if((F->free_pos +1) % RING_BUFFER_LEN == F->current_pos){
    return RING_FULL;
  }else {
    ret = (eth_frame_t *) &F->ring_buffer[F->free_pos];
    F->free_pos=(F->free_pos +1) % RING_BUFFER_LEN;
    return ret;
  } //end if-else clause

}// end function alloc_ring_element



int read_ring(ring_queue_t *F, char *E ){
  
  int ret;

  while(is_ring_empty(F)){
    if(F->blocking_read==FALSE){
      return RING_EMPTY;
    }
    sem_wait(&sem);
  } // end while
  ret=F->ring_buffer[F->current_pos].info_length;
  memcpy(E, F->ring_buffer[F->current_pos].eth_frame, ret);
  F->current_pos = (F->current_pos +1) % RING_BUFFER_LEN;

  return ret;

}//end function read_ring
