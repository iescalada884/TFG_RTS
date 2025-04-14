/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                          'r i n g _ b u f f e r s'
 *
 *                                      H
 *
 *  File 'ring_buffers.h'                                         By MAR and
 *                                                                   MGH.
 *
 *  Ring buffers intended to share data between the interrupt handler
 *  and the driver functions (read and write).
 *
 *  Implemented in Ada package 'Ring_Buffers'
 *  ('misc/ring_buffers.ad[sb]').
 *
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
 *   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                      Michael Gonzalez Harbour      mgh@unican.es
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
 *---------------------------------------------------------------------------*/
#ifndef __RING_BUFFERS_H__
#define __RING_BUFFERS_H__

#include <sys/types.h>

typedef void * ring_input_buffer_id_t;
typedef void * ring_output_buffer_id_t;

/*---------------------------------------------------------------------------*
 *--  Input Buffers  --------------------------------------------------------*
 *---------------------------------------------------------------------------*/
//
//  Interrupt handler => puts
//  User tasks        => get
//
//  'Get_From_Input_Buffer' blocks the calling task if there is no
//  characters in buffer until a new character is put using
//  'Put_In_Input_Buffer'.

/*-------------------------*
 *--  Init_Input_Buffer  --*
 *-------------------------*/
void init_input_buffer(ring_input_buffer_id_t buff_id);

/*-----------------------------*
 *--  Get_From_Input_Buffer  --*
 *-----------------------------*/
//
//  To be called from a 'read' driver function.
void get_from_input_buffer(ring_input_buffer_id_t buff_id,
			   int             blocking,
			   char            *c,
			   char            *empty,
                           struct timespec *rel_timeout,
                           char            *timedout);
   
/*---------------------------*
 *--  Put_In_Input_Buffer  --*  
 *---------------------------*/
//
//  To be called from an interrupt handler
void put_in_input_buffer(ring_input_buffer_id_t buff_id,
			 char c,
			 int  replace_last);

/*------------------------------*
 *--  Read_From_Input_Buffer  --*
 *------------------------------*/
//  To be called from a 'read' driver function.
//
//  This function shall attempt to read 'Nbytes' bytes from the
//  ring buffer pointed to by 'Buf', into the buffer pointed to by
//  'Buffer_Ptr'.
//
//  Correct behaviour for blocking and not blocking devices. POSIX
//  says: When attempting to read a file (other than a pipe or
//  FIFO) that supports non-blocking reads and has no data
//  currently available:
//
//  * If O_NONBLOCK is set, read() shall return -1 and set errno to
//  [EAGAIN].
//
//  * If O_NONBLOCK is clear, read() shall block the calling thread
//  until some data becomes available.
//
//  * The use of the O_NONBLOCK flag has no effect if there is some
//  data available.
ssize_t  read_from_input_buffer (ring_input_buffer_id_t buff_id,
				 const void      *buf, 
				 size_t          n,
				 int             blocking,
                                 struct timespec *rel_timeout);
   
/*---------------------------------------------------------------------------*
 *--  Output Buffers  -------------------------------------------------------*
 *---------------------------------------------------------------------------*/
//
//  Interrupt handler => gets
//  User tasks        => put
//
//  'Put_In_Output_Buffer' blocks the calling task if there is no
//  free room in buffer until a character is get using
//  'Get_From_Output_Buffer'. 
  
/*--------------------------*
 *--  Init_Output_Buffer  --*
 *--------------------------*/
 void init_output_buffer (ring_output_buffer_id_t buff_id);

/*------------------------------*
 *--  Get_From_Output_Buffer  --*
 *------------------------------*/
//
//  To be called from an interrupt handler
void get_from_output_buffer (ring_output_buffer_id_t buff_id,
			     char *c,
                             int  *empty);

/*----------------------------*
 *--  Put_In_Output_Buffer  --*  
 *----------------------------*/
//
//  To be called from a 'write' driver function.
void put_in_output_buffer (ring_output_buffer_id_t buff_id,
                           char c,
                           int  blocking,
			   void (*send_first_byte)
			                (ring_output_buffer_id_t buff_id),
                           int *full);

/*------------------------------*
 *--  Write_To_Output_Buffer  --*
 *------------------------------*/
//  To be called from a 'write' driver function.
//
//  This function shall attempt to write 'Nbytes' bytes from the
//  buffer pointed to by 'Buffer_Ptr' to the ring buffer pointed to
//  by 'Buf'.
//
//  POSIX says: When attempting to write to a file descriptor
//  (other than a pipe or FIFO) that supports non-blocking writes
//  and cannot accept the data immediately:
//
//  - If the O_NONBLOCK flag is clear, write() shall block the
//  calling thread until the data can be accepted.
//
//  - If the O_NONBLOCK flag is set, write() shall not block the
//  thread. If some data can be written without blocking the
//  thread, write() shall write what it can and return the number
//  of bytes written. Otherwise, it shall return -1 and set errno
//  to [EAGAIN].
ssize_t write_to_output_buffer(ring_output_buffer_id_t buff_id,
			       const void *buf, 
			       size_t      n,
			       int         blocking,
			       void (*send_first_byte)
			                (ring_output_buffer_id_t buff_id));

/*------------------------------------*
 *--  Reset_Last_Interrupt_Pending  --*
 *------------------------------------*/
//
void reset_last_interrupt_pending (ring_output_buffer_id_t buff_id);

#endif  // __RING_BUFFERS_H__
