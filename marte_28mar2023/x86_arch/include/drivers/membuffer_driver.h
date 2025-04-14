/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                                  'membuffer_driver'
 *
 *                                      h
 *
 *  File 'membuffer_driver.h'                                  by Sangorrin
 *
 *
 *  This driver creates an internal circular buffer to write and read bytes.
 *  When the buffer is full the bytes are overwritting (putting a driver). It
 *  is aimed at logging applications for example where you write log messages
 *  and then another task reads periodically the log and sends it to another
 *  computer.
 *
 *  The driver has the following operations:
 *
 *      1.- Create: allocates the internal buffer and initializes de buffer
 *      2.- Write: writes data to the buffer
 *      3.- Read: reads (and extracts) data from the buffer
 *      4.- Remove: free the resources allocated by the driver
 *
 *  There are some configurable parameters which are set here:
 *
 *      a) MBUFFER_SIZE: the size of the internal buffer
 *      b) MBUFFER_CEILING: the highest prio of the threads using it
 *      c) The debugging messages
 *
 *  In MaRTE OS examples/ directory there is a logging applications that shows
 *  the use of the driver. There is also a test/ in MaRTE OS tests directory.
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
#ifndef MEMORY_BUFFER_DRIVER_H
#define MEMORY_BUFFER_DRIVER_H

#define MBUFFER_SIZE      100000
#define MBUFFER_CEILING   98

int membuffer_driver_create();
ssize_t membuffer_driver_read(int file_descriptor, void *buffer, size_t bytes);
ssize_t membuffer_driver_write(int file_descriptor, void *buffer, size_t bytes);
int membuffer_driver_remove();

#endif /* MEMORY_BUFFER_DRIVER_H */
