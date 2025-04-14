/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                              'membuffer_driver_c'
 *
 *                                      c
 *
 *  File 'membuffer_driver_c.c'                                  by Sangorrin
 *
 *  The implementation is based on the circular_memory_buffers which are
 *  placed in the misc/ directory of MaRTE OS.
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
#include <drivers/drivers_marte.h>
#include <drivers/membuffer_driver.h>
#include <misc/circular_memory_buffer.h>
#include <stdbool.h>

#if 0
#include <stdio.h>
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

static membuffer_t the_mbuff;
static int is_initialized = false;

/**
 * membuffer_driver_create()
 *
 */

int membuffer_driver_create()
{
        int err;
        if (is_initialized == false) {
                err = membuffer_init(&the_mbuff, MBUFFER_SIZE, MBUFFER_CEILING);
                if (err != 0) return err;
                is_initialized = true;
        }
        return 0;
}

/**
 * membuffer_driver_read()
 *
 */

ssize_t membuffer_driver_read(int file_descriptor, void *buffer, size_t bytes)
{
        return membuffer_read(&the_mbuff, buffer, bytes);
}

/**
 * membuffer_driver_write()
 *
 */

ssize_t membuffer_driver_write(int file_descriptor, void *buffer, size_t bytes)
{
        return membuffer_write(&the_mbuff, buffer, bytes);
}

/**
 * membuffer_driver_remove()
 *
 */

int membuffer_driver_remove()
{
        return membuffer_destroy(&the_mbuff);
}
