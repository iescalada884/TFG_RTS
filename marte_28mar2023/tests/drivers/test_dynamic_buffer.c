// Test for: 

// Not possible to execute automatically the test, it requires to
// install the dynamic_buffer_driver in kernel-devices_table.ads

/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                           'test_dynamic_buffer'
 *
 *                                      C
 *
 *  File 'test_dynamic_buffer.c'                                 By Sangorrin
 *
 *  This test checks the behaviour of the dynamic_buffer_driver
 *
 *      1.- Install the dynamic_buffer_driver in kernel-devices_table.ads
 *      2.- Compile the test 'mgcc test_dynamic_buffer.c'
 *
 *  The test is very simple and consist of:
 *
 *      1.- Open the buffer and set its maximum size
 *      2.- Write and Read some bytes and check they are the same
 *      3.- Write more bytes than the buffer_length and check that only
 *          the first ones are written
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

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#include <drivers/console_switcher.h>
#include <drivers/dynamic_buffer_driver.h>

#define DEVICE_PATH "/dev/buffer"
#define BUFFER_LENGTH 10 /* bytes */

#if 1
#define DEBUG(x,args...) printf("%s: " x, __func__ , ##args)
#else
#define DEBUG(x,args...)
#endif

int main ()
{
        int fd, err;
        int count;
        dyn_buf_length_t buffer_length = BUFFER_LENGTH;
        char write_data[BUFFER_LENGTH + 1] = "0123456789";
        char read_data[BUFFER_LENGTH + 1]  = "0000000000";
        char overflow_data[BUFFER_LENGTH * 2] = "00112233445566778899";

#if MARTE_ARCHITECTURE == ARCH_X86
        SERIAL_CONSOLE_INIT();
#endif

        DEBUG("opening buffer device file\n");
        fd = open(DEVICE_PATH, O_RDWR);
        if (fd == -1) return -1;

        DEBUG("setting size of buffer\n");
        err = ioctl(fd, SETLENGTH, &buffer_length);
        if (err == -1) return -1;

        DEBUG("writing data bellow maximum size of buffer\n");
        count = write(fd, write_data, BUFFER_LENGTH);
        if (count != BUFFER_LENGTH) return -1;

        DEBUG("reading the written data\n");
        count = read(fd, read_data, BUFFER_LENGTH);
        if (count != BUFFER_LENGTH) return -1;

        DEBUG("comparing the written data with the read data\n");
        err = strncmp(write_data, read_data, BUFFER_LENGTH);
        if (err != 0) return -1;

        DEBUG("resetting counters\n");
        err = ioctl(fd, RESETBOTH, NULL);
        if (err == -1) return -1;

        DEBUG("writing data over the maximum size of buffer\n");
        count = write(fd, overflow_data, sizeof(overflow_data));
        DEBUG("count %d\n", count);
        if (count != BUFFER_LENGTH) return -1;

        DEBUG("reading the written data\n");
        count = read(fd, read_data, BUFFER_LENGTH);
        DEBUG("count %d\n", count);
        read_data[count] = '\0';
        DEBUG("%s\n", read_data);
        if (count != BUFFER_LENGTH) return -1;

        DEBUG("closing file\n");
        err = close(fd);
        if (err == -1) return -1;

        printf("Test OK\n");
        return 0;
}
