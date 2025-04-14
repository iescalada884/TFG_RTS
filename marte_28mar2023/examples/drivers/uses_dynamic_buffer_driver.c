/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *              'u s e s _ d y n a m i c _ b u f f e r _ d r i v e r'
 *
 *                                      C
 *
 *  File 'uses_dynamic_buffer_driver.c'                      By Fguerreira and
 *                                                              MAR.
 *
 *  This program uses the dynamic buffer driver in
 *  'drivers/dynamic_driver/' directory. In order to run the program
 *  you should install that driver in the system. To do so, uncomment
 *  the appropriate lines in 'kernel/k-devices_table.ads'. After that
 *  you must recompile the kernel (p.e. executing 'mkkernel -gnatn
 *  -gnatp -O3') and then compile this program using 'mgcc'.
 *
 *  For more information about installing drivers in MaRTE OS refer to
 *  the "MaRTE OS User's Guide" (marte_ug.html).
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

#include <drivers/dynamic_buffer_driver.h>

#define ERROR(s) {perror (s); exit (-1);}

int main ()
{
        int fd;
        int count, bytes_to_read;
        const dyn_buf_length_t buffer_length = 4;
        char key;
        char data[40];

        // Open device file
        if ((fd = open ("/dev/buffer", O_RDWR)) == -1)
                ERROR ("open");

        // Set buffer size
        if (ioctl(fd, SETLENGTH, &buffer_length) == -1)
                ERROR ("ioctl");

        // Read or write data
        while (1) {
                printf ("Write, Read or Quit (w,r,q)...");
                key = (char)getchar ();
                getchar (); // to "flush" '\n'
                switch (key) {
                case 'w':
                case 'W':
                        printf ("  Message to write?..."); scanf("%40s", data);
                        if ((count = write(fd, data, strlen (data))) == -1)
                                ERROR ("write");
                        printf ("  Written '%s'(%d bytes) in the device file\n",
                                data, count);
                        break;
                case 'r':
                case 'R':
                        printf ("  Number of characters to read?...");
                        scanf("%d", &bytes_to_read);
                        if ((count = read(fd, data, bytes_to_read)) == -1)
                                ERROR ("read");
                        data[count] = 0; // finish the string
                        printf ("  Read '%s'(%d bytes) from device file\n",
                                data, count);
                        break;
                case 'q':
                case 'Q':
                        goto end;
                        break;
                default:
                        printf ("  Wrong option\n");
                }
        }

 end:
                 if (close(fd) == -1)
                 ERROR ("close");

 return 0;
}
