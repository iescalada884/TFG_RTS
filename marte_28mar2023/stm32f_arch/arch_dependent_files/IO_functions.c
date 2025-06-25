/****************************************************************************
 *                                                                          *
 *               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                    Copyright (C) 2016-2018, AdaCore                      *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

#include <stddef.h>  // Para size_t
#include <unistd.h>  // Para write y read
#include <sys/stat.h>  // Para struct stat

// Declaraciones de funciones de newlib-bb.c
extern int write_newlib_bb(int fd, char *buf, int nbytes);
extern int read_newlib_bb(int fd, char *buf, int count);

// Redefinición de fwrite
size_t fwrite(const void * ptr, size_t size, size_t count, void * stream) {
    int fd = (int)(size_t)stream;  // Interpretamos el stream como descriptor de archivo (entero)
    int total_bytes = size * count;

    int written = write_newlib_bb(fd, (char *)ptr, total_bytes);
    if (written <= 0) return 0;
    return written/size;
}

// Redefinición de fputc
int fputc(int c, void * stream) {
    int fd = (int)(size_t)stream;  // Interpretamos el stream como descriptor de archivo (entero)
    char ch = (char)c;

    int written = write_newlib_bb(fd, &ch, 1);
    if (written == 1) {
        return c;  // Devuelve el carácter escrito 
    } else {
        return -1;  // Error
    }
}

// Redefinición de fread
size_t fread(void * ptr, size_t size, size_t count, void * stream) {
    int fd = (int)(size_t)stream;  // Interpretamos el stream como descriptor de archivo (entero)
    int total_bytes = size * count;

    int read = read_newlib_bb(fd, (char *)ptr, total_bytes);
    if (read <= 0) return 0;
    return read/size;
}
