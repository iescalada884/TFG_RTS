/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *         'T e x t   a n d   S e r i a l   C o n s o l e   D r i v e r'
 *
 *                                      C
 *
 * File 'text_and_serial_console.c'                                    by MAR.
 *
 * Driver that allows switching between serial and text consoles.
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
 *
 *  MaRTE OS web page: http://marte.unican.es
 *  Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
 *                     Michael Gonzalez Harbour      mgh@unican.es
 *
 * MaRTE OS  is free software; you can  redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as published by the
 * Free Software Foundation;  either  version 2, or (at  your option) any
 * later version.
 *
 * MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
 * WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
 * MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
 * General Public License for more details.
 *
 * You should have received  a  copy of  the  GNU General Public  License
 * distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
 * Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
 * 02111-1307, USA.
 *
 * As a  special exception, if you  link this  unit  with other  files to
 * produce an   executable,   this unit  does  not  by  itself cause  the
 * resulting executable to be covered by the  GNU General Public License.
 * This exception does  not however invalidate  any other reasons why the
 * executable file might be covered by the GNU Public License.
 *
 *---------------------------------------------------------------------------*/
#include <sys/types.h>
#include <stdio.h>
#include <drivers/console_switcher.h>
#include <drivers/text_console.h>
#include <drivers/serial_port_driver.h>
#include <drivers/membuffer_driver.h>

/*----------------------*
 *-- Global variables --*
 *----------------------*/

struct console_functions_t {
        int (*create)();
        void (*end_kernel_initialization)();
        ssize_t (*write) (int, void *, size_t);       /* mandatory */
        ssize_t (*write_error) (int, void *, size_t); /* mandatory */
        int (*ioctl) (int, int, void*);
};

struct console_functions_t console_functions [3] =
{
#define TEXT_CONSOLE   0
       {.create = text_console_basic_create,
        .end_kernel_initialization = text_console_end_of_kernel_initialization,
        .write = text_console_write,
        .write_error = text_console_write_error,
        .ioctl = text_console_ioctl},
#define SERIAL_CONSOLE 1
       {.create = serial_port_init_console,
        .write = serial_port_console_write,
        .write_error = serial_port_console_write,
        .ioctl = serial_port_ioctl},
#define MEMBUFFER_CONSOLE 2
       {.create = membuffer_driver_create,
        .write = membuffer_driver_write,
        .write_error = membuffer_driver_write}
};

static int current_console = TEXT_CONSOLE; // text console by default

/*---------------------------------------------------------------------------*
 *--  Text Console Driver Functions  ----------------------------------------*
 *---------------------------------------------------------------------------*/
// 'open', 'close', 'read' and 'remove' not implemented for this driver.

/*--------------*
 *--  Create  --*
 *--------------*/

int console_switcher_create()
{
        if (console_functions[current_console].create != NULL) {
                return console_functions[current_console].create();
        }
        return 0;
}

/*------------------------------------*
 *--  End of kernel initialization  --*
 *------------------------------------*/

void console_switcher_end_of_kernel_initialization()
{
    if (console_functions[current_console].end_kernel_initialization != NULL) {
        console_functions[current_console].end_kernel_initialization();
    }
}

/*-------------*
 *--  Write  --*
 *-------------*/

ssize_t console_switcher_write (int file_descriptor,
                                       void *buffer,
                                       size_t bytes)
{
        if (console_functions[current_console].write == NULL) {
                return -1;
        }

        return console_functions[current_console].write(file_descriptor,
                                                        buffer,
                                                        bytes);
}

/*-------------------*
 *--  Write Error  --*
 *-------------------*/

ssize_t console_switcher_write_error (int file_descriptor,
                                             void *buffer,
                                             size_t bytes)
{
        if (console_functions[current_console].write_error == NULL) {
                return -1;
        }

        return console_functions[current_console].write_error (file_descriptor,
                                                               buffer,
                                                               bytes);
}

/*-------------*
 *--  Ioctl  --*
 *-------------*/

int console_switcher_ioctl (int file_descriptor,
                                   int request,
                                   void *data)
{
        switch (request) {
        case CHANGE_TO_TEXT_CONSOLE:
                console_functions[current_console].write
                        (file_descriptor,
                         "\nConsole output sent to standard console...\n", 44);
                text_console_create ();
                current_console = TEXT_CONSOLE;
                break;

        case CHANGE_TO_SERIAL_CONSOLE:
                console_functions[current_console].write
                        (file_descriptor,
                         "\nConsole output sent to serial port ...\n", 40);
                serial_port_init_console ();
                current_console = SERIAL_CONSOLE;
                console_functions[current_console].write
                        (file_descriptor,
                         "------------------------------------------\n", 43);
                console_functions[current_console].write
                        (file_descriptor,
                         "  MaRTE OS console output...\n", 29);
                break;

        case CHANGE_TO_MEMBUFFER_CONSOLE:
                console_functions[current_console].write
                        (file_descriptor,
                         "\nConsole output sent to membuffer ...\n", 38);
                membuffer_driver_create ();
                current_console = MEMBUFFER_CONSOLE;
                break;

        default:
                if (console_functions[current_console].ioctl != NULL) {
                        return console_functions[current_console].ioctl
                                        (file_descriptor, request, data);
                }
        }
        return 0;
}

void SERIAL_CONSOLE_INIT(void)
{
        ioctl(STDOUT_FILENO, CHANGE_TO_SERIAL_CONSOLE, NULL);
}

void STANDARD_CONSOLE_INIT(void)
{
        ioctl(STDOUT_FILENO, CHANGE_TO_TEXT_CONSOLE, NULL);
}

void MEMBUFFER_CONSOLE_INIT(void)
{
        ioctl(STDOUT_FILENO, CHANGE_TO_MEMBUFFER_CONSOLE, NULL);
}
