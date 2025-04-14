/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                    'T e x t   C o n s o l e   D r i v e r'
 *
 *                                      C
 *
 * File 'text_console.c'                                              by MAR.
 *
 * Driver for the PC text console.
 *
 * Simple console output mechanism that sends text straight to
 * CGA/EGA/VGA video memory.  It has the nice property of being
 * functional right from the start, so it can be used to debug things
 * that happen very early before any devices are initialized.
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
/*
 * Copyright (c) 1994-1995, 1998 University of Utah and the Flux Group.
 * All rights reserved.
 *
 * This file is part of the Flux OSKit.  The OSKit is free software, also known
 * as "open source;" you can redistribute it and/or modify it under the terms
 * of the GNU General Public License (GPL), version 2, as published by the Free
 * Software Foundation (FSF).  To explore alternate licensing terms, contact
 * the University of Utah at csl-dist@cs.utah.edu or +1-801-585-3271.
 *
 * The OSKit is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GPL for more details.  You should have
 * received a copy of the GPL along with the OSKit; see the file COPYING.  If
 * not, write to the FSF, 59 Temple Place #330, Boston, MA 02111-1307, USA.
 */
#include <oskit/x86/base_vm.h>
#include <oskit/x86/pio.h>
#include <sys/types.h>
#include <string.h>
#include <drivers/text_console.h>

/*----------------------*
 *-- Global variables --*
 *----------------------*/
static unsigned int cursor_pos = -1;
static unsigned char text_attr = 0x0f; // white over black
static int scroll_active = 1;
static unsigned char *vidbase;
static unsigned short video_port_reg;
static char initialized = 0;
#define video_port_val (video_port_reg + 1)

#define MONO_BUF ((unsigned char *)phystokv(0xb0000))
#define MONO_BASE 0x3b4
#define CGA_BUF ((unsigned char *)phystokv(0xb8000))
#define CGA_BASE 0x3d4

#define BG_CHAR ((unsigned short)text_attr<<8)
// 0xTT00. 'TT' is the 'text_attr' char (remember: little-endian order)


/*---------------------------------------------------------------------------*
 *--  Auxiliary functions  --------------------------------------------------*
 *---------------------------------------------------------------------------*/

/*-------------*
 *--  fillw  --*
 *-------------*/
static void fillw (unsigned short pat, const void *base, oskit_size_t cnt)
{
        volatile unsigned short *p;

        p = (unsigned short *)base;

        while (cnt--)
                *p++ = pat;
}

/*--------------------*
 *-- set_cursor_pos --*
 *--------------------*/
static void set_cursor_pos(unsigned int pos)
{
        cursor_pos = pos;

        /* set cursor position high byte */
        outb_p(video_port_reg, 0xe);
        outb_p(video_port_val, (unsigned char)((pos >> 8)) & 0xff);

        /* set cursor position low byte */
        outb_p(video_port_reg, 0xf);
        outb_p(video_port_val, (unsigned char)(pos & 0xff));
}

/*--------------------*
 *-- get_cursor_pos --*
 *--------------------*/
// static void get_cursor_pos(unsigned int *pos)
// {
//         unsigned int high_byte, low_byte;
//
//         /* get cursor position high byte */
//         outb_p(video_port_reg, 0xe);
//         high_byte = inb_p(video_port_val);
//
//         /* get cursor position low byte */
//         outb_p(video_port_reg, 0xf);
//         low_byte = inb_p(video_port_val);
//
//         *pos = 0;
//         *pos = (high_byte << 8) + low_byte;
// }
static void get_cursor_pos(unsigned int *pos)
{
        *pos = cursor_pos;
}


/*----------------------------*
 *--  text_console_putchar  --*
 *----------------------------*/
// Here is where the characters are actually written to the video
// memory and the scroll is performed.
static void text_console_putchar(unsigned char c)
{
        switch (c) {
        case '\r':
        case '\n':
                if (cursor_pos / 80 == 24) {
                        // It's in the last line, so scroll screen
                        if (scroll_active) {
                                memcpy(vidbase, vidbase + 80*2, 80*24*2);
                                fillw(BG_CHAR, vidbase + 80*24*2, 80);
                                cursor_pos =  80 * 24;
                        } else {
                                cursor_pos =  0;
                        }
                } else {
                        cursor_pos = (cursor_pos + 80) - (cursor_pos % 80);
                }
                break;

        case '\b':
                if (cursor_pos > 0) cursor_pos--;
                break;
        case '\t':
                do {
                        text_console_putchar(' ');
                } while ((cursor_pos & 7) != 0);
                break;

        default:
        // Stuff the character into the video buffer.
        {
                volatile unsigned char *p = vidbase + cursor_pos * 2;

                p[0] = c;
                p[1] = text_attr;

                // Wrap if we reach the end of a line.
                if (cursor_pos % 80 == 79) {
                        text_console_putchar('\n');
                } else {
                        cursor_pos++;
                }
        }
        }
}


/*---------------------------------------------------------------------------*
 *--  Text Console Driver Functions  ----------------------------------------*
 *---------------------------------------------------------------------------*/
// 'open', 'close', 'read' and 'remove' not implemented for this driver.

/*--------------*
 *--  Create  --*
 *--------------*/
int text_console_create ()
{
        if (!initialized) {
                volatile unsigned short *p = (unsigned short *)CGA_BUF, val;

                val = *p;
                *p = (unsigned short) 0xa55a;
                if (*p != 0xa55a) {
                        vidbase = MONO_BUF;
                        video_port_reg = MONO_BASE;
                } else {
                        *p = val;
                        vidbase = CGA_BUF;
                        video_port_reg = CGA_BASE;
                }

                cursor_pos = 80 * 24;
                text_console_putchar('\n');
                initialized = 1;
        }

        return 0;
}

/*--------------------*
 *--  Basic_Create  --*
 *--------------------*/
int text_console_basic_create ()
{
        text_attr = 0x09; // blue over black
        return text_console_create ();
}

/*------------------------------------*
 *--  End of kernel initialization  --*
 *------------------------------------*/
void text_console_end_of_kernel_initialization ()
{
        text_attr = 0x0f; // white over black
}

/*-------------*
 *--  Write  --*
 *-------------*/
ssize_t text_console_write (int file_descriptor, void *buffer, size_t bytes)
{
        int i = 0;
        for(i=0; i<bytes; ++i) {
                text_console_putchar (((char*)buffer)[i]);
        }
        set_cursor_pos(cursor_pos);
        return bytes;
}

/*-------------------*
 *--  Write_Error  --*
 *-------------------*/
ssize_t text_console_write_error (int file_descriptor, void *buffer,
                                  size_t bytes)
{
        int i = 0, old_text_attr = text_attr;
        text_attr = 0x0c; // light red over black
        for(i=0; i<bytes; ++i) {
                text_console_putchar (((char*)buffer)[i]);
        }
        text_attr = old_text_attr;
        set_cursor_pos(cursor_pos);
        return bytes;
}


/*-------------*
 *--  Ioctl  --*
 *-------------*/
int text_console_ioctl (int file_descriptor, int request, void *datap)
{
        text_console_ioctl_data_t *data = (text_console_ioctl_data_t *)datap;

        switch (request) {
                case TEXT_CONSOLE_SET_CURSOR_POS:
                        set_cursor_pos(data->pos);
                        break;

                case TEXT_CONSOLE_GET_CURSOR_POS:
                        get_cursor_pos(&data->pos);
                        break;

                case TEXT_CONSOLE_ACTIVATE_SCROLL_MODE:
                        scroll_active = 1;
                        break;

                case TEXT_CONSOLE_DEACTIVATE_SCROLL_MODE:
                        scroll_active = 0;
                        break;

                case TEXT_CONSOLE_SET_TEXT_ATTR:
                        text_attr = data->text_attr;
                        break;

                case TEXT_CONSOLE_GET_CHAR_IN_SCREEN_POS:
                {
                        volatile unsigned char *p = vidbase + data->pos * 2;
                        data->character = p[0];
                }
                break;

                case TEXT_CONSOLE_GET_ATTR_OF_SCREEN_POS:
                {
                        volatile unsigned char *p = vidbase + data->pos * 2;
                        data->text_attr = p[1];
                }
                break;

                case TEXT_CONSOLE_CLRSCR:
                        fillw(BG_CHAR, vidbase, 80*25);
                        break;

                default:
                        return -1;
        }
        return 0;
}
