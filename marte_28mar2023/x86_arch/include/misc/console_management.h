/*----------------------------------------------------------------------------
 *------------------------      M a R T E   O S      -------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                   'c o n s o l e _ m a n a g e m e n t'
 *
 *                                    H
 *
 * File 'console_management.h'                                         By MAR.
 *
 * Some utility functions for the text console. They allow changing
 * the text attributes, positioning the cursor on the screen and
 * changing the console input configuration.
 *
 * Functions are defined in the Ada package 'Console_Management' (files
 * 'misc/console_management.ad[s]') then, 'console_management.o'
 * should be linked with any application that uses functionality
 * described in this header file. In order to compile the Ada package
 * (and obtain the object file) just "cd" to directory 'misc/' and
 * execute 'mgnatmake -c console_management.ads'.
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

#ifndef MARTE_CONSOLE_MANAGEMENT_H
#define MARTE_CONSOLE_MANAGEMENT_H

//  Foreground (text) colors
#define BLACK        0x00
#define BLUE         0x01
#define GREEN        0x02
#define CYAN         0x03
#define RED          0x04
#define MAGENTA      0x05
#define BROWN        0x06
#define LIGHTGRAY    0x07
#define DARKGRAY     0x08
#define LIGHTBLUE    0x09
#define LIGHTGREEN   0x0a
#define LIGHTCYAN    0x0b
#define LIGHTRED     0x0c
#define LIGHTMAGENTA 0x0d
#define YELLOW       0x0e
#define WHITE        0x0f

// Background colors
#define BK_BLACK        0x00
#define BK_BLUE         0x10
#define BK_GREEN        0x20
#define BK_CYAN         0x30
#define BK_RED          0x40
#define BK_MAGENTA      0x50
#define BK_BROWN        0x60
#define BK_LIGHTGRAY    0x70

/*
 * Init console management
 */
//extern void console_management___elabb();
//#define INIT_CONSOLE_MANAGEMENT console_management___elabb()
extern void console_management_init();
#define INIT_CONSOLE_MANAGEMENT console_management_init()

/*
 * Clear the screen
 */
void clrscr();
// The whole screen takes the active background color.

/*
 * Change text attributes
 */
void set_text_background_color (unsigned char bkc);
// Establish backgroung color for the characters to be printed on screen.

void set_text_color (unsigned char c);
// Establish foregroung color for the characters to be printed on screen.

void set_highvideo ();
// Establish high video for the characters to be printed on screen.

void set_lowvideo ();
// Establish low video for the characters to be printed on screen.

void set_blink ();
// Establish blinking mode for the characters to be printed on screen.

void cancel_blink ();
// Establish non-blinking mode for the characters to be printed on screen.

void activate_scroll ();
// Deactivates scroll mode for the characters to be printed on screen.

void deactivate_scroll ();
//  Deactivates scroll mode for the characters to be printed on screen.

/*
 *Placing cursor
 */
struct position {
  int row;    // range 0 .. 24
  int column; // range 0 .. 79
};

void set_cursor(struct position * to);
#define set_cursorxy(x,y)   {                               \
                              struct position pos = {y, x}; \
                              set_cursor (&pos);            \
                            }

void get_cursor(struct position * to);

/*
 * Console input configuration
 */
void set_cooked_mode ();
// In this mode line editing is allowed, the textual unit of input is an
// entire "line" of text, where a "line" is a sequence of characters
// terminated by the line termination character CR. Thus, characters
// typed in this mode are not immediately made available to the calling
// program. They are first buffered to allow the user to perform line
// editing (erase characteres) on them.

void set_raw_mode ();
// Every character is made available to the calling program as soon as
// it is typed, so no line editing is available in this mode.

void enable_echo ();
// Echo input characters

void disable_echo ();
// Input characters are not echoed

void set_blocking_mode ();
// Default behaviour. Applications wait in 'getchar' until there are
// characters available.

void reset_blocking_mode ();
// 'getchar' returns -1 inmediately when there is no characters
// available at the moment of the call.

#endif // MARTE_CONSOLE_MANAGEMENT_H
