/*----------------------------------------------------------------------------
 *--------------------        M a R T E     O S        -----------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                          'k e y b o a r d _ c'
 *
 *                                   C
 *
 *
 * File 'keyboard.c'                                                   By Mar.
 *
 * Basic PC keyboard functions: initialization and get characters.
 *
 * Function 'initialize_kdb' based on file
 * 'linux/drivers/char/keyboard.c' from Linux sources.
 *
 * Function 'direct_cons_trygetchar' taken from the Flux OSKit.
 * Copyright (c) 1994-1995, 1998 University of Utah and the Flux Group.
 * All rights reserved.
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
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <sys/pio.h>

#define SHIFT -1

/*
 *  Keyboard map
 */
static char keymap[128][2] = {
	{0},			/* 0 */
	{27,	27},		/* 1 - ESC */
	{'1',	'!'},		/* 2 */
	{'2',	'@'},
	{'3',	'#'},
	{'4',	'$'},
	{'5',	'%'},
	{'6',	'^'},
	{'7',	'&'},
	{'8',	'*'},
	{'9',	'('},
	{'0',	')'},
	{'-',	'_'},
	{'=',	'+'},
	{8,	8},		/* 14 - Backspace */
	{'\t',	'\t'},		/* 15 */
	{'q',	'Q'},
	{'w',	'W'},
	{'e',	'E'},
	{'r',	'R'},
	{'t',	'T'},
	{'y',	'Y'},
	{'u',	'U'},
	{'i',	'I'},
	{'o',	'O'},
	{'p',	'P'},
	{'[',	'{'},
	{']',	'}'},		/* 27 */
	{'\r',	'\r'},		/* 28 - Enter */
	{0,	0},		/* 29 - Ctrl */
	{'a',	'A'},		/* 30 */
	{'s',	'S'},
	{'d',	'D'},
	{'f',	'F'},
	{'g',	'G'},
	{'h',	'H'},
	{'j',	'J'},
	{'k',	'K'},
	{'l',	'L'},
	{';',	':'},
	{'\'',	'"'},		/* 40 */
	{'`',	'~'},		/* 41 */
	{SHIFT,	SHIFT},		/* 42 - Left Shift */
	{'\\',	'|'},		/* 43 */
	{'z',	'Z'},		/* 44 */
	{'x',	'X'},
	{'c',	'C'},
	{'v',	'V'},
	{'b',	'B'},
	{'n',	'N'},
	{'m',	'M'},
	{',',	'<'},
	{'.',	'>'},
	{'/',	'?'},		/* 53 */
	{SHIFT,	SHIFT},		/* 54 - Right Shift */
	{0,	0},		/* 55 - Print Screen */
	{0,	0},		/* 56 - Alt */
	{' ',	' '},		/* 57 - Space bar */
	{0,	0},		/* 58 - Caps Lock */
	{0,	0},		/* 59 - F1 */
	{0,	0},		/* 60 - F2 */
	{0,	0},		/* 61 - F3 */
	{0,	0},		/* 62 - F4 */
	{0,	0},		/* 63 - F5 */
	{0,	0},		/* 64 - F6 */
	{0,	0},		/* 65 - F7 */
	{0,	0},		/* 66 - F8 */
	{0,	0},		/* 67 - F9 */
	{0,	0},		/* 68 - F10 */
	{0,	0},		/* 69 - Num Lock */
	{0,	0},		/* 70 - Scroll Lock */
	{'7',	'7'},		/* 71 - Numeric keypad 7 */
	{'8',	'8'},		/* 72 - Numeric keypad 8 */
	{'9',	'9'},		/* 73 - Numeric keypad 9 */
	{'-',	'-'},		/* 74 - Numeric keypad '-' */
	{'4',	'4'},		/* 75 - Numeric keypad 4 */
	{'5',	'5'},		/* 76 - Numeric keypad 5 */
	{'6',	'6'},		/* 77 - Numeric keypad 6 */
	{'+',	'+'},		/* 78 - Numeric keypad '+' */
	{'1',	'1'},		/* 79 - Numeric keypad 1 */
	{'2',	'2'},		/* 80 - Numeric keypad 2 */
	{'3',	'3'},		/* 81 - Numeric keypad 3 */
	{'0',	'0'},		/* 82 - Numeric keypad 0 */
	{'.',	'.'},		/* 83 - Numeric keypad '.' */
};


/*
 * keyboard controller registers
 */
#define KBD_STATUS_REG      (unsigned int) 0x64
#define KBD_CNTL_REG        (unsigned int) 0x64
#define KBD_DATA_REG        (unsigned int) 0x60
/*
 * controller commands
 */
#define KBD_READ_MODE       (unsigned int) 0x20
#define KBD_WRITE_MODE      (unsigned int) 0x60
#define KBD_SELF_TEST       (unsigned int) 0xAA
#define KBD_SELF_TEST2      (unsigned int) 0xAB
#define KBD_CNTL_ENABLE     (unsigned int) 0xAE
/*
 * keyboard commands
 */
#define KBD_ENABLE          (unsigned int) 0xF4
#define KBD_DISABLE         (unsigned int) 0xF5
#define KBD_RESET           (unsigned int) 0xFF
/*
 * keyboard replies
 */
#define KBD_ACK             (unsigned int) 0xFA
#define KBD_POR             (unsigned int) 0xAA
/*
 * status register bits
 */
#define KBD_OBF             (unsigned int) 0x01
#define KBD_IBF             (unsigned int) 0x02
#define KBD_GTO             (unsigned int) 0x40
#define KBD_PERR            (unsigned int) 0x80
/*
 * keyboard controller mode register bits
 */

#define KBD_EKI             (unsigned int) 0x01
#define KBD_SYS             (unsigned int) 0x04
#define KBD_DMS             (unsigned int) 0x20
#define KBD_KCC             (unsigned int) 0x40


#define TIMEOUT_CONST   500000

static int kbd_wait_for_input(void)
{
  int     n;
  int     status, data;

  n = TIMEOUT_CONST;
  do {
    status = inb(KBD_STATUS_REG);
    /*
     * Wait for input data to become available.  This bit will
     * then be cleared by the following read of the DATA
     * register.
     */

    if (!(status & KBD_OBF))
      continue;

    data = inb(KBD_DATA_REG);

    /*
     * Check to see if a timeout error has occurred.  This means
     * that transmission was started but did not complete in the
     * normal time cycle.  PERR is set when a parity error occurred
     * in the last transmission.
     */
    if (status & (KBD_GTO | KBD_PERR)) {
      continue;
    }
    return (data & 0xff);
  } while (--n);
  return (-1);    /* timed-out if fell through to here... */
}

static void kbd_write(int address, int data)
{
  int status;

  do {
    status = inb(KBD_STATUS_REG);  /* spin until input buffer empty*/
  } while (status & KBD_IBF);
  outb(data, address);               /* write out the data*/
}


/*
 * initialize_kdb
 */
int initialize_kdb(void)
{
  unsigned long flags;

  save_flags(flags); cli();

  /* Flush any pending input. */
  while (kbd_wait_for_input() != -1)
    continue;

  // XXX I don't need further configuration
  return(1);

  /*
   * Test the keyboard interface.
   * This seems to be the only way to get it going.
   * If the test is successful a x55 is placed in the input buffer.
   */
  kbd_write(KBD_CNTL_REG, KBD_SELF_TEST);
  if (kbd_wait_for_input() != 0x55) {
    printe("keyboard failed self test.\n");
    restore_flags(flags);
    return(-1);
  }

  /*
   * Perform a keyboard interface test.  This causes the controller
   * to test the keyboard clock and data lines.  The results of the
   * test are placed in the input buffer.
   */
  kbd_write(KBD_CNTL_REG, KBD_SELF_TEST2);
  if (kbd_wait_for_input() != 0x00) {
    printe("keyboard failed self test 2.\n");
    restore_flags(flags);
    return(-1);
  }

  /* Enable the keyboard by allowing the keyboard clock to run. */
  kbd_write(KBD_CNTL_REG, KBD_CNTL_ENABLE);

  /*
   * Reset keyboard. If the read times out
   * then the assumption is that no keyboard is
   * plugged into the machine.
   * This defaults the keyboard to scan-code set 2.
   */
  kbd_write(KBD_DATA_REG, KBD_RESET);
  if (kbd_wait_for_input() != KBD_ACK) {
    printe("reset kbd failed, no ACK.\n");
    restore_flags(flags);
    return(-1);
  }

  if (kbd_wait_for_input() != KBD_POR) {
    printe("reset kbd failed, not POR.\n");
    restore_flags(flags);
    return(-1);
  }

  /*
   * now do a DEFAULTS_DISABLE always
   */
  kbd_write(KBD_DATA_REG, KBD_DISABLE);
  if (kbd_wait_for_input() != KBD_ACK) {
    printe("disable kbd failed, no ACK.\n");
    restore_flags(flags);
    return(-1);
  }

  /*
   * Enable keyboard interrupt, operate in "sys" mode,
   *  enable keyboard (by clearing the disable keyboard bit),
   *  disable mouse, do conversion of keycodes.
   */
  kbd_write(KBD_CNTL_REG, KBD_WRITE_MODE);
  kbd_write(KBD_DATA_REG, KBD_EKI|KBD_SYS|KBD_DMS|KBD_KCC);

  /*
   * now ENABLE the keyboard to set it scanning...
   */
  kbd_write(KBD_DATA_REG, KBD_ENABLE);
  if (kbd_wait_for_input() != KBD_ACK) {
    printe("keyboard enable failed.\n");
    restore_flags(flags);
    return(-1);
  }

  restore_flags(flags);

  return (1);
}

/*
 * keyboard_trygetchar
 *
 * Quick poll for a pending input character.
 * Returns a character if available, -1 otherwise.  This routine can return
 * false negatives in the following cases:
 *
 *	- a valid character is in transit from the keyboard when called
 *	- a key release is received (from a previous key press)
 *	- a SHIFT key press is received (shift state is recorded however)
 *	- a key press for a multi-character sequence is received
 *
 * Yes, this is horrible.
 */
int direct_cons_flags = 0;

int keyboard_trygetchar(void)
{
  static unsigned shift_state;
  unsigned scan_code, ch;

  /* See if a scan code is ready, returning if none. */
  if ((inb(KBD_STATUS_REG) & KBD_OBF) == 0) {
    return -1;
  }
  scan_code = inb(KBD_DATA_REG);


  /* Handle key releases - only release of SHIFT is important. */
  if (scan_code & 0x80) {
    scan_code &= 0x7f;
    if (keymap[scan_code][0] == SHIFT)
      shift_state = 0;
    ch = -1;
  } else {
    /* Translate the character through the keymap. */
    ch = keymap[scan_code][shift_state];
    if (ch == SHIFT) {
      shift_state = 1;
      ch = -1;
    } else if (ch == 0)
      ch = -1;
  }

  return ch;
}
