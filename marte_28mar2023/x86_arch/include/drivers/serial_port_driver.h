/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V2.0 2019-05-24
 *
 *                    's e r i a l _ p o r t _ d r i v e r'
 *
 *                                      H
 *
 *  File 'serial_port_driver.h'                               By Fguerreira,
 *                                                               MAR and
 *                                                               MGH.
 *
 *  Values and data types used by the serial port
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

#ifndef _MARTE_SERIAL_PORT_H_
#define _MARTE_SERIAL_PORT_H_

#include <unistd.h>

typedef unsigned int    tcflag_t;
typedef unsigned char   cc_t;
typedef unsigned int    speed_t;

/* Magic control character value to disable the associated feature */
#define VDISABLE    0xFF

/* Input flags (iflag) */
#define  IGNBRK   0x00000001           /* ignore BREAK condition */
#define  BRKINT   0x00000002           /* map BREAK to SIGINTR */
#define  IGNPAR   0x00000004           /* ignore (discard) parity errors */
#define  PARMRK   0x00000008           /* mark parity and framing errors */
#define  INPCK    0x00000010           /* enable checking of parity errors */
#define  ISTRIP   0x00000020           /* strip 8th bit off chars */
#define  INLCR    0x00000040           /* map NL into CR */
#define  IGNCR    0x00000080           /* ignore CR */
#define  ICRNL    0x00000100           /* map CR to NL (ala CRMOD) */
#define  IXON     0x00000200           /* enable output flow control */
#define  IXOFF    0x00000400           /* enable input flow control */
#define  IXANY    0x00000800           /* any char will restart after stop */

/* Output flags (oflag) */
#define  OPOST    0x00000001           /* enable following output processing */
#define  ONLCR    0x00000002           /* map NL to CR-NL (ala CRMOD) */

/* Control flags (cflag) */
#define CSIZE     0x00000300           /* character size mask */
#define CS5       0x00000000               /* 5 bits (pseudo) */
#define CS6       0x00000100               /* 6 bits */
#define CS7       0x00000200               /* 7 bits */
#define CS8       0x00000300               /* 8 bits */
#define CSTOPB    0x00000400           /* send 2 stop bits */
#define CREAD     0x00000800           /* enable receiver */
#define PARENB    0x00001000           /* parity enable */
#define PARODD    0x00002000           /* odd parity, else even */
#define HUPCL     0x00004000           /* hang up on last close */
#define CLOCAL    0x00008000           /* ignore modem status lines */

/* Local flags (lflag) */
#define  ECHOE    0x00000002           /* visually erase chars */
#define  ECHOK    0x00000004           /* echo NL after line kill */
#define  ECHO     0x00000008           /* enable echoing */
#define  ECHONL   0x00000010           /* echo NL even if ECHO is off */
#define  ISIG     0x00000080           /* enable signals INTR, QUIT, [D]SUSP */
#define  ICANON   0x00000100           /* canonicalize input lines */
#define  IEXTEN   0x00000400           /* enable DISCARD and LNEXT */
#define  TOSTOP   0x00400000           /* stop background jobs from output */
#define  NOFLSH   0x80000000           /* don't flush after interrupt */

/* Standard speeds */
#define B50      50
#define B75      75
#define B110     110
#define B150     150
#define B200     200
#define B300     300
#define B600     600
#define B1200    1200
#define B1800    1800
#define B2400    2400
#define B3600    3600
#define B4800    4800
#define B7200    7200
#define B9600    9600
#define B14400   14400
#define B19200   19200
#define B28800   28800
#define B38400   38400
#define B57600   57600
#define B115200  115200

/* Indexes into control characters array (cc) */
#define VEOF     0
#define VEOL     1
#define VERASE   3
#define VKILL    5
#define VINTR    8
#define VQUIT    9
#define VSUSP   10
#define VSTART  12
#define VSTOP   13
#define VMIN    16
#define VTIME   17
#define NCCS    20

/*
 * Standard POSIX terminal I/O parameters structure.
 */

typedef struct {
  tcflag_t  iflag;
  tcflag_t  oflag;
  tcflag_t  cflag;
  tcflag_t  lflag;
  cc_t      cc[NCCS];
  speed_t   ispeed;
  speed_t   ospeed;
} termios_t;

/* Serial Ioctl Options */
//  Any change here should be also performed in
//  'drivers/serial_port/serial_port_driver.ads'
//
// Third argument of ioctl function (arg) must be a pointer to a 'termios_t'
// structure for values SERIAL_SETATTR, SERIAL_GETATTR,
// SERIAL_SETSPEED and SERIAL_GETSPEED.
//
// Third argument of ioctl function (arg) must be a pointer to a
// struct timespec structure for the value SERIAL_SET_REL_TIMEOUT
// A null pointer in this case means no timeout

#define  SERIAL_SETATTR              0  /* Set attributes to be
                                           written into the UART
                                           regs */
#define  SERIAL_GETATTR              1  /* Get attributes from the
                                           UART registers */
#define  SERIAL_SETSPEED             2  /* Set input/output speed */
#define  SERIAL_GETSPEED             3  /* Get input/output speed */
#define  SERIAL_ENABLE_INTERRUPTS    4  /* Enable interrupts */
#define  SERIAL_DISABLE_INTERRUPTS   5  /* Disable interrupts */
#define  SERIAL_GET_LAST_LINE_ERROR  6  /* Get error bits in the line
                                           status register for the
                                           last line status error
                                           detected. A pointer to
                                           unsigned char is expected
                                           in the third argument of
                                           ioctl function (arg) */
#define  SERIAL_RESET_LAST_LINE_ERROR  7  /* Reset last line error */
#define  SERIAL_FLUSH                  8  /* Flush the buffer */
#define  SERIAL_SET_REL_TIMEOUT        9  /* Set the relative timeout
                                             for blocking read operations */

//---------------------//
// function prototypes //
//---------------------//

extern int serial_port_init_console ();

extern void serial_port_end_of_kernel_initialization ();

extern ssize_t serial_port_console_write (int file_descriptor,
                                          void *buffer,
                                          size_t bytes);

extern int serial_port_ioctl (int file_descriptor, int request, void* argp);

#endif /* _MARTE_SERIAL_PORT_H_ */


