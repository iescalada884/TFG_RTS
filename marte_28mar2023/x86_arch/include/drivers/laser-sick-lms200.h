/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                'l a s e r'
 *
 *                                      C
 *
 *  File 'laser-sick-lms.h'                                 by F.J.Feijoo
 *                                            University of Zaragoza (UNIZAR)
 *
 *  ----------------------------------------------------------------------
 *   Copyright (C) 2000-2007, Universidad de Cantabria, SPAIN
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

#ifndef _LASERDEVICE_H
#define _LASERDEVICE_H

//#define DEBUG 1
// #define O_NOCTTY 0x0010
#define SIG_ERR  (void (*)(int))-1

#define SERIAL_SETATTR              0  /* Set attributes to be
                                          written into the UART
                                          regs */
#define SERIAL_GETATTR              1  /* Get attributes from the
                                          UART registers */
#define SERIAL_SETSPEED             2  /* Set input/output speed */
#define SERIAL_GETSPEED             3  /* Get input/output speed */
#define SERIAL_ENABLE_INTERRUPTS    4  /* Enable interrupts */
#define SERIAL_DISABLE_INTERRUPTS   5  /* Disable interrupts */
#define SERIAL_GET_LAST_LINE_ERROR  6  /* Get error bits in the line
                                          status register for the
                                          last line status error
                                          detected. A pointer to
                                          unsigned char is expected
                                          in the third argument of
                                          ioctl function (arg) */
#define SERIAL_RESET_LAST_LINE_ERROR  7 /* Reset last line error */
#define SERIAL_FLUSH                  8 /* Flush the buffer */
#define SERIAL_SET_REL_TIMEOUT        9 /* Set the relative timeout
                                           for blocking read operations */

#define _POSIX_SOURCE 1 /* POSIX compliant source */
#define FALSE 0
#define TRUE 1
#define MAXRETRY 25
#define MAXNDATA 802
#define MAXVALORES 401
#define STX 0x02   /*every PC->LMS packet is started by STX*/
#define ACKSTX 0x06 /*every PC->LMS packet is started by ACKSTX*/
#define BAUD_500000 512
#define BAUD_38400 256
#define BAUD_19200 128
#define BAUD_9600 64
#define RANGE_100 32
#define RANGE_180 16
#define RES_1_DEG 8
#define RES_0_5_DEG 4
#define RES_0_25_DEG 2
#define MMMODE 1
#define CMMODE 0

typedef unsigned char uchar;

extern void showdata(int len, char *buf);

extern void showLaser(int len, char *buf);

extern int initLMS(const char *serialdev, int baud_sel);

extern int setUnits(int unit);

extern int sendPassword();

extern int setRangeRes(int res);

extern void chkstatus(char byte);

extern int startLMS();

extern void resetLMS();

extern void stopLMS();

extern void connectToLMS(int range_mode, int res_mode, int unit_mode,
                      char * port, int baud_sel);

extern int readLMSdata(uchar* buf);

extern int readLMSdataDemand(uchar* buf);

extern int GetCountLaser();

extern double GetScanResLaser();

extern double GetRangeResLaser();

extern int readLMSValues();

extern int readLMSValuesDemand();

extern int laserazo (int i);

extern int getStatus ();

extern void lockLaser();

extern void unlockLaser();

#endif
