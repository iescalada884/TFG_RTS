/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *   Copyright (C) 2003-2005   Universidad de Cantabria, SPAIN
 *
 *   MaRTE OS web page: http://marte.unican.es
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
 *---------------------------------------------------------------------------
 *
 *                            'e t h e r n e t . h'
 *
 *                                     C
 *
 *                                               
 * File 'ethernet.h'                                                  By Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>
 * Driver independent functions to tx and rx data.
 *
 * Body fuctions on 'nic.c'
 *
 *---------------------------------------------------------------------------*/

#ifndef _ETHERNET_H_
#define _ETHERNET_H_

#include <sys/types.h>


/********
  Create
*********/

/* eth_create : This function scans and configure all the Ethernet PCI  */
/*                  ethernet cards in the system. Returns the number of     */
/*                  interfaces found in the station.                        */
/*                  Returns 0 if OK.                                        */

extern ssize_t eth_create(int create_arg);


/******
  Open
*******/

extern ssize_t  eth_open(int file_descriptor, int file_access_mode);

/******
  Read
*******/
/* eth_read : Receives an Ethernet II frame.                            */
/*            The structure of *msg is a Ethernet II frame, as help you */
/*            can use:                                                  */
/*            {                                                         */
/*               struct ethhdr eth_header; // defined in if_ether.h     */
/*               char *info;               // a free reserved area to   */
/*            }                            // store the information.    */
/*            Returns the number of received bytes including the header */
/*            The protocol is in Network byte order !                   */
/*            On error return -1 // pending on a errno implementation   */
/*               on MaRTE*/
/*            The field len is unused.                                  */


extern ssize_t eth_read(int file_descriptor, void *msg, size_t len);

/*******
  Write
********/
/* eth_write : Sends an Ethernet II frame.                               */
/*             *msg - is part of as valid Ethernet frame, it *MUST* have */
/*                    in this order:                                     */
/*            The structure of *msg is as follows:                       */
/*            {                                                          */
/*               struct ethhdr eth_header; // defined in if_ether.h      */
/*               char *info;               // a free reserved area to    */
/*            }                            // store the information.     */
/*            len *MUST* be the size of the ethernet frame.              */
/*            The protocol of ethhdr MUST be in network  byte order.     */
/*            len = ETH_HLEN + Data size.                                */
/*            The protocol is MUST be in network byte order.             */


extern ssize_t eth_write(int file_descriptor, void *msg, size_t len);


/*******
  Close
********/
/* eth_close : Depends on the driver but usually turns off interrupts and  */
/*               stops Tx and Rx engines. */
extern ssize_t eth_close(int file_descriptor);


/*******
  Ioctl
********/

extern ssize_t eth_ioctl(int file_descriptor, ssize_t request, void *priv);


#endif
