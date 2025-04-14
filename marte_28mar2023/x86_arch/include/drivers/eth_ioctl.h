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
 *----------------------------------------------------------------------------
 *
 *                          'e t h _ i o c t l . h'
 *
 *                                     C
 *
 *                                               
 * File 'eth_ioctl.h'                                                 By Chema.
 *                                                          Jose Maria Martinez
 *                                                            <chema@gmx.net>

 *---------------------------------------------------------------------------*/

#ifndef _ETH_IOCTL_H_
#define _ETH_IOCTL_H_

/* Commands explanation.                                                     */

/* ETH_HWADDR : Will write in priv the Physical MAC address of the           */
/*              current Ethernet card. (the size of MAC addr is 48 bits.)    */

/* ETH_BLOCKING_READ : Will turn the read operation blocking. read will not  */
/*                     return til any data is received.                      */

/* ETH_NON_BLOCKIN_READ : Will turn the read operation as non blocking.      */
/*                        The read call will return with or without (in      */
/*                        case not any) data */

/* SET_PROTOCOL_FILTER : Will set the receiving protocol filter. By default  */
/*                       the driver only receives RT_EP packets. You can     */
/*                       configure to receive only packets defined in priv   */
/*                       (unsigned short *) (IP, NETBIOS, etc..              */
/*                       <see include/drivers/if_ether.h> for protocol       */
/*                       numbers). If set to 0, the driver will receive all  */
/*                       protocol packets on the net.                        */

/* GET_PROTOCOL_FILTER : Will write in priv (unsigned short) the current     */
/*                       protocol filter.                                    */
/*                       If protocol 0, the protocol filter is disabled      */


#define ETH_HWADDR             0x500  
#define ETH_BLOCKING_READ      0x501
#define ETH_NON_BLOCKING_READ  0x502
#define SET_PROTOCOL_FILTER    0x503
#define GET_PROTOCOL_FILTER    0x504

#endif
