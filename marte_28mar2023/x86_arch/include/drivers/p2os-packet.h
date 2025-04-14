/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                   'p a c k e t'
 *
 *                                      C
 *
 *  File 'packet.h'                                        by F.J.Feijoo
 *                                            University of Zaragoza (UNIZAR)
 *
 *
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
/*
 *  Player - One Hell of a Robot Server
 *  Copyright (C) 2000
 *     Brian Gerkey, Kasper Stoy, Richard Vaughan, & Andrew Howard
 *
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/*
 * $Id: packet.h,v 1.1 2002/11/29 17:07:43 gerkey Exp $
 *   part of the P2OS parser.  this class has methods for building,
 *   printing, sending and receiving P2OS packets.
 *
 */

#ifndef _PACKET_H
#define _PACKET_H

#define PACKET_LEN 256

typedef struct{
  unsigned char packet[PACKET_LEN];
  unsigned char size;
}P2OSPacket ;

int CalcChkSum(P2OSPacket p);
void Print(P2OSPacket p);
void PrintHex(P2OSPacket p);
int Build( P2OSPacket *p, unsigned char *data, unsigned char datasize );
int Send( int fd, P2OSPacket p );
int Receive( int fd, P2OSPacket *p );
int Check(P2OSPacket p);
int comp_size(P2OSPacket p1, P2OSPacket p2);

#endif
