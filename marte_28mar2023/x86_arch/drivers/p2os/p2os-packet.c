/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                   'p a c k e t'
 *
 *                                      C
 *
 *  File 'packet.c'                                        by F.J.Feijoo
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
 * $Id: packet.cc,v 1.5 2006/03/02 21:39:28 reed Exp $
 *   part of the P2OS parser.  this class has methods for building,
 *   printing, sending and receiving P2OS packets.
 *
 */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <drivers/p2os-packet.h>
#include <unistd.h>
#include <stdbool.h>
#include <fcntl.h>
#define ERROR(s) {perror (s); }
//exit (-1);}

int comp_size(P2OSPacket p1, P2OSPacket p2){

    if ( p1.size != p2.size) return(true);

    if ( memcmp( p1.packet, p2.packet, p1.size ) != 0 ) return (false);

    return(false);
  }

void Print(P2OSPacket p) {
  int i;
  if (p.packet) {
    printf("\"");
    for(i=0;i<p.size;i++) {
      printf("%u ", p.packet[i]);
    }
    puts("\"");
  }
}

void PrintHex(P2OSPacket p) {
  int i;
  if (p.packet) {
    printf("\"");
    for(i=0;i<p.size;i++) {
      printf("0x%.2x ", p.packet[i]);
    }
    puts("\"");
  }
}


int Check(P2OSPacket p) {
  short chksum;
  chksum = CalcChkSum(p);

  if ( (chksum == p.packet[p.size-2] << 8) | (p.packet[p.size-1]))
    return(true);


  return(false);
}

int CalcChkSum(P2OSPacket p) {
  unsigned char *buffer = &p.packet[3];
  int c = 0;
  int n;

  n = p.size - 5;

  while (n > 1) {
    c+= (*(buffer)<<8) | *(buffer+1);
    c = c & 0xffff;
    n -= 2;
    buffer += 2;
  }
  if (n>0) c = c^ (int)*(buffer++);

  return(c);
}

int Receive( int fd ,P2OSPacket *p)
{
  unsigned char prefix[3];
  //int skipped=0;
  int cnt;

  memset(p->packet,0,sizeof(p->packet));
//printf("Uno - ");
  do
  {
    memset(prefix,0,sizeof(prefix));
    //memset( prefix, 0, 3);

    while(1)
    {
      cnt = 0;
      //se reciben los dos primeros bytes
      while( cnt!=1 )
      {
	//printf("ENTRO AL WHILE1\n");
        cnt+=read( fd, &prefix[2], 1 );
        if ( cnt < 0 )
        {
          ERROR("Error reading packet header from robot connection: P2OSPacket():Receive():read():");
          //return(1);
        }

	//printf("leo %d\n",(unsigned char)prefix[2]);
      }
      //printf("leido %c \n",prefix[2]);break;

      //espera un 250 y un 251

      if (prefix[0]==0xFA && prefix[1]==0xFB) break;
      prefix[0]=prefix[1];
      prefix[1]=prefix[2];
      //skipped++;
    }
    //if (skipped>3) printf("Skipped %d bytes\n", skipped);

    p->size = prefix[2]+3;
    memcpy( p->packet, prefix, 3);

    cnt = 0;
    while( cnt!=prefix[2] )
    {
      //printf("ENTRO AL WHILE2\n");
      if ( (cnt+=read( fd, &p->packet[3+cnt],  prefix[2]-cnt )) < 0 )
      {
        ERROR("Error reading packet body from robot connection: P2OSPacket():Receive():read():");
        //return(1);
      }
      //printf("leo %d\n",(unsigned char)p->packet[3+cnt]);
    }
  } while (!Check(*p));
//printf(" - Dos \n");
  //printf("Recibido ");
  //PrintHex(*p);

  return(0);

}

int Build( P2OSPacket *p, unsigned char *data, unsigned char datasize ) {
  short chksum;

  p->size = datasize + 5;

  /* header */
  p->packet[0]=0xFA;
  p->packet[1]=0xFB;

  if ( p->size > 198 ) {
    puts("Packet to P2OS can't be larger than 200 bytes");
    return(1);
  }
  p->packet[2] = datasize + 2;

  memcpy( &p->packet[3], data, datasize );

  chksum = CalcChkSum(*p);
  p->packet[3+datasize] = chksum >> 8;
  p->packet[3+datasize+1] = chksum & 0xFF;

  if (!Check(*p)) {
    puts("DAMN");
    return(1);
  }

  return(0);
}

int Send( int fd, P2OSPacket p)
{
  int cnt=0;

  //printf("Send(): ");
  //PrintHex(p);
  while(cnt!=p.size)
  {
    if((cnt += write( fd, p.packet, p.size )) < 0)
    {
      perror("Send");
      return(1);
    }
  }
//despues de cada write un flush!
 /*  if( ioctl(fd, SERIAL_FLUSH, NULL) < 0)
    {
      perror("ERROR cada vez que se envia se hace flush");
      close(fd);
      fd = -1;
      return(1);
    }
*/
   //chapuza, no se vacia la linea serie ---> leemos lo que escribimos para vaciarla
   //fflush a la aragonesa:
  /*
 cnt=0;
   while(cnt!=p.size)
  {
   if((cnt += read( fd, p.packet, p.size )) < 0)
    {
      perror("Vaciando el buffer en write");
      return(1);
    }
   }
 */

  return(0);
}
