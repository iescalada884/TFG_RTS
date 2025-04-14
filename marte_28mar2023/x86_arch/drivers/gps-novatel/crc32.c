/*----------------------------------------------------------------------------
 *-------------------------      M a R T E   O S      ------------------------
 *----------------------------------------------------------------------------
 *                                                             V1.59B   070502
 *
 *                                'c r c 3 2 . c'
 *
 *                                      C
 *
 *  File 'crc32.c'                                  by F.J.Feijoo
 *                                             University of Zaragoza (UNIZAR)
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

#include "crc32.h"

unsigned long CRC32Value(int i)
{
   int j;
   unsigned long crc;

   crc = i;
   for (j = 8; j > 0; j--){
       if(crc & 1)
           crc = (crc >> 1)^CRC32_POLYNOMIAL;
       else
           crc >>=1;
   }

   return crc;
}


unsigned long CalculateBlockCRC32(unsigned int size, unsigned char * buffer)
{
    unsigned long temp1, temp2, crc = 0;

    while (size-- != 0){
        temp1 = (crc >> 8) & 0x00FFFFFFL;
        temp2 = CRC32Value(((int)crc ^ *buffer++) & 0xff);
        crc = temp1 ^ temp2;
    }
    return crc;
}
