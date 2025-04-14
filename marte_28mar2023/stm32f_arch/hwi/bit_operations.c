/*----------------------------------------------------------------------------
 *--------------------        M a R T E     O S        -----------------------
 *----------------------------------------------------------------------------
 *                                                             V1.0 2010-04-19
 *
 *                      'b i t   o p e r a t i o n s'
 *
 *                                   C
 *
 *
 * File 'bit_operations.c'                                             By Luis
 *
 * ----------------------------------------------------------------------
 *  Copyright (C) 2000-2010, Centro Politécnico Superior, SPAIN
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

unsigned int set__bit__from__zeros (unsigned int Bit) {
	return 1<<Bit;
}

unsigned int reset__bit__from__ones (unsigned int Bit) {
	return ~(1<<Bit);
}

#define ONE 0x00000001

void bit_scan_forward(unsigned int Bit_Field, unsigned int *Bit) {
	int i;
	for (i=0; i<32; i++) {
		if ((Bit_Field&(ONE<<i))!=0) {
			*Bit=i;
			return;
		}
	}

	return;
}

void bit_scan_reverse(unsigned int Bit_Field, unsigned int *Bit) {
	int i;
	for (i=31; i>=0; i--) {
		if ((Bit_Field&(ONE<<i))!=0) {
			*Bit=i;
			return;
		}
	}

	return;
}
