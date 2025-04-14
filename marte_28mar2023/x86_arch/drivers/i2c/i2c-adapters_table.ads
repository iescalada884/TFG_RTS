------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                       i 2 c . a d a p t e r s  _ t a b l e
--
--                                    spec
--
-- File 'i2c-adapters_table.ads'                                 By Sangorrin
--
-- This file is a table of the available adapters. If you want to add a new
-- adapter you have to add an Identificator in the 'I2C_Adapter_ID' type at
-- 'i2c.ads' and 'i2c.h' so it can be used by Ada and C drivers.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2004   Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http:--marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael González Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------
with I2C.Adapters; use I2C.Adapters;

-- Withs of the Adapters
with I2C_Elite;
with I2C_Parport;
with I2C_Pcm3718;

package I2C.Adapters_Table is

	type I2C_Adapter_List is array (I2C_Adapter_ID) of I2C_Adapter_Ref;
	
	The_I2C_Adapter_List : I2C_Adapter_List := 
		(Elite => I2C_Elite.I2C_Adapter_BIT_Elite'Access,
		 Parport => I2C_Parport.I2C_Adapter_BIT_Parport'Access,
		 Pcm3718 => I2C_Pcm3718.I2C_Adapter_BIT_Pcm3718'Access);

end I2C.Adapters_Table;

