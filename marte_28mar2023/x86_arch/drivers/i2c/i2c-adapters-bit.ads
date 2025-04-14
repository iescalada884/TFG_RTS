------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                          i 2 c . a d a p t e r s . b i t
--
--                                    spec
--
-- File 'i2c-adapters-bit.ads'                                 By Sangorrin
--
--
-- This adapter implements the i2c protocol by bitbanging two digital lines.
-- In order to obtain a bit-adapter you have to make an instance and assign
-- the low-level functions (set_sda, set_scl ...) that control this lines.
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

package I2c.Adapters.Bit is

   Default_Semiperiod : constant Duration := 0.005;
   procedure Wait_Default_Semiperiod;

   type Wait_Semiperiod_Ac is access procedure
   ;
   type Set_I2c_Lines_Ac is access procedure
   ;
   type Get_I2c_Lines_Ac is access function
   return Unsigned_8;

   type I2c_Adapter_Bit is new I2c_Adapter with
      record
         Wait_Semiperiod : Wait_Semiperiod_Ac := null;
         Set_Sda_High    : Set_I2c_Lines_Ac   := null;
         Set_Sda_Low     : Set_I2c_Lines_Ac   := null;
         Set_Scl_High    : Set_I2c_Lines_Ac   := null;
         Set_Scl_Low     : Set_I2c_Lines_Ac   := null;
         Get_Sda         : Get_I2c_Lines_Ac   := null;
         Get_Scl         : Get_I2c_Lines_Ac   := null;
      end record;

   function Master_Xfer (
         Adap     : I2c_Adapter_Bit;
         Msg_List : I2c_Msg_List)
     return Int;

private

   procedure Start (
         Adap : in     I2c_Adapter_Bit);

   procedure Restart (
         Adap : in     I2c_Adapter_Bit);

   procedure Stop (
         Adap : in     I2c_Adapter_Bit);

   procedure Write (
         Adap : in     I2c_Adapter_Bit;
         Byte : in     Unsigned_8);

   procedure Read (
         Adap : in     I2c_Adapter_Bit;
         Byte :    out Unsigned_8);

   function Get_Ack (
         Adap : in     I2c_Adapter_Bit)
     return Boolean;

   procedure Set_Ack (
         Adap : in     I2c_Adapter_Bit);

   procedure Set_Nack (
         Adap : in     I2c_Adapter_Bit);

end I2c.Adapters.Bit;

