------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                            i 2 c _ p a r p o r t
--
--                                    body
--
-- File 'i2c_parport.adb'                                        By Sangorrin
--
--
--  Bit Banging Adapter using Paralell Port lines (not tested)
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;  -- for Unsigned_8
with MaRTE.Direct_IO;
use MaRTE.Direct_IO;

package body I2c_Parport is

   Last_Data_Byte    : Unsigned_8 := 2#1000_0000#;
   Last_Status_Byte  : Unsigned_8 := 2#0000_0000#;
   Last_Control_Byte : Unsigned_8 := 2#0000_1000#;

   procedure I2c_Parport_Sda_High is
   begin
      Last_Data_Byte := Last_Data_Byte And (Not 2#1000_0000#);
      Outb_P (PP_BASE_REG + PP_DATA_REG, Last_Data_Byte);
   end I2c_Parport_Sda_High;

   procedure I2c_Parport_Sda_Low is
   begin
      Last_Data_Byte := Last_Data_Byte Or 2#1000_0000#;
      Outb_P (PP_BASE_REG + PP_DATA_REG, Last_Data_Byte);
   end I2c_Parport_Sda_Low;

   procedure I2c_Parport_Scl_High is
   begin
      Last_Control_Byte := Last_Control_Byte or 2#0000_1000#;
      Outb_P (Pp_Base_Reg + Pp_Control_Reg, Last_Control_Byte);
   end I2c_Parport_Scl_High;

   procedure I2c_Parport_Scl_Low is
   begin
      Last_Control_Byte := Last_Control_Byte and (not 2#0000_1000#);
      Outb_P (Pp_Base_Reg + Pp_Control_Reg, Last_Control_Byte);
   end I2c_Parport_Scl_Low;

   function I2c_Parport_Read_Sda return Unsigned_8 is
      Byte : Unsigned_8;
   begin
      Byte := Inb_P (Pp_Base_Reg + PP_STATUS_REG);
      Byte := Byte and 2#1000_0000#;
      if Byte = 0 then
         return 0;
      else
         return 1;
      end if;
   end I2c_Parport_Read_Sda;

   function I2c_Parport_Read_Scl return Unsigned_8 is
      Byte : Unsigned_8;
   begin
      Byte := Inb_P (Pp_Base_Reg + PP_STATUS_REG);
      Byte := Byte and 2#0000_1000#;
      if Byte = 0 then
         return 0;
      else
         return 1;
      end if;
   end I2c_Parport_Read_Scl;

end I2c_Parport;

