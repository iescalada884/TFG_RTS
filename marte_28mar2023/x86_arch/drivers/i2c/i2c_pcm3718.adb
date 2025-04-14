------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                            i 2 c _ p c m 3 7 1 8
--
--                                    body
--
-- File 'i2c_pcm3718.adb'                                       By Sangorrin
--
--  Bit Banging Adapter using PCM3718H lines (not tested)
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
with MaRTE.Integer_Types;
use MaRTE.Integer_Types;  -- for Unsigned_8
with I2c.Adapters.Bit;
use I2c.Adapters.Bit;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
pragma Elaborate_All(MaRTE.HAL.IO);

package body I2c_Pcm3718 is

   Last_Control_Byte : Unsigned_8 := 2#0000_0000#;

   procedure I2c_Pcm3718_Sda_High is
   begin
      Last_Control_Byte := Last_Control_Byte and (not Sda_Write);
      Outb_P (Pcm3718_Base + Pcm3718_Dio_1, Last_Control_Byte);
   end I2c_Pcm3718_Sda_High;

   procedure I2c_Pcm3718_Sda_Low is
   begin
      Last_Control_Byte := Last_Control_Byte or Sda_Write;
      Outb_P (Pcm3718_Base + Pcm3718_Dio_1, Last_Control_Byte);
   end I2c_Pcm3718_Sda_Low;

   procedure I2c_Pcm3718_Scl_High is
   begin
      Last_Control_Byte := Last_Control_Byte and (not Scl_Write);
      Outb_P (Pcm3718_Base + Pcm3718_Dio_1, Last_Control_Byte);
   end I2c_Pcm3718_Scl_High;

   procedure I2c_Pcm3718_Scl_Low is
   begin
      Last_Control_Byte := Last_Control_Byte or Scl_Write;
      Outb_P (Pcm3718_Base + Pcm3718_Dio_1, Last_Control_Byte);
   end I2c_Pcm3718_Scl_Low;

   function I2c_Pcm3718_Read_Sda return Unsigned_8 is
      Byte : Unsigned_8;
   begin
      Byte := Inb_P (Pcm3718_Base + Pcm3718_Dio_1);
      Byte := Byte and Sda_Read;
      if Byte = 0 then
         return 0;
      else
         return 1;
      end if;
   end I2c_Pcm3718_Read_Sda;

   function I2c_Pcm3718_Read_Scl return Unsigned_8 is
   begin
      return 1;
   end I2c_Pcm3718_Read_Scl;

end I2c_Pcm3718;