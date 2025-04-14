------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                            i 2 c _ p c m 3 7 1 8
--
--                                    spec
--
-- File 'i2c_pcm3718.ads'                                            By Sangorrin
--
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
with MaRTE.Integer_Types; use MaRTE.Integer_Types;  -- for Unsigned_8
with I2c.Adapters.Bit; use I2c.Adapters.Bit;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
pragma Elaborate_All (MaRTE.HAL.IO);

package I2c_Pcm3718 is

   package Adbit renames I2c.Adapters.Bit;

-----------------------------------------------------------------------------
-- LAYOUT:
-----------------------------------------------------------------------------
--                                         LS05 pin 14 (Vcc) o      -------
--                                                           |      |     |
--               +------------------------+--+--+------------+------+-o 1 |
--               |                        |  |  |           ===.1uF | +5V |
--           1K [R]                3x10K [R][R][R]   LS05    |      |     |
--               |                        |  |  |    pin 7 o-+------+-o 2 |
--   ------      |      3|\ 4             |  |  |    (Gnd)          | GND |
--  |  1 o-+-----|-------| >o-------------+--|--|--------------+    |     |
--  |      |     |       |/        8 /|9     |  |     10 /|11  +----+-o 3 |
--  |  2 o-+-----+-----------------o< |------+--|------o< |----+    | SDA |
--  |      |            1|\ 2        \|         |        \|         |     |
--  |  3 o-+-------------| >o-------------------+--------------+----+-o 4 |
--  |      |             |/                                         | SCL |
--  |      |                                                        -------
--  | 17 o-+-+                                                       4-pin
--  | 18 o-+-+--oGND                                               Connector
--  --------          ------------------ Part List --------------------------
--                    | 1 - .01 uF capacitor  | 3 - 10K 5% resistors        |
--  PCM-3718-H        | 1 - 4-pin connector   | 1 - 1K 5% resistor          |
--                    | 1 - 74LS05 open collector hex inverter              |
--                    -------------------------------------------------------

   Pcm3718_Base  : constant MaRTE.HAL.IO.Io_Port := 16#300#;
   Pcm3718_Dio_1 : constant Io_Port              := 3; -- RW

   Scl_Write     : constant Unsigned_8           := 2#0000_0100#;
   Sda_Write     : constant Unsigned_8           := 2#0000_0001#;
   Sda_Read      : constant Unsigned_8           := 2#0000_0010#;
   -- Scl_Read is not supported so we don't watch if the slave is holding SCL

   procedure I2c_Pcm3718_Sda_High;

   procedure I2c_Pcm3718_Sda_Low;

   procedure I2c_Pcm3718_Scl_High;

   procedure I2c_Pcm3718_Scl_Low;

   function I2c_Pcm3718_Read_Sda return Unsigned_8;

   function I2c_Pcm3718_Read_Scl return Unsigned_8;

   I2C_Adapter_BIT_Pcm3718 : aliased AdBit.I2C_Adapter_BIT :=
     (Wait_Semiperiod => Wait_Default_Semiperiod'Access,
      ID                          => I2C.Pcm3718,
      Set_SDA_High => I2C_Pcm3718_SDA_High'Access,
      Set_SDA_Low  => I2C_Pcm3718_SDA_Low'Access,
      Set_SCL_High => I2C_Pcm3718_SCL_High'Access,
      Set_SCL_Low  => I2C_Pcm3718_SCL_Low'Access,
      Get_SDA      => I2C_Pcm3718_Read_SDA'Access,
      Get_SCL      => I2C_Pcm3718_Read_SCL'Access);

end I2c_Pcm3718;
