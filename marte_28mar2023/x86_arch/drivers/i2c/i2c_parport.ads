------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                            i 2 c _ p a r p o r t
--
--                                    spec
--
-- File 'i2c_parport.ads'                                            By Sangorrin
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
with I2C.Adapters.Bit; use I2C.Adapters.Bit;
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
pragma Elaborate_All(MaRTE.HAL.IO);

package I2C_Parport is

  package AdBit renames I2C.Adapters.Bit;

-----------------------------------------------------------------------------
-- LAYOUT:
-----------------------------------------------------------------------------
--                                         LS05 pin 14 (Vcc) o      -------
--                                                           |      |     |
--            +--+--+---------------------+--+--+------------+------+-o 1 |
--            |  |  |                     |  |  |           ===.1uF | +5V |
--  -------- [R][R][R] 3x10K       3x10K [R][R][R]   LS05    |      |     |
--  |      |  |  |  |                     |  |  |    pin 7 o-+------+-o 2 |
--  | 12 o-+--+  |  |   3|\ 4             |  |  |    (Gnd)          | GND |
--  | 17 o-+-----|--|----| >o-------------+--|--|--------------+    |     |
--  |      |     |  |    |/        8 /|9     |  |     10 /|11  +----+-o 3 |
--  | 15 o-+-----+--|--------------o< |------+--|------o< |----+    | SCL |
--  |      |        |   1|\ 2        \|         |        \|         |     |
--  |  9 o-+--------|----| >o-------------------+--------------+----+-o 4 |
--  |      |        |    |/                            6 /|5   |    | SDA |
--  | 11 o-+--------+----------------------------------o< |----+    -------
--  | 10 o-+-+                                           \|          4-pin
--  | 13 o-+-+--oGND                                               Connector
--  | 25 o-+-+        ------------------ Part List --------------------------
--  --------          | 1 - .01 uF capacitor  | 6 - 10K 5% resistors        |
--  25-pin male D     | 1 - 4-pin connector   | 1 - 25-pin male D connector |
--  connector to PC   | 1 - 74LS05 open collector hex inverter              |
--  printer port      -------------------------------------------------------

  PP_BASE_REG : constant IO_Port := 16#378#;
  PP_DATA_REG : constant IO_Port := 0;
  PP_STATUS_REG : constant IO_Port := 1;
  PP_CONTROL_REG : constant IO_Port := 2;


  procedure I2C_Parport_SDA_High;

  procedure I2C_Parport_SDA_Low;

  procedure I2C_Parport_SCL_High;

  procedure I2C_Parport_SCL_Low;

  function I2C_Parport_Read_SDA return Unsigned_8;

  function I2C_Parport_Read_SCL return Unsigned_8;

  I2C_Adapter_BIT_Parport : aliased AdBit.I2C_Adapter_BIT :=
    (Wait_Semiperiod => Wait_Default_Semiperiod'Access,
     ID                           => I2C.Parport,
     Set_SDA_High => I2C_Parport_SDA_High'Access,
     Set_SDA_Low  => I2C_Parport_SDA_Low'Access,
     Set_SCL_High => I2C_Parport_SCL_High'Access,
     Set_SCL_Low  => I2C_Parport_SCL_Low'Access,
     Get_SDA      => I2C_Parport_Read_SDA'Access,
     Get_SCL      => I2C_Parport_Read_SCL'Access);

end I2C_Parport;
