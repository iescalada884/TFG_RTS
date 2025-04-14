------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                            i 2 c _ e l i t e
--
--                                    spec
--
-- File 'i2c_elite.ads'                                            By Sangorrin
--
--
-- This file imports the functions in i2c_elite_c.c providing the way to
-- emulate the i2c protocol on the STPC Elite CPU. It makes use of some bits
-- of the DDC control register. It supposes that you are the only master in
-- the bus.
--
-- Philips I2C protocol manual
-- http://www.semiconductors.philips.com/acrobat/literature/9398/39340011.pdf
--
-- how to use it --> test_i2c_elite.adb
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
with System, Ada.Unchecked_Conversion;

with I2C.Adapters.Bit;
use I2C.Adapters.Bit;

package I2C_Elite is

  package AdBit renames I2C.Adapters.Bit;

  ACK : constant Unsigned_8 := 1;
  NACK : constant Unsigned_8 := 0;
---------------------------------------------------------------------------
-- 1.- LOW-LEVEL functions for accesing the DDC Register of STPC ELite
-- You probably don't need to use this functions in your app or driver. They
-- are only provided for testing and giving more power to the drivers.
---------------------------------------------------------------------------
-- a) Wait one semi-cycle. You can adjust the time with I2C_BIT_RATE (see .h)
-- On the STPC Elite 133Mhz I2C_BIT_RATE=200 is a good value to start
-- testing. The Standard use of I2C is 0-100Kbit/s
procedure I2C_Elite_Wait;
pragma Import (C, I2C_Elite_Wait, "I2C_Elite_Wait");

-- b) Get the whole DDC control register to see its bits
function GET_DDC_REG return Unsigned_8;
pragma Import (C, GET_DDC_REG, "GET_DDC_REG");

-- c) Set the the DDC control register to change its bits
procedure SET_DDC_REG ( DDCR : in Unsigned_8);
pragma Import (C, SET_DDC_REG, "SET_DDC_REG");

-- d) Set SDA line to a high value
procedure I2C_Elite_SDA_High;
pragma Import (C, I2C_Elite_SDA_High, "I2C_Elite_SDA_High");

-- e) Set SDA line to a low value
procedure I2C_Elite_SDA_Low;
pragma Import (C, I2C_Elite_SDA_Low, "I2C_Elite_SDA_Low");

-- f) Set SCL line to a high value
procedure I2C_Elite_SCL_High;
pragma Import (C, I2C_Elite_SCL_High, "I2C_Elite_SCL_High");

-- g) Set SCL line to a low value
procedure I2C_Elite_SCL_Low;
pragma Import (C, I2C_Elite_SCL_Low, "I2C_Elite_SCL_Low");

-- h) Read the current SDA value in the I2C bus
function I2C_Elite_Read_SDA return Unsigned_8;
pragma Import (C, I2C_Elite_Read_SDA, "I2C_Elite_Read_SDA");

-- i) Read the current SCL value in the I2C bus
function I2C_Elite_Read_SCL return Unsigned_8;
pragma Import (C, I2C_Elite_Read_SCL, "I2C_Elite_Read_SCL");

-----------------------------------------------------------------------------
-- 2.- HIGH-LEVEL functions for controlling an I2C slave device
-----------------------------------------------------------------------------
-- ------------------------------------------------------------
-- Function:  void I2C_Elite_Start (void)
-- Description: Start bit of the I2C protocol
-- ------------------------------------------------------------
procedure I2C_Elite_Start;
pragma Import (C, I2C_Elite_Start, "I2C_Elite_Start");
-- ------------------------------------------------------------
-- Function:  void I2C_Elite_ReStart (void)
-- Description: Repeated Start bit of the I2C protocol
-- ------------------------------------------------------------
procedure I2C_Elite_ReStart;
pragma Import (C, I2C_Elite_ReStart, "I2C_Elite_ReStart");
-- ------------------------------------------------------------
-- Function:  void I2C_Elite_Stop (void)
-- Description: Stop bit of the I2C protocol
-- ------------------------------------------------------------
procedure I2C_Elite_Stop;
pragma Import (C, I2C_Elite_Stop, "I2C_Elite_Stop");
-- ------------------------------------------------------------
-- Function:  void I2C_Elite_Write ( unsigned char byte)
-- Description: Write a byte in the bus
-- ------------------------------------------------------------
procedure I2C_Elite_Write ( byte : in Unsigned_8);
pragma Import (C, I2C_Elite_Write, "I2C_Elite_Write");
-- ------------------------------------------------------------
-- Function:  unsigned char I2C_Elite_Get_Ack (void)
-- Description: Retreive acknowledge if data is sended correctly.
-- It can return ACK or NACK (1 and 0).
-- ------------------------------------------------------------
function I2C_Elite_Get_Ack return Unsigned_8;
pragma Import (C, I2C_Elite_Get_Ack, "I2C_Elite_Get_Ack");
-- ------------------------------------------------------------
-- Function:  unsigned char I2C_Elite_Read (void)
-- Description: Read a byte from the SDA line
-- ------------------------------------------------------------
function I2C_Elite_Read return Unsigned_8;
pragma Import (C, I2C_Elite_Read, "I2C_Elite_Read");
-- ------------------------------------------------------------
-- Function:  void I2C_Elite_Set_Ack (void)
-- Description: Send an Acknowledge
-- ------------------------------------------------------------
procedure I2C_Elite_Set_Ack;
pragma Import (C, I2C_Elite_Set_Ack, "I2C_Elite_Set_Ack");
-- ------------------------------------------------------------
-- Function:  void I2C_Elite_Set_Nack (void)
-- Description: Send a No Acknowledge. See the I2C protocol
-- manual in order to know when you should use Nacks.
-- ------------------------------------------------------------
procedure I2C_Elite_Set_Nack;
pragma Import (C, I2C_Elite_Set_Nack, "I2C_Elite_Set_Nack");

--------------------------------------------------------------

	function Address_To_Set_Ac is
      new Ada.Unchecked_Conversion (System.Address, Set_I2C_Lines_Ac);

	function Address_To_Get_Ac is
      new Ada.Unchecked_Conversion (System.Address, Get_I2C_Lines_Ac);

	I2C_Adapter_BIT_Elite : aliased AdBit.I2C_Adapter_BIT :=
		(Wait_Semiperiod => Wait_Default_Semiperiod'Access,
		 ID 			  => I2C.Elite,
		 Set_SDA_High => Address_To_Set_Ac(I2C_Elite_SDA_High'Address),
		 Set_SDA_Low  => Address_To_Set_Ac(I2C_Elite_SDA_Low'Address),
		 Set_SCL_High => Address_To_Set_Ac(I2C_Elite_SCL_High'Address),
		 Set_SCL_Low  => Address_To_Set_Ac(I2C_Elite_SCL_Low'Address),
		 Get_SDA      => Address_To_Get_Ac(I2C_Elite_Read_SDA'Address),
		 Get_SCL      => Address_To_Get_Ac(I2C_Elite_Read_SCL'Address));

end I2C_Elite;

