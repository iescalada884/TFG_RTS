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
with MaRTE.Direct_IO;
use MaRTE.Direct_IO;
with MaRTE.Integer_Types;
use MaRTE.Integer_Types;

-- with ada.text_io; -- DEBUGGING
package body I2c.Adapters.Bit is

   use type MaRTE.Integer_Types.Int; --  Mario

   -- DEBUGGING
   --   procedure Pause (S : String) is
   --      H : Character;
   --   begin
   --      Ada.Text_Io.Put(S);
   --      Ada.Text_Io.Get_Immediate(H);
   --      Ada.Text_Io.New_Line;
   --   end Pause;
   -- FIN DEBUGGING

   Retry_Mx : constant := 3;
   Retry_Exceeded : exception;

   procedure Start (
                    Adap : in     I2c_Adapter_Bit) is
   begin
      Adap.Wait_Semiperiod.All;
      Adap.Set_Sda_Low.All;
      Adap.Wait_Semiperiod.All;
      Adap.Set_Scl_Low.All;
   end Start;

   procedure Restart (
                      Adap : in     I2c_Adapter_Bit) is
   begin
      Adap.Set_Sda_Low.All;
      Adap.Wait_Semiperiod.All;
      Adap.Set_Sda_High.All;
      Adap.Wait_Semiperiod.All;
      Adap.Set_Scl_High.All;
      Adap.Wait_Semiperiod.All;
      Adap.Wait_Semiperiod.All;
      Adap.Set_Sda_Low.All;
      Adap.Wait_Semiperiod.All;
      Adap.Set_Scl_Low.All;
   end Restart;

   procedure Stop (
                   Adap : in     I2c_Adapter_Bit) is
   begin
      Adap.Set_Sda_Low.All;
      Adap.Wait_Semiperiod.All;
      Adap.Set_Scl_High.All;
      Adap.Wait_Semiperiod.All;
      Adap.Set_Sda_High.All;
   end Stop;

   procedure Write (
                    Adap : in     I2c_Adapter_Bit;
                    Byte : in     Unsigned_8) is
      Tmp_Byte : Unsigned_8 := Byte;
   begin
      -- We write each bit in the bus from the most significant to the least one
      for I in 0 .. 7 loop
         if (Tmp_Byte and 16#80#) > 0 then
            Adap.Set_Sda_High.All;   --      ______________________
         else                        -- SDA |__0__|__1__|__ . . 7_|
            Adap.Set_Sda_Low.All;    --        __    __        __
         end if;                     -- SCL __|  |__|  |__ . .|  |
         Adap.Wait_Semiperiod.All;
         Adap.Set_Scl_High.All;
         Adap.Wait_Semiperiod.All;
         Adap.Set_Scl_Low.All;
         Tmp_Byte := Shift_Left(Tmp_Byte,1);
      end loop;
   end Write;

   -- Read: can raise Retry_Exceeded
   procedure Read (
                   Adap : in     I2c_Adapter_Bit;
                   Byte :    out Unsigned_8) is
      Bit : Unsigned_8;
   begin
      Byte := 0;
      -- We read each bit, from the most significant to the least one
      for I in 0 .. 7 loop
         Adap.Set_Sda_High.All;
         -- The slave should stick SDA line to a low
         Adap.Wait_Semiperiod.All;
         Adap.Set_Scl_High.All;
         Adap.Wait_Semiperiod.All;
         -------------------------
         for J in 1 .. Retry_Mx loop
            Bit := Adap.Get_Scl.All;
            if Bit /= 0 then
               exit;
            end if;
            Put("retry called read");
            Adap.Wait_Semiperiod.All;
         end loop;
         if Bit = 0 then
            raise Retry_Exceeded;
         end if;
         -------------------------
         Bit := Adap.Get_Sda.All;
         Byte  := Byte or Bit;
         if ( I /= 7 ) then
            Byte := Shift_Left(Byte,1);
         end if;
         Adap.Set_Scl_Low.All;
      end loop;
   end Read;

   function Get_Ack (
                     Adap : in     I2c_Adapter_Bit)
                    return Boolean is
      Sda_Value, Bit : Unsigned_8;
   begin
      Adap.Set_Sda_High.All;
      -- The slave should stick SDA line to a low value
      Adap.Wait_Semiperiod.All;
      Adap.Set_Scl_High.All;
      Adap.Wait_Semiperiod.All;
      -------------------------
      for J in 1 .. Retry_Mx loop
         Bit := Adap.Get_Scl.All;
         if Bit /= 0 then
            exit;
         end if;
         Adap.Wait_Semiperiod.All;
      end loop;
      if Bit = 0 then
         raise Retry_Exceeded;
      end if;
      -------------------------
      Sda_Value := Adap.Get_Sda.All;
      Adap.Set_Sda_Low.All;
      Adap.Set_Scl_Low.All;
      Adap.Wait_Semiperiod.All;
      return (Sda_Value = 0);
   end Get_Ack;

   procedure Set_Ack (
                      Adap : in     I2c_Adapter_Bit) is
   begin
      Adap.Set_Scl_Low.All;
      Adap.Set_Sda_Low.All;         --
      Adap.Wait_Semiperiod.All;     -- SDA   ______
      Adap.Set_Scl_High.All;        --          __
      Adap.Wait_Semiperiod.All;     -- SCL   __|  |__
      Adap.Set_Scl_Low.All;
   end Set_Ack;

   procedure Set_Nack (
                       Adap : in     I2c_Adapter_Bit) is
   begin
      Adap.Set_Scl_Low.All;
      Adap.Set_Sda_High.All;        --      ______
      Adap.Wait_Semiperiod.All;     -- SDA
      Adap.Set_Scl_High.All;        --          __
      Adap.Wait_Semiperiod.All;     -- SCL   __|  |
      Adap.Set_Scl_Low.All;
   end Set_Nack;

   procedure Wait_Default_Semiperiod is
   begin
      delay Default_Semiperiod;
   end Wait_Default_Semiperiod;

   function Master_Xfer (
                         Adap     : I2c_Adapter_Bit;
                         Msg_List : I2c_Msg_List)
                        return Int is
      Data : I2c_Data_Ref;
      Ack  : Boolean;

      function To_Rw_Address (
                              Address : in     I2c_Address;
                              Read    : in     Boolean)
                             return I2c_Address is
         New_Address : I2c_Address;
      begin
         New_Address := Shift_Left (Address, 1);
         if Read then
            New_Address := New_Address or 16#01#;
         end if;
         return New_Address;
      end To_Rw_Address;
      pragma Inline(To_Rw_Address);

      No_Ack : exception;

      --       value : Unsigned_8; -- DEBUGGING
   begin
      --  DEBUGGING
      --       loop
      --          Adap.Set_Sda_High.All; pause("sda h");
      --          Adap.Set_Sda_Low.All; pause("sda L");
      --          Adap.Set_Scl_High.All; pause("scl h");
      --          Adap.Set_Scl_Low.All; pause("scl L");
      --          value := Adap.Get_Sda.All; pause("SDA:"&Unsigned_8'Image(value));
      --          value := Adap.Get_Scl.All; pause("SCL:"&Unsigned_8'Image(value));
      --       end loop;
      -- FIN DEBUGGING

      Adap.Set_Sda_High.All;
      Adap.Set_Scl_High.All;
      Adap.Wait_Semiperiod.All;

      Start (Adap);
      for I in Msg_List'range loop
         Data := new I2c_Data(1 .. Msg_List(I).Count);
         if (Msg_List(I).Flags and I2c_M_Rd) = I2c_M_Rd then
            -- Write the Address
            Write (Adap, Unsigned_8( To_Rw_Address(Msg_List(I).Addr,True) ));
            Ack := Get_Ack (Adap);
            if not Ack then
               raise No_Ack;
            end if;
            -- Read the bytes
            for J in I2c_Data_Count range 1 .. (Data'Length - 1) loop
               Read (Adap, Data(J));
               Set_Ack (Adap);
            end loop;
            -- last byte needs a nack
            Read (Adap, Data(Data'Length));
            Set_Nack (Adap);
            Msg_List(I).Buffer.All := Data.All;
         else -- MSG: Write
              -- Write the Address
            Write (Adap, Unsigned_8( To_Rw_Address(Msg_List(I).Addr,False) ));
            Ack := Get_Ack (Adap);
            if not Ack then
               raise No_Ack;
            end if;
            -- Write the bytes
            Data.All := Msg_List(I).Buffer.All;
            for J in Data'range loop
               Write (Adap, Data(J));
               Ack := Get_Ack (Adap);
               if not Ack then
                  raise No_Ack;
               end if;
            end loop;
         end if;
         if (I = Msg_List'Length) then
            Stop (Adap);
         else
            Restart (Adap);
         end if;
      end loop;
      return 0;
   exception
      when No_Ack =>
         Put("Error: No Ack received");
         Stop (Adap);
         return -1;
      when Retry_Exceeded =>
         Put("Error: Retry Exceeded. Review your connections and try reducing the I2C Rate");
         Stop (Adap);
         return -1;
   end Master_Xfer;

end I2c.Adapters.Bit;

