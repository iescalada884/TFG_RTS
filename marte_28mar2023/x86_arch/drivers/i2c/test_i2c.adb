------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                          t e s t _ i 2 c . a d b
--
--                                    body
--
-- File 'test_i2c.adb'                                 By Sangorrin
--
-- This package tests i2c subsistem with a CMPS03 device.
--
--  to compile: mgnatmake test_i2c.adb
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
-----------------------------------------------------------------------

with I2c;
with I2c.Daemon.Ada_Interface;
use I2c;
use I2c.Daemon.Ada_Interface;

with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;


procedure Test_I2c is

   pragma Priority(4);

   Adap  : I2c_Adapter_Id   := Elite;
   Op    : I2c_Operation_Id := 1;
   Prio  : I2c_Priority     := 4;
   Addr  : I2c_Address      := 16#60#;
   Flags : I2c_Flags;

   Buffer_Send : I2c_Buffer;
   Buffer_Read : I2c_Buffer;
   Data        : I2c_Data (1 .. 1);

   Msg      : I2c_Msg_Ada;
   Msg_List : I2c_Msg_Ada_List;

   Stat : I2c_Operation_Status;

   procedure Pause is
      H : Character;
   begin
      Put("Press..");
      Get_Immediate(H);
      New_Line;
   end Pause;

begin

   Buffer_Read := Create_Recv_Buff (Num_Bytes => 1);

   Data(1) := 16#01#; -- Register Bearing
   Buffer_Send := Create_Send_Buff (Data => Data);

   Flags_Init (Flags);

   loop
      Pause;
      Create_Msg_List (2, Msg_List);
      Create_Msg (Msg, Addr, Buffer_Send, Flags);
      Add (Msg, Msg_List);
      Create_Msg (Msg, Addr, Buffer_Read, Flags);
      Add (Msg, Msg_List);
      I2c_Transfer (Adap, Op, Prio, Msg_List);

      loop
         Stat := Get_Status (Op);
         exit when (Stat = Workdone);
      end loop;
      Stat := Wait (Op);
      Put(I2c_Operation_Status'Image(Stat));

      Read_Buffer (
         Data   => Data,
         Buffer => Buffer_Read,
         Op     => Op,
         Stat   => Stat);
      Put( Byte_Type'Image( Data(1) ));
   end loop;


end Test_I2c;