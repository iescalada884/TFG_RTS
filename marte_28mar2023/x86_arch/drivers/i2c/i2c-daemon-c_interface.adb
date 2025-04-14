------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                   i 2 c - d a e m o n - c _ i n t e r f a c e
--
--                                    spec
--
-- File 'i2c-daemon-c_interface.ads'                  By Sangorrin
--
-- This is the interfaz for C drivers that want to make use of an i2c
-- bus. You will have to '#include' the i2c.h header.
-- Besides, you will have to put 'with I2C.Daemon.C_Interface;' in the
-- kernel file 'marte_os_c.ads'
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
with Interfaces.C.Pointers; -- for C Interface
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;

package body I2c.Daemon.C_Interface is

   -------------------------------------------------------------------
   -- 1) Functions to create the Messages that will be sent to the Daemon
   -------------------------------------------------------------------
   --  a) I2C_Buffer Functions
   -------------------------------------------------------------------
   --  This is the buffer type for each message you want to send.
   --  You can create a buffer for receiving 'Num_Bytes' bytes, or
   --  for sending 'Data' (array of bytes).
   --  You can read the received bytes with the procedure 'Read_Buffer'
   -------------------------------------------------------------------
   function Create_Recv_Buff (
         Num_Bytes : in     I2c_Data_Count)
     return I2c_Buffer is
      Buffer : I2c_Buffer;
   begin
      I2c.Daemon.Create_Recv_Buff (Num_Bytes,Buffer,The_Buffer_List);
      return Buffer;
   end Create_Recv_Buff;

   function Create_Send_Buff (
         Data  : in     Char.Pointer;
         Count : in     I2c_Data_Count)
     return I2c_Buffer is
      Buffer   : I2c_Buffer;
      P        : Char.Pointer          := Data;
      Data_Ada : I2c_Data (1 .. Count);
   begin
      for I in Data_Ada'range loop
         Data_Ada(I) := P.All;
         P := P + 1;
      end loop;
      I2c.Daemon.Create_Send_Buff (Data_Ada,Buffer,The_Buffer_List);
      return Buffer;
   end Create_Send_Buff;

   procedure Free_Buffer (
         Buffer : in     I2c_Buffer) is
   begin
      I2c.Daemon.Free (Buffer,The_Buffer_List);
   end Free_Buffer;

   -------------------------------------------------------------------
   -- 2) Functions for sending the Msgs to the Daemon
   -------------------------------------------------------------------
   --  a) I2C_Transfer
   -------------------------------------------------------------------
   --  This is the main procedure. You can do every type of i2c
   --  operation with this procedure.
   -----------------------------------------------------------------
   function I2c_Transfer (
         Adap     : in     I2c_Adapter_Id;
         Op       : in     I2c_Operation_Id;
         Prio     : in     I2c_Priority;
         Msg_List : in     Msg.Pointer;
         Count    : in     C_Msg_Count)
     return Int is
      Cmd : I2c_Command_Ref :=
      new I2c_Command;
      Tmp_Msg : I2c_Msg;
      P       : Msg.Pointer     := Msg_List;
      Cell    : I2c_Buffer_Cell;
   begin
      Cmd.Adap_Id := Adap;
      Cmd.Operation_Id    := Op;
      Cmd.Msg_List := new I2c_Msg_List_Array(1 .. Count);
      for I in Cmd.Msg_List'range loop
         Tmp_Msg.Addr := P.All.Addr;
         Cell := Get_Buffer_Cell (P.All.Buffer, The_Buffer_List);
         Tmp_Msg.Flags := P.All.Flags or Cell.Flags;
         Tmp_Msg.Buffer := Cell.Data;
         Tmp_Msg.Count := Cell.Data'Length;
         Cmd.Msg_List(I) := Tmp_Msg;
         P := P + 1;
      end loop;
      Set_Status (Op, Waiting);
      Enqueue(Cmd, Prio, The_Commands_Queue);
      return 0;
   end I2c_Transfer;

   -------------------------------------------------------------------
   --   b) I2C_Master_Send, I2C_Master_Recv
   -------------------------------------------------------------------
   --   We also provide two more simple procedures that allow us to
   --   make a single Write or Read i2c operation.
   -------------------------------------------------------------------
   function I2c_Master_Send (
         Adap   : in     I2c_Adapter_Id;
         Op     : in     I2c_Operation_Id;
         Prio   : in     I2c_Priority;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags)
     return Int is
      Cmd : I2c_Command_Ref :=
      new I2c_Command;
      Tmp_Msg : I2c_Msg;
      Cell    : I2c_Buffer_Cell;
   begin
      Put("Not tested with a real device yet");
      Cmd.Adap_Id := Adap;
      Cmd.Operation_Id    := Op;
      Cmd.Msg_List := new I2c_Msg_List_Array(1 .. 1);

      Cell := Get_Buffer_Cell (Buffer, The_Buffer_List);

      Tmp_Msg.Addr := Addr;
      Tmp_Msg.Flags := Flags or Cell.Flags;
      Tmp_Msg.Buffer := Cell.Data;
      Tmp_Msg.Count := Cell.Data'Length;
      Cmd.Msg_List(1) := Tmp_Msg;

      Set_Status (Op, Waiting);
      Enqueue(Cmd, Prio, The_Commands_Queue);
      return 0;
   end I2c_Master_Send;

   function I2c_Master_Recv (
         Adap   : in     I2c_Adapter_Id;
         Op     : in     I2c_Operation_Id;
         Prio   : in     I2c_Priority;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags)
     return Int renames I2c_Master_Send;

   -------------------------------------------------------------------
   -- 3) Functions for checking the status of our commands
   -------------------------------------------------------------------
   --   If you want a non-blocking behavior you can use 'Get_Status' to
   -- check the status of your command regularly.
   -- If you want to get blocked until your command is executed you can
   -- use the function 'Wait'. (Of course you have to pass the same
   -- Operation variable of your command)
   -------------------------------------------------------------------
   function I2c_Get_Status (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status is
   begin
      return I2c.Daemon.Get_Status(Op);
   end I2c_Get_Status;

   function I2c_Wait (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status is
   begin
      return I2c.Daemon.Wait(Op);
   end I2c_Wait;

   function Read_Buffer (
         Data   : in     Char.Pointer;
         Count  : in     I2c_Data_Count;
         Buffer : in     I2c_Buffer;
         Op     : in     I2c_Operation_Id)
     return I2c_Operation_Status is
      Stat : I2c_Operation_Status;
      Cell : I2c_Buffer_Cell;
      P    : Char.Pointer         := Data;
   begin
      Stat := I2c.Daemon.Wait (Op);
      Cell := Get_Buffer_Cell (Buffer, The_Buffer_List);
      for I in 1 .. Count loop
         P.All := Cell.Data(I);
         P := P + 1;
      end loop;
      return Stat;
   end Read_Buffer;

end I2c.Daemon.C_Interface;
