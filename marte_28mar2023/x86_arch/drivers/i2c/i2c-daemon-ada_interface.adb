------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                   i 2 c - d a e m o n - a d a _ i n t e r f a c e
--
--                                    body
--
-- File 'i2c-daemon-ada_interface.adb'                  By Sangorrin
--
-- This is the interfaz for Ada drivers that want to make use of an i2c
-- bus.
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
package body I2c.Daemon.Ada_Interface is

   -------------------------------------------------------------------
   -- 1) Functions to create the Messages that will be sent to the Daemon
   -------------------------------------------------------------------
   --  a) I2C_Flags
   -------------------------------------------------------------------
   --  The FLAGS allow us to distinguish a READ from a SEND message.
   --  Besides, they may be useful in the future to add other features
   --  like supporting Ten Bit addresses, etc...
   -------------------------------------------------------------------
   procedure Flags_Init (
         Flags : in out I2c_Flags) is
   begin
      Flags := I2c_Default_Flags;
   end Flags_Init;

   procedure Flags_Set_Tenbitaddress (
         Flags : in out I2c_Flags) is
   begin
      Flags := Flags or I2c_M_Ten;
   end Flags_Set_Tenbitaddress;

   procedure Flags_Set_Nostart (
         Flags : in out I2c_Flags) is
   begin
      Flags := Flags or I2c_M_Nostart;
   end Flags_Set_Nostart;

   procedure Flags_Set_Revdiraddr (
         Flags : in out I2c_Flags) is
   begin
      Flags := Flags or I2c_M_Rev_Dir_Addr;
   end Flags_Set_Revdiraddr;

   procedure Flags_Set_Ignorenack (
         Flags : in out I2c_Flags) is
   begin
      Flags := Flags or I2c_M_Ignore_Nak;
   end Flags_Set_Ignorenack;

   procedure Flags_Set_Nordack (
         Flags : in out I2c_Flags) is
   begin
      Flags := Flags or I2c_M_No_Rd_Ack;
   end Flags_Set_Nordack;
   -------------------------------------------------------------------
   --  b) I2C_Buffer Functions
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
         Data : in     I2c_Data)
     return I2c_Buffer is
      Buffer : I2c_Buffer;
   begin
      I2c.Daemon.Create_Send_Buff (Data,Buffer,The_Buffer_List);
      return Buffer;
   end Create_Send_Buff;

   procedure Free (
         Buffer : in     I2c_Buffer) is
   begin
      I2c.Daemon.Free (Buffer,The_Buffer_List);
   end Free;

   -------------------------------------------------------------------
   --  c) I2C_Msg_Ada, I2C_Msg_Ada_List
   -------------------------------------------------------------------
   --  The messages are used in the 'I2C_Transfer' function to tell
   --  the I2C Daemon to do a group of i2c operations. This operations
   --  are done by writing a 'Restart' bit between them.
   -------------------------------------------------------------------
   procedure Create_Msg (
         Msg    : in out I2c_Msg_Ada;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags) is
      Cell : I2c_Buffer_Cell;
   begin
      Cell := Get_Buffer_Cell (Buffer, The_Buffer_List);
      Msg.Addr := Addr;
      Msg.Buffer := Cell.Data;
      Msg.Flags := Cell.Flags or Flags;
      Msg.Count := Cell.Data'Length;
   end Create_Msg;

   procedure Create_Msg_List (
         Msg_Max  : in     Positive;
         Msg_List : in out I2c_Msg_Ada_List) is
   begin
      Msg_List.Msg_List := new I2c_Msg_List_Array(1 .. Msg_Max);
      Msg_List.Num := 0;
   end Create_Msg_List;

   procedure Add (
         Msg      : in     I2c_Msg_Ada;
         Msg_List : in out I2c_Msg_Ada_List) is
      Tmp : Natural := Msg_List.Num;
   begin
      Tmp := Tmp + 1;
      if Tmp in Msg_List.Msg_List'range then
         Msg_List.Msg_List(Tmp) := Msg;
         Msg_List.Num := Tmp;
      end if;
   end Add;

   procedure Empty (
         Msg_List : in out I2c_Msg_Ada_List) is
   begin
      Msg_List.Num := 0;
   end Empty;

   -------------------------------------------------------------------
   -- 2) Functions for sending the Msgs to the Daemon
   -------------------------------------------------------------------
   --  a) I2C_Transfer
   -------------------------------------------------------------------
   --  This is the main procedure. You can do every type of i2c
   --  operation with this procedure.
   -------------------------------------------------------------------
   procedure I2c_Transfer (
         Adap     : in     I2c_Adapter_Id;
         Op       : in     I2c_Operation_Id;
         Prio     : in     I2c_Priority;
         Msg_List : in     I2c_Msg_Ada_List) is
      Cmd : I2c_Command_Ref :=
      new I2c_Command;
   begin
      Cmd.Adap_Id := Adap;
      Cmd.Operation_Id    := Op;
      Cmd.Msg_List := new I2c_Msg_List_Array(1 .. Msg_List.Num);
      Cmd.Msg_List(1 .. Msg_List.Num) := Msg_List.Msg_List(1 ..
         Msg_List.Num);
      Set_Status (Op, Waiting);
      Enqueue(Cmd, Prio, The_Commands_Queue);
   end I2c_Transfer;

   -------------------------------------------------------------------
   --  b) I2C_Master_Send, I2C_Master_Recv
   -------------------------------------------------------------------
   --  We also provide two more simple procedures that allow us to
   --  make a single Write or Read i2c operation.
   -------------------------------------------------------------------
   procedure I2c_Master_Send (
         Adap   : in     I2c_Adapter_Id;
         Op     : in     I2c_Operation_Id;
         Prio   : in     I2c_Priority;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags) is
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
   end I2c_Master_Send;

   procedure I2c_Master_Recv (
         Adap   : in     I2c_Adapter_Id;
         Op     : in     I2c_Operation_Id;
         Prio   : in     I2c_Priority;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags) renames I2c_Master_Send;

   -------------------------------------------------------------------
   -- 3) Functions for checking the status of our commands
   -------------------------------------------------------------------
   -- If you want a non-blocking behavior you can use 'Get_Status' to
   -- check the status of your command regularly.
   -- If you want to get blocked until your command is executed you can
   -- use the function 'Wait'. (Of course you have to pass the same
   -- Operation variable of your command)
   -------------------------------------------------------------------
   function Get_Status (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status renames I2c.Daemon.Get_Status;

   function Wait (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status renames I2c.Daemon.Wait;

   procedure Read_Buffer (
         Data   :    out I2c_Data;
         Buffer : in     I2c_Buffer;
         Op     : in     I2c_Operation_Id;
         Stat   :    out I2c_Operation_Status) is
      Cell : I2c_Buffer_Cell;
   begin
      Stat := I2c.Daemon.Wait (Op);
      Cell := Get_Buffer_Cell (Buffer, The_Buffer_List);
      Data := Cell.Data(1 .. Data'Length);
   end Read_Buffer;

end I2c.Daemon.Ada_Interface;

