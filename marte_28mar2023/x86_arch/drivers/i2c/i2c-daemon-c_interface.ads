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

package I2c.Daemon.C_Interface is

   package Char is new Interfaces.C.Pointers(I2c_Data_Count,
      Byte_Type,
      I2c_Data,
      Byte_Type(0));
   use Char;
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
     return I2c_Buffer;
   pragma Export (C, Create_Recv_Buff, "create_recv_buff");

   function Create_Send_Buff (
         Data  : in     Char.Pointer;
         Count : in     I2c_Data_Count)
     return I2c_Buffer;
   pragma Export (C, Create_Send_Buff, "create_send_buff");

   procedure Free_Buffer (
         Buffer : in     I2c_Buffer);
   pragma Export (C, Free_Buffer, "free_buffer");
   -------------------------------------------------------------------
   --  b) C_Msg, C_Msg_List
   -------------------------------------------------------------------
   --  The messages are used in the 'I2C_Transfer' function to tell
   --  the I2C Daemon to do a group of i2c operations. This operations
   --  are done by writing a 'Restart' bit between them.
   -------------------------------------------------------------------
   type C_Msg is
      record
         Addr   : I2c_Address;
         Flags  : I2c_Flags;
         Buffer : I2c_Buffer;
      end record;
   pragma Convention( C, C_Msg);

   subtype C_Msg_Count is Integer;
   type C_Msg_List is array (C_Msg_Count range <>) of aliased C_Msg;

   package Msg is new Interfaces.C.Pointers(C_Msg_Count,
      C_Msg,
      C_Msg_List,
      C_Msg'(0,0,0));
   use Msg;

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
     return Int;
   pragma Export (C, I2c_Transfer, "i2c_transfer");

   -------------------------------------------------------------------
   --  b) I2C_Master_Send, I2C_Master_Recv
   -------------------------------------------------------------------
   --  We also provide two more simple procedures that allow us to
   --  make a single Write or Read i2c operation.
   -------------------------------------------------------------------
   function I2c_Master_Send (
         Adap   : in     I2c_Adapter_Id;
         Op     : in     I2c_Operation_Id;
         Prio   : in     I2c_Priority;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags)
     return Int;
   pragma Export (C, I2c_Master_Send, "i2c_master_send");

   function I2c_Master_Recv (
         Adap   : in     I2c_Adapter_Id;
         Op     : in     I2c_Operation_Id;
         Prio   : in     I2c_Priority;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags)
     return Int;
   pragma Export (C, I2c_Master_Recv, "i2c_master_recv");

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
     return I2c_Operation_Status;
   pragma Export (C, I2c_Get_Status, "i2c_get_status");

   function I2c_Wait (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status;
   pragma Export (C, I2c_Wait, "i2c_wait");

   function Read_Buffer (
         Data   : in     Char.Pointer;
         Count  : in     I2c_Data_Count;
         Buffer : in     I2c_Buffer;
         Op     : in     I2c_Operation_Id)
     return I2c_Operation_Status;
   pragma Export (C, Read_Buffer, "read_buffer");

end I2c.Daemon.C_Interface;

