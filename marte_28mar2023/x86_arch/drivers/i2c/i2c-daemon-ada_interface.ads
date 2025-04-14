------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                   i 2 c - d a e m o n - a d a _ i n t e r f a c e
--
--                                    spec
--
-- File 'i2c-daemon-ada_interface.ads'                  By Sangorrin
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

package I2c.Daemon.Ada_Interface is

   -------------------------------------------------------------------
   -- 1) Functions to create the Messages that will be sent to the Daemon
   -------------------------------------------------------------------
   --  a) I2C_Flags
   -------------------------------------------------------------------
   --  The FLAGS may be useful in the future to add other features
   --  like supporting Ten Bit addresses, etc...
   -------------------------------------------------------------------
   procedure Flags_Init (
         Flags : in out I2c_Flags);

   procedure Flags_Set_Tenbitaddress (
         Flags : in out I2c_Flags);

   procedure Flags_Set_Nostart (
         Flags : in out I2c_Flags);

   procedure Flags_Set_Revdiraddr (
         Flags : in out I2c_Flags);

   procedure Flags_Set_Ignorenack (
         Flags : in out I2c_Flags);

   procedure Flags_Set_Nordack (
         Flags : in out I2c_Flags);
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
     return I2c_Buffer;

   function Create_Send_Buff (
         Data : in     I2c_Data)
     return I2c_Buffer;

   procedure Free (
         Buffer : in     I2c_Buffer);
   -------------------------------------------------------------------
   --  c) I2C_Msg, I2C_Msg_List functions
   -------------------------------------------------------------------
   --  The messages are used in the 'I2C_Transfer' function to tell
   --  the I2C Daemon to do a group of i2c operations. This operations
   --  are done by writing a 'Restart' bit between them.
   -------------------------------------------------------------------
   subtype I2c_Msg_Ada is I2c_Msg;
   type I2c_Msg_Ada_List is private;

   procedure Create_Msg (
         Msg    : in out I2c_Msg_Ada;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags);

   procedure Create_Msg_List (
         Msg_Max  : in     Positive;
         Msg_List : in out I2c_Msg_Ada_List);

   procedure Add (
         Msg      : in     I2c_Msg_Ada;
         Msg_List : in out I2c_Msg_Ada_List);

   procedure Empty (
         Msg_List : in out I2c_Msg_Ada_List);

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
         Msg_List : in     I2c_Msg_Ada_List);
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
         Flags  : in     I2c_Flags);

   procedure I2c_Master_Recv (
         Adap   : in     I2c_Adapter_Id;
         Op     : in     I2c_Operation_Id;
         Prio   : in     I2c_Priority;
         Addr   : in     I2c_Address;
         Buffer : in     I2c_Buffer;
         Flags  : in     I2c_Flags);

   -------------------------------------------------------------------
   -- 3) Get_Status, Wait, Read_Buffer
   -------------------------------------------------------------------
   --   If you want a non-blocking behavior you can use 'Get_Status' to
   -- check the status of your command regularly.
   -- If you want to get blocked until your command is executed you can
   -- use the function 'Wait' or 'Read_Buffer'. (Of course you have to
   -- pass the same Operation variable of your command)
   -------------------------------------------------------------------
   function Get_Status (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status;

   function Wait (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status;

   procedure Read_Buffer (
         Data   :    out I2c_Data;
         Buffer : in     I2c_Buffer;
         Op     : in     I2c_Operation_Id;
         Stat   :    out I2c_Operation_Status);

private

   type I2c_Msg_Ada_List is
      record
         Msg_List : I2c_Msg_List;
         Num      : Natural;
      end record;

end I2c.Daemon.Ada_Interface;

