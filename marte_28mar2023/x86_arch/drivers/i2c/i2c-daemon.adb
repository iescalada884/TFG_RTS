------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                              i 2 c - d a e m o n
--
--                                    body
--
-- File 'i2c-daemon.adb'                                  By Sangorrin
--
-- This is the i2c daemon. Drivers for i2c clients should interact with this
-- daemon through the corresponding interface by sending him commands and
-- waiting to receive responses.
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
with MaRTE.Kernel.Mutexes; pragma Elaborate_All (MaRTE.Kernel.Mutexes);
with MaRTE.Kernel.Condition_Variables;
pragma Elaborate_All (MaRTE.Kernel.Condition_Variables);
------------------------------------------------------------------------------
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with Ada.Exceptions; use Ada.Exceptions;
------------------------------------------------------------------------------
with I2c.Adapters, I2c.Adapters_Table;
use I2c.Adapters, I2c.Adapters_Table;

package body I2c.Daemon is

   package Km renames MaRTE.Kernel.Mutexes;
   package Kcv renames MaRTE.Kernel.Condition_Variables;

   use type MaRTE.Integer_Types.Int;

   -------------------------------------------------------------------
   -- 1) THE OPERATION LIST
   -------------------------------------------------------------------
   -- Operation_ID variables are used by I2C drivers to check the status of
   -- their operations.
   -------------------------------------------------------------------
   type Status_List is array (I2c_Operation_Id) of I2c_Operation_Status;
   type Operation_List is
      record
         List      : Status_List;
         Mutex_Ref : Km.Mutex_Descriptor;
         Cond_Ref  : Kcv.Condition_Descriptor;
      end record;
   The_Operation_List : Operation_List;
   -------------------------------------------------------------------
   --           a) Init
   -------------------------------------------------------------------
   -- Initialize the Operation list.
   -------------------------------------------------------------------
   procedure Init (
         Prio : in     Km.Ceiling_Priority;
         Op : in out Operation_List) is
      Attr      : aliased Km.Attributes;
      Cond_Attr : aliased Kcv.Attributes;
      Ret       : Int;
   begin
      -- Operation list initialization
      Op.List := (others => Not_In_Use);
      -- Mutex initialization
      Op.Mutex_Ref := new Km.Mutex;
      Ret := Km.Pthread_Mutexattr_Init (Attr'access);
      Ret := Km.Pthread_Mutexattr_Setprotocol (Attr'access,
         Km.Highest_Ceiling_Priority);
      Ret := Km.Pthread_Mutexattr_Setprioceiling (Attr'access, Prio);
      Ret := Km.Pthread_Mutex_Init (Op.Mutex_Ref, Attr'access);
      Ret := Km.Pthread_Mutexattr_Destroy (Attr'access);
      -- Cond initialization
      Cond_Attr := Kcv.Default_Attributes;
      Op.Cond_Ref := new Kcv.Condition;
      Ret := Kcv.Pthread_Cond_Init (Op.Cond_Ref, Cond_Attr'access);
   end Init;
   -------------------------------------------------------------------
   --           b) Get_Status of a Operation variable
   -------------------------------------------------------------------
   function Get_Status (
         Id : in     I2c_Operation_Id)
     return I2c_Operation_Status is
      Stat : I2c_Operation_Status;
      Ret  : Int;
   begin
      Ret := Km.Pthread_Mutex_Lock (The_Operation_List.Mutex_Ref);
      Stat := The_Operation_List.List(Id);
      Ret := Km.Pthread_Mutex_Unlock (The_Operation_List.Mutex_Ref);
      return Stat;
   end Get_Status;
   -------------------------------------------------------------------
   --           c) Set_Status of a Operation variable
   -------------------------------------------------------------------
   procedure Set_Status (
         Id   : in     I2c_Operation_Id;
         Stat : in     I2c_Operation_Status) is
      Ret : Int;
   begin
      Ret := Km.Pthread_Mutex_Lock (The_Operation_List.Mutex_Ref);
      The_Operation_List.List(Id) := Stat;
      Ret := Kcv.Pthread_Cond_Broadcast (The_Operation_List.Cond_Ref);
      Ret := Km.Pthread_Mutex_Unlock (The_Operation_List.Mutex_Ref);
   end Set_Status;
   -------------------------------------------------------------------
   --           d) Sleep until the status is not WAITING
   -------------------------------------------------------------------
   function Wait (
         Op : in     I2c_Operation_Id)
     return I2c_Operation_Status is
      Stat : I2c_Operation_Status;
      Ret  : Int;
   begin
      Ret := Km.Pthread_Mutex_Lock (The_Operation_List.Mutex_Ref);
      while (The_Operation_List.List(Op) = Waiting) loop
         Ret := Kcv.Pthread_Cond_Wait (The_Operation_List.Cond_Ref,
            The_Operation_List.Mutex_Ref);
      end loop;
      Stat := The_Operation_List.List(Op);
      Ret := Km.Pthread_Mutex_Unlock (The_Operation_List.Mutex_Ref);
      return Stat;
   end Wait;

   -------------------------------------------------------------------
   -- 2) BUFFERS
   -------------------------------------------------------------------
   procedure Init (
         Prio : in     Km.Ceiling_Priority;
         List : in out I2c_Protected_Buffer_List) is
      Attr : aliased Km.Attributes;
      Ret  : Int;
   begin
      -- Buffer list initialization
      List.List := (others => (False,null,I2c_Default_Flags));
      -- Mutex initialization
      List.Mutex_Ref := new Km.Mutex;
      Ret := Km.Pthread_Mutexattr_Init (Attr'access);
      Ret := Km.Pthread_Mutexattr_Setprotocol (Attr'access,
         Km.Highest_Ceiling_Priority);
      Ret := Km.Pthread_Mutexattr_Setprioceiling (Attr'access, Prio);
      Ret := Km.Pthread_Mutex_Init (List.Mutex_Ref, Attr'access);
      Ret := Km.Pthread_Mutexattr_Destroy (Attr'access);
   end Init;

   procedure Create_Recv_Buff (
         Num_Bytes : in     I2c_Data_Count;
         Buffer    :    out I2c_Buffer;
         List      : in out I2c_Protected_Buffer_List) is
      Ret : Int;
   begin
      Buffer := Null_I2c_Buffer;
      Ret := Km.Pthread_Mutex_Lock (List.Mutex_Ref);
      for I in List.List'range loop
         if not List.List(I).In_Use then
            List.List(I).In_Use := True;
            List.List(I).Data := new I2c_Data(1 .. Num_Bytes);
            List.List(I).Flags := I2c_M_Rd;
            Buffer := I;
            exit;
         end if;
      end loop;
      Ret := Km.Pthread_Mutex_Unlock (List.Mutex_Ref);
   end Create_Recv_Buff;

   procedure Create_Send_Buff (
         Data   : in     I2c_Data;
         Buffer :    out I2c_Buffer;
         List   : in out I2c_Protected_Buffer_List) is
      Ret : Int;
   begin
      Buffer := Null_I2c_Buffer;
      Ret := Km.Pthread_Mutex_Lock (List.Mutex_Ref);
      for I in List.List'range loop
         if not List.List(I).In_Use then
            List.List(I).In_Use := True;
            List.List(I).Data := new I2c_Data'(Data);
            List.List(I).Flags := I2c_Default_Flags;
            Buffer := I;
            exit;
         end if;
      end loop;
      Ret := Km.Pthread_Mutex_Unlock (List.Mutex_Ref);
   end Create_Send_Buff;

   procedure Free (
         Buffer : in     I2c_Buffer;
         List   : in out I2c_Protected_Buffer_List) is
      Ret : Int;
   begin
      Ret := Km.Pthread_Mutex_Lock (List.Mutex_Ref);
      List.List(Buffer).In_Use := False;
      List.List(Buffer).Data := null;
      List.List(Buffer).Flags := I2c_Default_Flags;
      Ret := Km.Pthread_Mutex_Unlock (List.Mutex_Ref);
   end Free;


   function Get_Buffer_Cell (
         Buffer : in     I2c_Buffer;
         List   : in     I2c_Protected_Buffer_List)
     return I2c_Buffer_Cell is
      Ret  : Int;
      Cell : I2c_Buffer_Cell;
   begin
      Ret := Km.Pthread_Mutex_Lock (List.Mutex_Ref);
      Cell.In_Use := List.List(Buffer).In_Use;
      Cell.Data := List.List(Buffer).Data;
      Cell.Flags := List.List(Buffer).Flags;
      Ret := Km.Pthread_Mutex_Unlock (List.Mutex_Ref);
      return Cell;
   end Get_Buffer_Cell;

   -------------------------------------------------------------------
   -- 3)  OUR BELOVED I2C DAEMON ...
   -------------------------------------------------------------------
   task type I2c_Daemon is
      pragma Priority(I2c_Daemon_Prio);
   end I2c_Daemon;

   task body I2c_Daemon is
      Cmd           : I2c_Command_Ref;
      Prio          : I2c_Priority;
      Adap          : I2c_Adapter_Ref;
      I2c_Exception : exception;
   begin
      loop
         begin
            Dequeue(Cmd, Prio, The_Commands_Queue);
            Adap := The_I2c_Adapter_List(Cmd.Adap_Id);
            if (I2c.Adapters.Master_Xfer (Adap.All,Cmd.Msg_List) = -1) then
               Set_Status (Cmd.Operation_Id, I2c_Error);
               raise I2c_Exception;
            else
               Set_Status (Cmd.Operation_Id, Workdone);
            end if;
         exception
            when I2c_Exception =>
               Put("I2C Error (Daemon)");
               New_Line;
            when others =>
               Put("BUG Error");
               New_Line;
         end;
      end loop;
   end I2c_Daemon;
   type I2c_Daemon_Ref is access I2c_Daemon;

   The_I2c_Daemon : I2c_Daemon_Ref;

   -------------------------------------------------------------------
   -- 4)  Initialization Stuff
   -------------------------------------------------------------------
begin
   Init(I2c_Commands_List_Prio, The_Commands_Queue);
   Init(I2c_Operation_List_Prio, The_Operation_List);
   Init(I2c_Buffer_List_Prio, The_Buffer_List);
   -- No the Data structures are initialized we can create the Daemon
   The_I2c_Daemon := new I2c_Daemon;
end I2c.Daemon;

