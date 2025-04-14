----------------------------------------------------------------------------
--  ---------------------           RT-EP           ------------------------
----------------------------------------------------------------------------
--                         Copyright (C) 2003-2005
--                     Universidad de Cantabria, SPAIN
--                        http://www.ctr.unican.es/
--
--    This program is free software; you can redistribute it and/or
--    modify it under the terms of the GNU General Public
--    License as published by the Free Software Foundation; either
--    version 2 of the License, or (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--    General Public License for more details.
--
--    You should have received a copy of the GNU General Public
--    License along with this program; if not, write to the
--    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
--    Boston, MA 02111-1307, USA.
--
--    As a special exception, if other files instantiate generics from
--    this unit, or you link this unit with other files to produce an
--    executable, this  unit  does not  by itself cause  the resulting
--    executable to be covered by the GNU General Public License. This
--    exception does not however invalidate any other reasons why the
--    executable file  might be covered by the  GNU Public License.
--  ------------------------------------------------------------------------
--                                                     RT-EP /Ada95 {V1.1}
--
--                'r t e p - p r o t o c o l - c o r e . a d b'
--
--                                     Ada
--
--
--  File 'rtep-protocol-core.adb'                                  By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  RT-EP Core implementation. The Main Communication Task and Error Handling
--  Stuff.
--
--
--
--
-----------------------------------------------------------------------------

with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Synchronous_Task_Control;
with Ada.Exceptions;
--
with POSIX; pragma Elaborate_All (POSIX);
with POSIX.Signals; pragma Elaborate_All (POSIX.Signals);
with POSIX_Timers; pragma Elaborate_All (POSIX_Timers);
with MaRTE.POSIX_Constants;
--
with RTEP.Protocol.Stations;
pragma Elaborate_All (RTEP.Protocol.Stations);
with RTEP.Protocol.Packet;
with RTEP.Protocol.Buffers;
with RTEP.Protocol.Servers;
with RTEP.Protocol.System;
pragma Elaborate_All (RTEP.Protocol.System);
--
with RTEP_Debug_Functions; use RTEP_Debug_Functions;
pragma Elaborate_All (RTEP_Debug_Functions);

package body RTEP.Protocol.Core is

   use type MaRTE.Integer_Types.Int;

   ------------------------------------
   --  0) INTERNAL TYPES AND CONSTANTS
   --  1) GLOBAL VARIABLES
   --  2) MACROS
   --  3) RING TIMEOUTS HANDLER
   --  4) MAIN TASK
   --------------------------------

   --------------------------------------
   -- 0) INTERNAL TYPES AND CONSTANTS --
   --------------------------------------
   --   a) RTEP Constants for Packet Types
   REQ : constant := Interfaces.Unsigned_8 (Character'Pos ('R'));
   ACK : constant := Interfaces.Unsigned_8 (Character'Pos ('A'));
   TOK : constant := Interfaces.Unsigned_8 (Character'Pos ('T'));
   INF : constant := Interfaces.Unsigned_8 (Character'Pos ('I'));
   PER : constant := Interfaces.Unsigned_8 (Character'Pos ('P'));

   --   b) TimeOut Signal for retransmisions
   RTEP_Timeout_Signal : constant POSIX.Signals.Realtime_Signal :=
      MaRTE.POSIX_Constants.SIGRTMAX;

   --   c) RTEP Stations States
   type RTEP_States is (Offline, Idle, Send_Initial_Token, Check_Token,
      Send_Permission, Send_Token, Send_Info, Recv_Info);

   --   d) RTEP Token packet structure
   RTEP_Token_Packet_Length: constant := 12 + RTEP_Multicast_Msg_Length;
   type RTEP_Token_Packet is record
      Packet_Id          : Interfaces.Unsigned_8;
      Prio               : Interfaces.Unsigned_8;
      Packet_Number      : Interfaces.Unsigned_16;
      Token_Master_Id    : Interfaces.Unsigned_16;
      Station_Prio_Id    : Interfaces.Unsigned_16;
      Failure_Station_Id : Interfaces.Unsigned_16;
      Failure_Station    : Interfaces.Unsigned_8;
      Mult_Type          : Interfaces.Unsigned_8;
      Multicast_Info     : aliased Multicast_Info_Type;
   end record;
   pragma Pack (RTEP_Token_Packet);
   for RTEP_Token_Packet'Size use RTEP_Token_Packet_Length * 8;

   --   e) RTEP Info packet structure
   RTEP_Info_Packet_Header : constant := 8;
   RTEP_Info_Packet_Length : constant :=
      RTEP.Max_RTEP_MTU + RTEP_Info_Packet_Header;
   type RTEP_Info_Packet is record
      Packet_Id     : Interfaces.Unsigned_8 := INF;
      Prio          : Interfaces.Unsigned_8;
      Packet_Number : Interfaces.Unsigned_16;
      Channel_Id    : Interfaces.Unsigned_16;
      Info_Length   : Interfaces.Unsigned_16;
      Info          : Stream_Element_Array (1 .. RTEP.Max_RTEP_MTU);
   end record;
   pragma Pack (RTEP_Info_Packet);
   for RTEP_Info_Packet'Size use RTEP_Info_Packet_Length * 8;

   --  f) Buffer Types
   type RTEP_Token_Stream_Array is new Stream_Element_Array
      (1 .. RTEP_Token_Packet_Length);
   for RTEP_Token_Stream_Array'Size use RTEP_Token_Packet_Length * 8;

   type RTEP_Info_Stream_Array is
     new Stream_Element_Array (1 .. RTEP_Info_Packet_Length);
   for RTEP_Info_Stream_Array'Size use RTEP_Info_Packet_Length * 8;

   type RTEP_Token_Stream_Array_Ac is access all RTEP_Token_Stream_Array;
   type RTEP_Token_Packet_Ac is access all RTEP_Token_Packet;
   type RTEP_Info_Stream_Array_Ac is access all RTEP_Info_Stream_Array;
   type RTEP_Info_Packet_Ac is access all RTEP_Info_Packet;

   -------------------------
   -- 1) GLOBAL VARIABLES --
   -------------------------
   --   a) Rx Buffers
   Rx_Buffer            : Stream_Element_Array (1 .. RTEP_Info_Packet_Length);
   Rx_Buffer_Last       : Stream_Element_Offset;
   Rx_RTEP_Token_Packet : RTEP_Token_Packet;
   Rx_RTEP_Info_Packet  : RTEP_Info_Packet;

   --   b) Tx Buffers
   Tx_Buffer_Info_Ac    : RTEP_Info_Stream_Array_Ac;
   Tx_Buffer_Info_Last  : Stream_Element_Offset;
   Tx_Buffer_Token_Ac   : RTEP_Token_Stream_Array_Ac;
   Tx_RTEP_Token_Packet : aliased RTEP_Token_Packet;
   Tx_RTEP_Info_Packet  : aliased RTEP_Info_Packet;

   --   c) Error Handling Stuff
   Last_Station_State : RTEP_States := Offline; --  last state
   Station_State : RTEP_States := Offline; --  current state
   Error_Retry_Counter  : Integer := 0; --  retries when timout expires
   Last_Sent_Station_ID : Station_ID; --  last destination station in tx
   Last_Packet_Number : Interfaces.Unsigned_16 := 0; -- Sequence number
   Check_For_Error : Boolean := False; --  check the media for errors (timers)
   Check_For_Valid_Station : Boolean := False; -- valid_station to 0 in a round

   --   d) Timers
   Timeout_Timer_Id : POSIX_Timers.Timer_Id;
   Timer_Signal_Event : POSIX.Signals.Signal_Event;
   Timeout_Time  : POSIX_Timers.Timer_State;
   RTEP_Timer_Options : POSIX_Timers.Timer_Options;

   --   e) Initialization
   RTEP_Init_Object : Ada.Synchronous_Task_Control.Suspension_Object;
   Initializated : Boolean := False;
   pragma Volatile (Initializated);
   Initialization_Sem  : aliased MS.Semaphore;

   --   f) other global variables
   Me, Next : Station_ID;
   Is_Token_Master : Boolean := False;

   ----------------
   --  2) MACROS --
   ----------------
   --   a) A comparator auxiliar function
   function "=" (Left : Interfaces.Unsigned_16; Right : Station_ID)
      return Boolean is
   begin
      return  Left = Interfaces.Unsigned_16
        (Stations.Get_Station_Position (Right));
   end "=";

   --   b) This procedure will set to null all the packet fields
   procedure Reset_RTEP_Token_Packet
     (Token : in out RTEP_Token_Packet) is
   begin
      Token.Packet_Id           := 0;
      Token.Prio                := 0;
      Token.Packet_Number       := 0;
      Token.Token_Master_Id     := 0;
      Token.Failure_Station     := 0;
      Token.Failure_Station_Id  := 0;
      Token.Station_Prio_Id     := 0;
      Token.Mult_Type           := No_Multicast;
   end Reset_RTEP_Token_Packet;

   --   c) Unchecked_Conversion functions for buffers
   function To_Stream_Array is new Ada.Unchecked_Conversion
      (RTEP_Token_Packet, RTEP_Token_Stream_Array);
   function To_Stream_Array is new Ada.Unchecked_Conversion
      (RTEP_Info_Packet, RTEP_Info_Stream_Array);
   function To_Stream_Array_Ac is new Ada.Unchecked_Conversion
      (RTEP_Token_Packet_Ac, RTEP_Token_Stream_Array_Ac);
   function To_Stream_Array_Ac is new Ada.Unchecked_Conversion
      (RTEP_Info_Packet_Ac, RTEP_Info_Stream_Array_Ac);

   function To_Token_Packet is new Ada.Unchecked_Conversion
      (RTEP_Token_Stream_Array, RTEP_Token_Packet);
   function To_Token_Packet_Ac is new Ada.Unchecked_Conversion
      (RTEP_Token_Stream_Array_Ac, RTEP_Token_Packet_Ac);

   function To_Info_Packet is new Ada.Unchecked_Conversion
      (RTEP_Info_Stream_Array, RTEP_Info_Packet);
   function To_Info_Packet_Ac is new Ada.Unchecked_Conversion
      (RTEP_Info_Stream_Array_Ac, RTEP_Info_Packet_Ac);

   --   d) Get_Highest_Priority
   type Highest_Priority_Queue_Type is (Tx_Q, Server_Q, Mutex_Q);

   procedure Get_Highest_Priority
      (HPrio      : out RTEP.Priority;
       HP_Queue   : out Highest_Priority_Queue_Type;
       Id         : out Server_Id;
       M_Id       : out Distributed_Mutex_Id;
       Are_Msg_To_Send  : out Boolean) --  If there are messages to send
   is
      Packet_Ac : Packet.RTEP_Packet_Ac;
      Tx_Chan_Prio, Server_Prio, Mutex_Prio : Priority;
      Not_Empty : array (Highest_Priority_Queue_Type) of Boolean;
   begin
      --  Check the fixed priority tx queue
      Buffers.FP_Tx_Queue_Read_Packet (Packet_Ac, Tx_Chan_Prio);
      Not_Empty (Tx_Q) := not Packet."=" (Packet_Ac, null);
      --  Check the servers queue
      if (Enable_Servers'First) then
         Servers.Get_Max_Priority_Server
            (Id, Server_Prio, Not_Empty (Server_Q));
      else
         Not_Empty (Server_Q) := False;
      end if;
      --  Check the mutexes
      --  Get_Max_Priority_Mutex (M_Id, Mutex_Prio, Not_Empty (Mutex_Q));
      if (Enable_Mutexes'First) then
        Mutex_Prio := Priority'First; -- 1
        Not_Empty (Mutex_Q) := False;
        for I in Distributed_Mutex_Id'Range loop
            case The_Dist_Mutexes (I).Action is
                when Lock =>
                if (not The_Dist_Mutexes (I).Locked) then
                    if The_Dist_Mutexes (I).Prio >= Mutex_Prio then
                        M_Id := I;
                        Mutex_Prio := The_Dist_Mutexes (I).Prio;
                        Not_Empty (Mutex_Q) := True;
                    end if;
                end if;
                when Unlock =>
                if The_Dist_Mutexes (I).Prio >= Mutex_Prio then
                    M_Id := I;
                    Mutex_Prio := The_Dist_Mutexes (I).Prio;
                    Not_Empty (Mutex_Q) := True;
                end if;
                when None => null;
            end case;
        end loop;
      else
        Not_Empty (Mutex_Q) := False;
      end if;
      --  And finally, choose the one with the highest priority
      HPrio := Priority'First; -- 1
      if Not_Empty (Tx_Q) then
         HPrio := Tx_Chan_Prio;
         HP_Queue := Tx_Q;
      end if;
      if Not_Empty (Server_Q) then
         if (Server_Prio >= HPrio) then -- ">=" for HPrio=1 but Tx_Q empty
            HPrio := Server_Prio;
            HP_Queue := Server_Q;
         end if;
      end if;
      if Not_Empty (Mutex_Q) then
         if (Mutex_Prio >= HPrio) then -- ">=" for HPrio=1 but Tx_Q and Server_Q empty
            HPrio := Mutex_Prio;
            HP_Queue := Mutex_Q;
         end if;
      end if;
      Are_Msg_To_Send := Not_Empty (Tx_Q) Or
                         Not_Empty (Server_Q) Or
                         Not_Empty (Mutex_Q);
   end Get_Highest_Priority;

   --   e) Create_Initial_Token
   --  Create_Initial_Token reads the hightest priority element in the Tx queue
   --  and write in Token the token information we will tx to the
   --  next station.
   --  If we read no element from the queue because it is empty, a generic
   --  holding-link token will be created
   procedure Create_Initial_Token
      (Token : in out RTEP_Token_Packet)
   is
      HPrio : Priority;
      HP_Queue : Highest_Priority_Queue_Type;
      Id : Server_Id;
      M_Id : Distributed_Mutex_Id;
      Are_Msg_To_Send : Boolean;
   begin
      --  Set the type of token.
      Token.Packet_Id := TOK;
      --  Set the Current Station.
      Token.Station_Prio_Id := Interfaces.Unsigned_16
        (Stations.Get_Station_Position (Me));
      --  Set the priority.
      --  We have to compare the highest priority of the Tx_Channel and
      --  the highest priority of the active servers
      Get_Highest_Priority (HPrio, HP_Queue, Id, M_Id, Are_Msg_To_Send);
      if Are_Msg_To_Send then
         Token.Prio := Interfaces.Unsigned_8 (HPrio);
      else
         Token.Prio := 0;
      end if;
      --  Since the station becomes the token_master station, it stores its
      --  address on the token
      Token.Token_Master_Id := Interfaces.Unsigned_16
        (Stations.Get_Station_Position (Me));
      --  We asign the data sequence number.
      Token.Packet_Number := Last_Packet_Number + 1;
   end Create_Initial_Token;

   --   f) Prepare_Msg_To_Send
   --  reads the packet with the highest priority to be sent and writes it into
   --  Tx_RTEP_Info_Packet (or Tx_RTEP_Token_Packet if it is a multicast msg)
   procedure Prepare_Msg_To_Send (Dest_Station : out Station_ID) is
      Packet_Ac : Packet.RTEP_Packet_Ac;
      Last : Stream_Element_Offset;
      Id : RTEP.Server_Id;
      HPrio : Priority;
      Are_Msg_To_Send : Boolean;
      Mult_Type : Multicast_Type;
      Chan : Channel;
      Mult_Msg : Multicast_Msg_Ptr;
      HP_Queue : Highest_Priority_Queue_Type;
      M_Id : Distributed_Mutex_Id;
      Dist_Mutex_Ac : RTEP_Dist_Mutex_Ptr;
   begin
      --  We extract the packet with the highest priority
      Get_Highest_Priority (HPrio, HP_Queue, Id, M_Id, Are_Msg_To_Send);
      if Are_Msg_To_Send then
         case HP_Queue is
            when Tx_Q =>
               Buffers.FP_Tx_Queue_Extract_Packet (Packet_Ac, HPrio);
               Mult_Type := Packet.Get_Multicast_Type (Packet_Ac.all);
            when Server_Q =>
               Buffers.Server_Tx_Queues_Extract_Packet (Packet_Ac, Id);
               Mult_Type := Packet.Get_Multicast_Type (Packet_Ac.all);
            when Mutex_Q =>
               case The_Dist_Mutexes (M_Id).Action is
                  when Lock => Mult_Type := Lock_Dist_Mutex;
                  when Unlock => Mult_Type := Unlock_Dist_Mutex;
                  when None => ERROR ("mutex action none!");
               end case;
         end case;
      else
         ERROR ("Prepare_Msg_To_Send, there should be a packet!!");
         raise Unexpected_Error;
      end if;
      --  We do different things depending on the type of msg
      case Mult_Type is
         when No_Multicast =>  --  This is a normal unicast message.
            Dest_Station := Packet.Get_Station_Id (Packet_Ac.all);
            Chan := Packet.Get_Channel_Id (Packet_Ac.all);
            Tx_RTEP_Info_Packet.Packet_Id := INF;
            Tx_RTEP_Info_Packet.Prio := Interfaces.Unsigned_8 (HPrio);
            Tx_RTEP_Info_Packet.Channel_Id := Interfaces.Unsigned_16 (Chan);
            Tx_RTEP_Info_Packet.Packet_Number := Last_Packet_Number + 1;
            Packet.Read_RTEP_Packet
               (Packet_Ac.all, Tx_RTEP_Info_Packet.Info, Last);
            Tx_RTEP_Info_Packet.Info_Length := Interfaces.Unsigned_16 (Last);
         when Multicast_Message =>
            Dest_Station := Packet.Get_Station_Id (Packet_Ac.all);
            Chan := Packet.Get_Channel_Id (Packet_Ac.all);
            Tx_RTEP_Token_Packet.Mult_Type := Multicast_Message;
            Mult_Msg:=To_Multi_Msg(Tx_RTEP_Token_Packet.Multicast_Info'Access);
            Mult_Msg.Multicast_Addr := Interfaces.Unsigned_16 (Dest_Station);
            Mult_Msg.Channel_Id := Interfaces.Unsigned_16 (Chan);
            Mult_Msg.Prio := Interfaces.Unsigned_8 (HPrio);
            Packet.Read_RTEP_Packet (Packet_Ac.all, Mult_Msg.Info, Last);
            Mult_Msg.Info_Length := Interfaces.Unsigned_8 (Last);
         when Lock_Dist_Mutex =>
            Tx_RTEP_Token_Packet.Mult_Type := Lock_Dist_Mutex;
            Dist_Mutex_Ac := To_Dist_Mutex
               (Tx_RTEP_Token_Packet.Multicast_Info'Access);
            Dist_Mutex_Ac.Mutex_Id := M_Id;
            Dist_Mutex_Ac.Holder := Me;
            The_Dist_Mutexes (M_Id).Locked := True;
            The_Dist_Mutexes (M_Id).Holder := Me;
            The_Dist_Mutexes (M_Id).Action := None;
            if MS.Post (The_Dist_Mutexes (M_Id).Sem'Access) /= 0 then
               raise Unexpected_Error;
            end if;
            Dest_Station := The_Dist_Mutexes (M_Id).Group;
            return;
         when Unlock_Dist_Mutex =>
            Tx_RTEP_Token_Packet.Mult_Type := Unlock_Dist_Mutex;
            Dist_Mutex_Ac := To_Dist_Mutex
               (Tx_RTEP_Token_Packet.Multicast_Info'Access);
            Dist_Mutex_Ac.Mutex_Id := M_Id;
            The_Dist_Mutexes (M_Id).Locked := False;
            The_Dist_Mutexes (M_Id).Action := None;
            if MS.Post (The_Dist_Mutexes (M_Id).Sem'Access) /= 0 then
               raise Unexpected_Error;
            end if;
            Dest_Station := The_Dist_Mutexes (M_Id).Group;
            return;
      end case;
      --  If it is synchronized and comes from a server we signal the tx
      if (HP_Queue = Server_Q) and Packet.Is_Synchronyzed (Packet_Ac.all) then
         Check_NZ (PSS.Signal (Is_Result_Available (Id)));
      end if;
      --  We destroy the Unused RTEP_Packet
      Packet.Destroy_RTEP_Packet (Packet_Ac);
   exception
      when Buffers.Empty =>  --  MIRAR: Buffers.Empty
         ERROR ("Empty Queue ");
      when Storage_Error =>  --  MIRAR: Storage_Error
         ERROR ("Storage_Error ");
   end Prepare_Msg_To_Send;

   --   g) Handle_Received_Packet
   --  writes the received packet from Source_Station to the Rx queue
   procedure Handle_Received_Packet
      (Info_Packet : in out RTEP_Info_Packet;
      Source_Station : in Station_ID)
   is
      Packet_Ac : Packet.RTEP_Packet_Ac;
      RTEP_Channel : Channel;
      Prio : Priority;
      Last : Stream_Element_Offset;
   begin
      --  We alloc. a new packet.
      Packet_Ac := Packet.Get_New_RTEP_Packet;
      Packet.Put_Station_Id (Packet_Ac.all, Source_Station);
      --  We extract the Rx channel.
      RTEP_Channel := Channel (Info_Packet.Channel_Id);
      --  For coherence with the structure.
      Packet.Put_Channel_Id (Packet_Ac.all, RTEP_Channel);
      Prio := Priority (Info_Packet.Prio);
      Last := Stream_Element_Offset (Info_Packet.Info_Length);
      --  We copy the info in the RTEP_Packet.
      Packet.Copy_Stream_Array_In_RTEP_Packet
        (Info_Packet.Info (1 .. Last), Packet_Ac.all);
      --  Now that everything is in order.
      Buffers.Rx_Queues_Insert_Packet (RTEP_Channel, Packet_Ac, Prio);
   exception
      when Packet.No_More_Free_Packets =>
         --  PACKET IS LOST!
         ERROR ("Packet.No_More_Free_Packets");
      when Packet.RTEP_Packet_Not_Allocated =>
         --  PACKET IS LOST! (Should not happen)
         ERROR ("Packet.RTEP_Packet_Not_Allocated");
      when Buffers.Full_Of_Packets =>
         --  PACKET IS LOST!
         ERROR ("Buffers.Full_Of_Packets");
         Packet.Destroy_RTEP_Packet (Packet_Ac);
      when Packet.RTEP_Packet_Full =>
         --  PACKET IS LOST!
         ERROR ("Packet.RTEP_Packet_Full");
         Packet.Destroy_RTEP_Packet (Packet_Ac);
   end Handle_Received_Packet;

   --  h) Check_Token_Prio
   --  takes the token his_token and check its priority with
   --  the priority of the highest element in the queue and return the
   --  highest priority element of both. If no error returns 0
   procedure Check_Token_Prio
      (Token : in out RTEP_Token_Packet)
   is
      HPrio : Priority;
      HP_Queue : Highest_Priority_Queue_Type;
      Are_Msg_To_Send : Boolean;
      Id : RTEP.Server_Id;
      M_Id : Distributed_Mutex_Id;
   begin
      --  Set the type of token.
      Token.Packet_Id := TOK;
      --  Set the priority.
      --  We have to compare the highest priority of the Tx_Channel and
      --  the highest priority of the active servers with the received
      --  priority
      Get_Highest_Priority (HPrio, HP_Queue, Id, M_Id, Are_Msg_To_Send);
      if Are_Msg_To_Send then
         if Token.Prio < Interfaces.Unsigned_8 (HPrio) then
            DEBUG ("Check token prio - Overwrite", Enable_DEBUG_States'First);
            Token.Prio := Interfaces.Unsigned_8 (HPrio);
            Token.Station_Prio_Id := Interfaces.Unsigned_16
              (Stations.Get_Station_Position (Me));
         end if;
      else
         --  Queue Empty.
         --  In case that our queue AND the predecessor queue are empty, we
         --  change the Station_Prio_Id field in order to circulate the token,
         --  in case the predessors hasn't got the queue empty we do nothing
         --  to the token and we pass it.
         if Token.Prio = 0 then
            --  Set the Current Station.
            Token.Station_Prio_Id := Interfaces.Unsigned_16
              (Stations.Get_Station_Position (Me));
         end if;
      end if;
      --  We asign the data sequence number.
      Token.Packet_Number := Last_Packet_Number + 1;
   end Check_Token_Prio;

   --  j) Free_Failure_Station_Mutex
   --  This procedure is called when the network detects a Failure Station to
   --  check if the failure station had a mutex locked
   procedure Free_Failure_Station_Mutex
      (Station : in Station_ID)
   is
   begin
      if Enable_Mutexes'First then
         for I in Distributed_Mutex_Id'Range loop
            DEBUG ("Free_Failure_Station_Mutex", Enable_DEBUG_Failure'First);
            if The_Dist_Mutexes (I).Locked and then
               The_Dist_Mutexes (I).Holder = Station then
               The_Dist_Mutexes (I).Locked := False;
            end if;
         end loop;
      end if;
   end Free_Failure_Station_Mutex;

   ------------------------------
   -- 3) RING TIMEOUTS HANDLER --
   ------------------------------
   --  Ring_Handler will handle the station state when detecting an error.
   procedure Ring_Handler is
   begin
      DEBUG ("Ring_Handler: Last="
         &RTEP_States'Image (Last_Station_State)&" Now="
         &RTEP_States'Image (Station_State)&" Retry="
         &Integer'Image (Error_Retry_Counter)&" Stat="
         &Station_ID'Image(Last_Sent_Station_ID),
         Enable_DEBUG_Timeouts'First);

      if Last_Station_State = Offline then
         --  In this case we are still initializating the ring.
         --  Send the packet "request" to the station.
         System.Send_RTEP_Packet (Last_Sent_Station_ID,
            Stream_Element_Array (Tx_Buffer_Token_Ac.all));
         return;
      end if;

      if Error_Retry_Counter < RTEP.RTEP_Error_Max_Retries then
         --  If the Retry Counter is less than the maximun we suposse
         --  the frame is lost or the receiver station takes too long in
         --  response. so we retransmit the frame.

         --  We have to check if the failure station is one of the
         --  inhibited ones.
         if Stations.Valid_Station_Id (Last_Sent_Station_ID) = False then
            Is_Token_Master := True;
            --  We build a new token
            Create_Initial_Token (Tx_RTEP_Token_Packet);

            --  And tx to the successor.
            System.Send_RTEP_Packet (Next,
               Stream_Element_Array (Tx_Buffer_Token_Ac.all));

            Last_Sent_Station_ID := Next;
            Last_Station_State := Send_Initial_Token;
            --  This is done to reset the value of Retries
            Error_Retry_Counter := 0;
         else
            DEBUG ("Ring_Handler: Retransmitting packet!",
               Enable_DEBUG_Timeouts'First);

            if Last_Station_State = Send_Info then
               System.Send_RTEP_Packet (Last_Sent_Station_ID,
                  Stream_Element_Array
                  (Tx_Buffer_Info_Ac (1..Tx_Buffer_Info_Last)));
            else
               System.Send_RTEP_Packet (Last_Sent_Station_ID,
                  Stream_Element_Array (Tx_Buffer_Token_Ac.all));
            end if;
            --  Since there has been a retransmision.
            Error_Retry_Counter := Error_Retry_Counter + 1;
         end if; --  Stations.Valid_Station_Id

      else --  We have exceded the maximun retries

         --  In case it's the last retry, we bypass the successor and makes
         --  the new successor the successor of the successor.
         case Last_Station_State is

            when Send_Initial_Token | Send_Token =>

               Check_For_Valid_Station := True;
               Tx_RTEP_Token_Packet.Failure_Station := 1;
               Tx_RTEP_Token_Packet.Failure_Station_Id :=
                 Interfaces.Unsigned_16 (Stations.Get_Station_Position
                                         (Last_Sent_Station_ID));
               --  Check if the station was the Token Master Station.
               if Tx_RTEP_Token_Packet.Token_Master_Id =
                 Last_Sent_Station_ID then
                  --  We check if the highest priority on the token is the
                  --  broken Token Master
                  if Tx_RTEP_Token_Packet.Station_Prio_Id =
                    Last_Sent_Station_ID or Tx_RTEP_Token_Packet.Prio = 0 then
                     --  In this case we return to Send Initial Token State.
                     Is_Token_Master := True;
                     Create_Initial_Token (Tx_RTEP_Token_Packet);
                     Last_Station_State := Send_Initial_Token;
                  else -- We have to do token masters rol.
                     Tx_RTEP_Token_Packet.Packet_Id := PER;
                     Last_Station_State := Send_Permission;
                  end if;
               end if;
               --  We inhibit the failure station.
               Stations.Inhibit_Station_Id (Last_Sent_Station_ID);
               --  We check if the failure station had a distributed mutex
               Free_Failure_Station_Mutex (Last_Sent_Station_ID);
               --  The failurer succesor must be replaced.
               Next := Stations.Get_Next_Station (Last_Sent_Station_ID);

               System.Send_RTEP_Packet (Next,
                  Stream_Element_Array (Tx_Buffer_Token_Ac.all));

               Last_Sent_Station_ID := Next;
               Error_Retry_Counter := 0;

            when Send_Info | Send_Permission =>
               Check_For_Valid_Station := True;
               Tx_RTEP_Token_Packet.Failure_Station := 1;
               Tx_RTEP_Token_Packet.Failure_Station_Id :=
                 Interfaces.Unsigned_16 (Stations.Get_Station_Position
                                         (Last_Sent_Station_ID));
               --  We inhibit the failure station.
               Stations.Inhibit_Station_Id (Last_Sent_Station_ID);
               --  We check if the failure station had a distributed mutex
               Free_Failure_Station_Mutex (Last_Sent_Station_ID);
               --  the failure station is NOT neccessary our successor
               if Last_Sent_Station_ID = Next then
                  Next := Stations.Get_Next_Station (Last_Sent_Station_ID);
               end if;

               Is_Token_Master := True;
               Create_Initial_Token (Tx_RTEP_Token_Packet);

               --  Send the packet to the successor.
               System.Send_RTEP_Packet (Next,
                  Stream_Element_Array (Tx_Buffer_Token_Ac.all));

               Last_Station_State := Send_Initial_Token;
               Last_Sent_Station_ID := Next;
               Error_Retry_Counter := 0;
            when others =>
               ERROR ("Logical Error");
         end case;
      end if; --  end If MAX Retries...
   exception
      when System.Network_Error =>
         --  MIRAR: Network_Error
         ERROR ("Network_Error");
      when Station_Not_Found =>
         --  MIRAR: Station_not_Found
         ERROR ("Station_Not_Found");
   end Ring_Handler;

   task RTEP_Timeout_Task is
      pragma Priority (RTEP.RTEP_Task_Prio);
   end RTEP_Timeout_Task;

   task body RTEP_Timeout_Task is
      Sig_Set : POSIX.Signals.Signal_Set;
      Sig_Info : POSIX.Signals.Signal_Info;
   begin
      POSIX.Signals.Delete_All_Signals (Sig_Set);
      POSIX.Signals.Add_Signal (Sig_Set, RTEP_Timeout_Signal);
      loop
         --  Wait for the signal
         <<Wait_For_Signal>>
         Sig_Info := POSIX.Signals.Await_Signal (Sig_Set);
         --  When the signal arrives...
         if POSIX.Signals."/=" (POSIX.Signals.Get_Signal (Sig_Info),
                                RTEP_Timeout_Signal) then
            ERROR ("RTEP_Timeout_Task: Received signal not for us. ");
            goto Wait_For_Signal;
         end if;

         POSIX_Timers.Disarm_Timer (Timeout_Timer_Id);
         Ring_Handler;
         POSIX_Timers.Arm_Timer (Timeout_Timer_Id,
                                 RTEP_Timer_Options,
                                 Timeout_Time);
      end loop;
   exception
      when E : others =>
         ERROR ("RTEP_Timeout_Task: "&Ada.Exceptions.Exception_Message (E));
   end RTEP_Timeout_Task;

   --------------------
   --  4) MAIN TASK  --
   --------------------

   task RTEP_Task is
      pragma Priority (RTEP.RTEP_Task_Prio);
   end RTEP_Task;

   task body RTEP_Task is
      Source_Station_ID : Station_ID;
      Dest_Station_ID : Station_ID;
--      Timeout_Expired : Boolean := False;
      Token_Master_Station_ID : Station_ID;
      Token_Time_Taken : Boolean;
      Rx_Mult_Msg_Ac : Multicast_Msg_Ptr :=
         To_Multi_Msg (Rx_RTEP_Token_Packet.Multicast_Info'Access);
      Dist_Mutex_Ac : RTEP_Dist_Mutex_Ptr;
      M : Distributed_Mutex_Id;
   begin
   loop --  BIG LOOP
   case Station_State is -- BIG CASE
      when Offline =>
         DEBUG (" * Offline State ...", Enable_DEBUG_States'First);
         --  Initialize variables
         Tx_Buffer_Info_Ac  := To_Stream_Array_Ac
            (Tx_RTEP_Info_Packet'Unchecked_Access);
         Tx_Buffer_Token_Ac := To_Stream_Array_Ac
            (Tx_RTEP_Token_Packet'Unchecked_Access);
         --  Prepare timers --
         POSIX.Signals.Set_Signal (Timer_Signal_Event, RTEP_Timeout_Signal);
         POSIX.Signals.Set_Notification
            (Timer_Signal_Event, POSIX.Signals.Signal_Notification);
         Timeout_Timer_Id := POSIX_Timers.Create_Timer
            (POSIX_Timers.Clock_Realtime, Timer_Signal_Event);

         --  Arming / DisArming initialitation
         --  Initialization of states.
         --  For Enabled_Timeout_Time, first we initialize it to
         --  Initialitation Timeout.
         POSIX_Timers.Set_Initial (Timeout_Time, POSIX.To_Timespec
            (RTEP.RTEP_Comunication_Initialitation_Timeout));
         POSIX_Timers.Set_Interval (Timeout_Time, POSIX.To_Timespec (0.0));

         --  Next we have to wait till initialitation.
         Ada.Synchronous_Task_Control.Suspend_Until_True (RTEP_Init_Object);

         --  Opening the device in promiscuous mode
         System.Open_RTEP_Communication;
         System.Set_Promiscous_Mode;

         --  Some Station related Variables initialitation
         Me := Get_Station_ID;
         DEBUG ("    Current_Station: "&Integer'Image (
            Stations.Get_Station_Position (Me)),
            Enable_DEBUG_Init'First);

         if not Stations.Valid_Station_Id (me) then
            DEBUG ("STATION NOT IN RING Stat: "&Integer'Image (
                        Stations.Get_Station_Position (Me)),
                        Enable_DEBUG_Init'First);
            if MS.Post (Initialization_Sem'Access) /= 0 then
               raise Unexpected_Error;
            end if;
            exit;
         end if;

         Next := Stations.Get_Next_Station (Me);
         Is_Token_Master := Stations.Check_Token_Master (Me);


         DEBUG ("    Next Station: "&Integer'Image (
            Stations.Get_Station_Position (Next)),
            Enable_DEBUG_Init'First);

         --------------------------
         --  Ring Initialitation --
         --------------------------
         --  The initialitation process is carried out by the first Token Master
         --  which is the first station in the ring spec. This station sends
         --  request packets to the rest of stations until it gets an ACK from
         --  them. So, the rest of stations wait for this REQUEST.
         if Is_Token_Master then
            --  We are the first Token Master
            Tx_RTEP_Token_Packet.Packet_Id := REQ;
            Last_Sent_Station_ID := Next;
            loop
               DEBUG ("    TMaster Sends REQUEST to "
                      &Station_ID'Image(Last_Sent_Station_ID),
                      Enable_DEBUG_Init'First);
               System.Send_RTEP_Packet (Last_Sent_Station_ID,
                  Stream_Element_Array (Tx_Buffer_Token_Ac.all));
               loop
                  DEBUG ("    Waits ACK", Enable_DEBUG_Init'First);
                  --  Arm the Timer --
                  POSIX_Timers.Arm_Timer
                     (Timeout_Timer_Id,RTEP_Timer_Options,Timeout_Time);
                  --  Receive from network
                  System.Receive_RTEP_Packet
                     (Dest_Station_ID, Source_Station_ID,
                      Rx_Buffer, Rx_Buffer_Last);
                  --  DisArm the Timer --
                  POSIX_Timers.Disarm_Timer (Timeout_Timer_Id);
                  --  Convert the receiving packet.
                  Rx_RTEP_Token_Packet :=
                     To_Token_Packet
                     (RTEP_Token_Stream_Array
                     (Rx_Buffer (1 .. RTEP_Token_Stream_Array'Length)));
                  DEBUG ("    Received Something", Enable_DEBUG_Init'First);
                  exit when (Dest_Station_ID = Me) and
                            (Source_Station_ID = Last_Sent_Station_ID) and
                            (Rx_RTEP_Token_Packet.Packet_Id = ACK);
               end loop;
               DEBUG ("    Received ACK", Enable_DEBUG_Init'First);
               --  We get the next station and continue with the poling
               Last_Sent_Station_ID :=
                  Stations.Get_Next_Station (Last_Sent_Station_ID);
               exit when Last_Sent_Station_ID = Me;
            end loop;
            --  Signal the user that Initialization is done
            if MS.Post (Initialization_Sem'Access) /= 0 then
               raise Unexpected_Error;
            end if;
         else --  In case we aren't the Token Master.
            --  We have to wait for the request station frame.
            Token_Master_Station_ID :=
               Get_Station_ID_By_Position (Position (1));
            loop
               DEBUG ("    NO TMaster("&Station_ID'Image(Me)&
                      "): Wait REQUEST", Enable_DEBUG_Init'First);
               System.Receive_RTEP_Packet
                  (Dest_Station_ID, Source_Station_ID,
                   Rx_Buffer, Rx_Buffer_Last);
               DEBUG ("    NO TMaster: Received packet", Enable_DEBUG_Init'First);
               --  Convert the receiving packet.
               Rx_RTEP_Token_Packet :=
                  To_Token_Packet
                  (RTEP_Token_Stream_Array
                  (Rx_Buffer (1 .. RTEP_Token_Stream_Array'Length)));
               exit when (Rx_RTEP_Token_Packet.Packet_Id = REQ) and
                         (Token_Master_Station_ID = Source_Station_ID) and
                         (Dest_Station_ID = Me);
            end loop;
            DEBUG ("    REQUEST Received, Send ACK", Enable_DEBUG_Init'First);
            --  We set the packet_id as ACK
            Tx_RTEP_Token_Packet.Packet_Id := ACK;
            System.Send_RTEP_Packet (Token_Master_Station_ID,
               Stream_Element_Array (Tx_Buffer_Token_Ac.all));
         end if;
         ------------------------------
         --  End Ring Initialitation --
         ------------------------------
         --  The ring initialitation is over. We have to reasign the
         --  Timeout to the corresponding one.
         POSIX_Timers.Set_Initial (Timeout_Time, POSIX.To_Timespec
            (RTEP.RTEP_Communication_Timeout));

         --  Continue with the initialitation.
         if Is_Token_Master then
            Station_State := Send_Initial_Token;
         else
            Station_State := Idle;
         end if;

         --  Reset the token packet:
         Reset_RTEP_Token_Packet (Tx_RTEP_Token_Packet);
         DEBUG (" * End Offline State", Enable_DEBUG_States'First);
      when Idle =>
         DEBUG (" * Begin Idle State, Check_For_Error="&Boolean'Image
                (Check_For_Error), Enable_DEBUG_States'First);

         BEGIN_TIME_MEASURE (Idle_State_ID);

         if Check_For_Error then
            -- Arm the Timer --
            DEBUG ("    Arm the timer", Enable_DEBUG_Timers'First);
            POSIX_Timers.Arm_Timer (Timeout_Timer_Id,
                                    RTEP_Timer_Options,
                                    Timeout_Time);
            DEBUG ("    [ OK! ]", Enable_DEBUG_Timers'First);
         end if;
         -------------------------------------------------------
         --  We wait (blocking read) to receive a message
         --  This goto label is necessary in order to recover
         --  from duplicated packet situation without re-enabling
         --  the timeout, thus the timeout will be always the same
         <<Read_Blocking_Section>>
         --  Receive from network
         System.Receive_RTEP_Packet
            (Dest_Station_ID, Source_Station_ID, Rx_Buffer, Rx_Buffer_Last);

         DEBUG ("    IDLE from: "&
            Station_ID'Image (Source_Station_ID)&
            " (Dest was "&
            Station_ID'Image (Dest_Station_ID)&
            "), I am "&Station_ID'Image (Me)&
            ", Check="&Boolean'Image
                (Check_For_Error),  Enable_DEBUG_States'First);

         --  In promiscuous mode we have to see if it's for us
         if Dest_Station_ID /= Me then
            Station_State := Idle;
            if Check_For_Error then
               --  If the hub (or emulator!) send my packet back to me
               --  discard it without reenabling the timer
               if Source_Station_ID = Me then
                  DEBUG ("    SOURCE WAS ME, rereading",
                      Enable_DEBUG_States'First);
               else
                  -- This is an implicit ACK by our successor in the ring
                  DEBUG ("    Received IMPLICIT ACK from "&
                     Station_ID'Image (Source_Station_ID)&
                     " (Dest was "&
                     Station_ID'Image (Dest_Station_ID)&
                     "), I am "&Station_ID'Image (Me), Enable_DEBUG_States'First);
                  Error_Retry_Counter := 0;
                  Check_For_Error := False;
                  DEBUG ("    Disarm Timer", Enable_DEBUG_Timers'First);
                  POSIX_Timers.Disarm_Timer (Timeout_Timer_Id);
               end if;
            end if;
         else
            DEBUG ("    Received Packet FOR me", Enable_DEBUG_States'First);
            --  We try to get the sequence number. We suppose to have
            --  received a Token. Later we will convert to the proper
            --  packet.
            Rx_RTEP_Token_Packet :=
               To_Token_Packet
               (RTEP_Token_Stream_Array
               (Rx_Buffer (1 .. RTEP_Token_Stream_Array'Length)));

            if Rx_RTEP_Token_Packet.Packet_Number =  Last_Packet_Number then
               --  SAME seq number as the last received
               if Rx_RTEP_Token_Packet.Packet_Id = REQ then
                  DEBUG ("    ACK got lost, retransmit ACK",
                     Enable_DEBUG_Init'First);
                  Tx_RTEP_Token_Packet.Packet_Id := ACK;
                  System.Send_RTEP_Packet (Token_Master_Station_ID,
                     Stream_Element_Array (Tx_Buffer_Token_Ac.all));
               else
                  DEBUG ("    Same SEQ number, Packet Discarded",
                     Enable_DEBUG_Timeouts'First);
               end if;
               Station_State := Idle;
               --  This goto is needed to return to the read from net
               --  function without re-enabling the timer (in case
               --  check_for_error==TRUE) in case duplicated packet is
               --  received.
               goto Read_Blocking_Section;
            end if;
            --  else It is a proper packet.
            if not Initializated then
               --  this is the first packet we receive, signal the user
               if MS.Post (Initialization_Sem'Access) /= 0 then
                  raise Unexpected_Error;
               end if;
            end if;

            Last_Packet_Number := Rx_RTEP_Token_Packet.Packet_Number;
            if Check_For_Error then
               Error_Retry_Counter := 0;
               Check_For_Error := False;
               DEBUG ("    Disarm Timer", Enable_DEBUG_Timers'First);
               POSIX_Timers.Disarm_Timer (Timeout_Timer_Id);
            end if;
            --  Now we have received a message, but we don't know
            --  whether it is an info packet or a token packet. We
            --  can guess it by looking into the first byte of the
            --  received information
            if Rx_RTEP_Token_Packet.Packet_Id = TOK then
               --  It is a Token Packet
               case Rx_RTEP_Token_Packet.Mult_Type is
                  when No_Multicast =>
                     DEBUG ("Token: No Multicast",
                            Enable_DEBUG_Multicast'First or
                            Enable_DEBUG_Mutexes'First);
                  when Multicast_Message =>
                     --  Check if the message is for me
                     if (not Is_Token_Master) and
                        Stations.Check_Subscription
                           (Station_ID (Rx_Mult_Msg_Ac.Multicast_Addr),
                            Me) then
                        DEBUG ("Token: Multicast Message (For me)",
                               Enable_DEBUG_Multicast'First);
                        --  Fill Info packet fields using the multicast msg
                        Rx_RTEP_Info_Packet.Channel_Id :=
                           Rx_Mult_Msg_Ac.Channel_Id;
                        Rx_RTEP_Info_Packet.Prio := Rx_Mult_Msg_Ac.Prio;
                        Rx_RTEP_Info_Packet.Info_Length :=
                           Interfaces.Unsigned_16 (Rx_Mult_Msg_Ac.Info_Length);
                        Rx_RTEP_Info_Packet.Info (1 .. Stream_Element_Offset
                           (Rx_RTEP_Info_Packet.Info_Length)) :=
                           Rx_Mult_Msg_Ac.Info (1 .. Stream_Element_Offset
                                 (Rx_RTEP_Info_Packet.Info_Length));
                        Source_Station_ID :=
                           Station_ID (Rx_RTEP_Token_Packet.Token_Master_Id);
                        Handle_Received_Packet
                           (Rx_RTEP_Info_Packet, Source_Station_ID);
                     else
                        if Is_Token_Master then
                           -- Reset Values
                           Rx_RTEP_Token_Packet.Mult_Type := No_Multicast;
                        end if;
                        DEBUG ("Token: Multicast Message (NOT for me)",
                               Enable_DEBUG_Multicast'First);
                     end if;
                  when Lock_Dist_Mutex =>
                     DEBUG ("Token: Lock_Dist_Mutex",
                        Enable_DEBUG_Mutexes'First);
                     if Is_Token_Master then
                        -- As I am the Master, the full token rotation is done.
                        DEBUG ("    I am MASTER", Enable_DEBUG_Mutexes'First);
                        -- Reset Values
                        Rx_RTEP_Token_Packet.Mult_Type := No_Multicast;
                     else
                        DEBUG ("    NOT MASTER", Enable_DEBUG_Mutexes'First);
                        Dist_Mutex_Ac := To_Dist_Mutex
                           (Rx_RTEP_Token_Packet.Multicast_Info'Access);
                        M := Dist_Mutex_Ac.Mutex_Id;
                        DEBUG ("    Mutex_Id: "&Distributed_Mutex_Id'Image (M),
                              Enable_DEBUG_Mutexes'First);
                        --  Update the local mutex
                        The_Dist_Mutexes (M).Locked := True;
                        The_Dist_Mutexes (M).Holder := Station_ID
                           (Rx_RTEP_Token_Packet.Token_Master_Id);
                        --  TODO: SIGNAL THIS TO APPLICATION!
                     end if;
                  when Unlock_Dist_Mutex =>
                     DEBUG ("Token: Unlock_Dist_Mutex",
                        Enable_DEBUG_Mutexes'First);
                     if Is_Token_Master then
                        DEBUG ("    I am MASTER", Enable_DEBUG_Mutexes'First);
                        -- As I am the Master, the full token rotation is done.
                        Rx_RTEP_Token_Packet.Mult_Type := No_Multicast;
                     else
                        DEBUG ("    NOT MASTER", Enable_DEBUG_Mutexes'First);
                        Dist_Mutex_Ac := To_Dist_Mutex
                           (Rx_RTEP_Token_Packet.Multicast_Info'Access);
                        M := Dist_Mutex_Ac.Mutex_Id;
                        DEBUG ("    Mutex_Id: "&Distributed_Mutex_Id'Image (M),
                              Enable_DEBUG_Mutexes'First);
                        --  Update the local mutex
                        The_Dist_Mutexes (M).Locked := False;
                     end if;
                  when others =>
                     DEBUG ("Token: WRONG Multicast_Type",
                            Enable_DEBUG_Multicast'First or
                            Enable_DEBUG_Mutexes'First);
               end case;
               Station_State := Check_Token;
               --  We have to check if the token has information
               --  about a failure station or if it has done a
               --  round informing about a invalid station.
               if Check_For_Valid_Station then
                  Check_For_Valid_Station := False;
                  Rx_RTEP_Token_Packet.Failure_Station := 0;
               end if; --  Check_For_Valid_Station.
               if Rx_RTEP_Token_Packet.Failure_Station /=
                  Interfaces.Unsigned_8 (0) then
                  if Get_Station_ID_By_Position (Integer (
                     Rx_RTEP_Token_Packet.Failure_Station_Id)) = Next then
                     Stations.Inhibit_Station_Id(Next);
                     Next := Stations.Get_Next_Station(Next);
                  else
                     Stations.Inhibit_Station_Id
                        (Get_Station_ID_By_Position
                        (Integer (Rx_RTEP_Token_Packet.Failure_Station_Id)));
                  end if;
                  --  We check if the failure station had a distributed mutex
                  Free_Failure_Station_Mutex (Last_Sent_Station_ID);
               end if; --  Failure_Station.
            elsif Rx_RTEP_Token_Packet.Packet_Id = PER then
                  --  It is a Send Permission Token
               DEBUG ("Token: Received Permission", Enable_DEBUG_States'First);
               Station_State := Send_Info;
            elsif Rx_RTEP_Token_Packet.Packet_Id = INF then
               --  It is not a token but an Info Packet
               --  Convert the Rx_Buffer to an Info Packet.
               Rx_RTEP_Info_Packet :=
                  To_Info_Packet
                  (RTEP_Info_Stream_Array (Rx_Buffer
                                             (1 .. Rx_Buffer'Last)));
               Station_State := Recv_Info;
            end if;
         end if; --  Dest_Station_ID = Me

         END_TIME_MEASURE (Idle_State_ID);
         DEBUG (" * End Idle State", Enable_DEBUG_States'First);

      when Send_Initial_Token =>
         DEBUG (" * Begin Send_Initial_Token State", Enable_DEBUG_States'First);

         BEGIN_TIME_MEASURE (Send_Initial_Token_State_ID);

         Is_Token_Master := True;
         --  We build a new token
         Create_Initial_Token (Tx_RTEP_Token_Packet);

         --  TIME_MEASURE : Begin measuring TOKEN_PASSING.
         BEGIN_TIME_MEASURE (Token_Passing_Time_ID);
         Token_Time_Taken := True;

         --  We sleep now in order to decrease CPU overhead
         if RTEP.Enable_RTEP_Delay'Last then
            delay RTEP.RTEP_Delay;
         end if;

         DEBUG ("    Sending, Dest="&
            Station_ID'Image (Next)&
            " Me="&Station_ID'Image (Me), Enable_DEBUG_States'First);

         System.Send_RTEP_Packet (Next,
            Stream_Element_Array (Tx_Buffer_Token_Ac.all));

         Check_For_Error := True;
         Last_Sent_Station_ID := Next;
         Last_Station_State := Send_Initial_Token;
         Station_State := Idle;

         END_TIME_MEASURE (Send_Initial_Token_State_ID);
         DEBUG (" * End Send_Initial_Token State", Enable_DEBUG_States'First);

      when Check_Token =>
         DEBUG (" * Check_Token State", Enable_DEBUG_States'First);
         BEGIN_TIME_MEASURE (Check_Token_State_ID);

         --  Comparing the received token information with the
         --  information in our queue.
         Tx_RTEP_Token_Packet := Rx_RTEP_Token_Packet;
         Check_Token_Prio (Tx_RTEP_Token_Packet);

         --  TIME_MEASURE : Finish measuring TOKEN_PASSING.
         if Is_Token_Master and Token_Time_Taken then
            END_TIME_MEASURE (Token_Passing_Time_ID);
            Token_Time_Taken := False;
         end if;

         if Is_Token_Master = False or Tx_RTEP_Token_Packet.Prio = 0 then
            Station_State := Send_Token;
         end if;

         if Is_Token_Master and Tx_RTEP_Token_Packet.Prio /= 0 then
            --  Since we are transmiting we set token_master to FALSE
            Is_Token_Master := False;
            if Tx_RTEP_Token_Packet.Station_Prio_Id = Me then
               Station_State := Send_Info;
            else
               Station_State := Send_Permission;
            end if;
         end if;

         END_TIME_MEASURE (Check_Token_State_ID);
         DEBUG (" * End Check_Token State", Enable_DEBUG_States'First);

      when Recv_Info =>
         DEBUG (" * Recv_Info State", Enable_DEBUG_States'First);
         BEGIN_TIME_MEASURE (Recv_Info_State_ID);

         Handle_Received_Packet (Rx_RTEP_Info_Packet, Source_Station_ID);
         Tx_RTEP_Token_Packet.Mult_Type := No_Multicast;
         Station_State := Send_Initial_Token;

         END_TIME_MEASURE (Recv_Info_State_ID);
         DEBUG (" * End Recv_Info State", Enable_DEBUG_States'First);

      when Send_Info =>
         DEBUG (" * Send_Info State", Enable_DEBUG_States'First);
         BEGIN_TIME_MEASURE (Send_Info_State_ID);

         Prepare_Msg_To_Send (Dest_Station_ID);
         if Stations.Valid_Multicast_Id (Dest_Station_ID) then
            --  Send it through the Token packet
            Station_State := Send_Initial_Token;
         else
            --  Send it through the Info packet
            Tx_Buffer_Info_Last := RTEP_Info_Packet_Header +
               Ada.Streams.Stream_Element_Offset
               (Tx_RTEP_Info_Packet.Info_Length);

            System.Send_RTEP_Packet (Dest_Station_ID, Stream_Element_Array
               (Tx_Buffer_Info_Ac (1 .. Tx_Buffer_Info_Last)));

            Last_Sent_Station_ID := Dest_Station_ID;
            Last_Station_State := Send_Info;
            Station_State := Idle;
            --  Tell the station to check the media for errors
            Check_For_Error := True;
         end if;

         END_TIME_MEASURE (Send_Info_State_ID);
         DEBUG (" * End Send_Info State", Enable_DEBUG_States'First);

      when Send_Token =>
         --  If we aren't in the management station and we are
         --  not allow to tx info we have to give the token to
         --  our successor.
         DEBUG (" * Send_Token State", Enable_DEBUG_States'First);
         BEGIN_TIME_MEASURE (Send_Token_State_ID);

         --  We sleep now in order to decrease CPU overhead
         if RTEP.Enable_RTEP_Delay'Last then
            delay RTEP.RTEP_Delay;
         end if;

         DEBUG ("    Sending token, Dest="&
            Station_ID'Image (Next)&
            " Me="&Station_ID'Image (Me), Enable_DEBUG_States'First);

         --  And tx to the successor.
         System.Send_RTEP_Packet
            (Next, Stream_Element_Array (Tx_Buffer_Token_Ac.all));

         --  We update the station that we have has last sent a packet.
         Last_Sent_Station_ID := Next;
         Station_State := Idle;
         Last_Station_State := Send_Token;
         --  Tell the station to check the media for errors
         Check_For_Error := True;

         END_TIME_MEASURE (Send_Token_State_ID);
         DEBUG (" * End Send_Token State", Enable_DEBUG_States'First);

      when Send_Permission =>
         DEBUG (" * Send_Permission State", Enable_DEBUG_States'First);
         BEGIN_TIME_MEASURE (Send_Permission_State_ID);

         Tx_RTEP_Token_Packet.Packet_Id := PER;
         Dest_Station_ID := Get_Station_ID_By_Position
            (Position (Tx_RTEP_Token_Packet.Station_Prio_Id));

         System.Send_RTEP_Packet
            (Dest_Station_ID, Stream_Element_Array (Tx_Buffer_Token_Ac.all));
         --  We update the station that we have has last sent a packet.
         Last_Sent_Station_ID := Dest_Station_ID;
         Station_State := Idle;
         Last_Station_State := Send_Permission;
         --  Tell the station to check the media for errors
         Check_For_Error := True;

         END_TIME_MEASURE (Send_Permission_State_ID);
         DEBUG (" * End Send_Permission State", Enable_DEBUG_States'First);

   end case; --  End BIG CASE
   end loop; --  End BIG LOOP
   exception
      when System.Network_Error =>
         --  MIRAR: Network_Error
         ERROR (" RTEP_Task : Network_Error");
      when Station_Not_Found =>
         --  MIRAR: Station_Not_Found
         ERROR ("Station_Not_Found");
      when E : others =>
         ERROR ("RTEP_Task: ");
         ERROR (Ada.Exceptions.Exception_Name (E));
         ERROR (Ada.Exceptions.Exception_Message (E));
   end RTEP_Task;

   ---------------
   -- Init_Comm --
   ---------------

   procedure Init_Comm  is
   begin
      if Initializated = False then
         --  Initialize time measures (only has effect if they are enabled)
         RTEP_Debug_Functions.Time_Measures_init;
         --  Initialize Buffers
         RTEP.Protocol.Buffers.Buffers_Init;
         --  Initialize the Packets pool
         RTEP.Protocol.Packet.Monitor_Init_RTEP_Packet_Pool;
         --  Initialize Servers
         RTEP.Protocol.Servers.Servers_Init;
         --  Initize the semaphore for telling the user init is done
         if MS.Initialize (Initialization_Sem'Access, 0, 0) /= 0 then
            raise Unexpected_Error;
         end if;
         --  Awake the RTEP main task
         Ada.Synchronous_Task_Control.Set_True (RTEP_Init_Object);
         --  Wait until the initialitation is done
         if MS.Wait (Initialization_Sem'Access) /= 0 then
            raise Unexpected_Error;
         end if;
         Initializated := True;
         if MS.Destroy (Initialization_Sem'Access) /= 0 then
            raise Unexpected_Error;
         end if;
      end if;
   exception
      when Station_Not_Found =>
         raise Initialitation_Error;
   end Init_Comm;

   -------------------------
   -- Is_RTEP_Initialized --
   -------------------------

   function Is_RTEP_Initialized return Boolean is
   begin
      return Initializated;
   end Is_RTEP_Initialized;

end RTEP.Protocol.Core;
