--  ------------------------------------------------------------------------
--  ---------------------           RT-EP           ------------------------
--  ------------------------------------------------------------------------
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
--                       'r t e p _ p r o t o c o l . a d b'
--
--                                     Ada
--
--
--  File 'RTEP_protocol.adb'                                       By Chema.
--                                                       Jose Maria Martinez
--                                                           <chema@gmx.net>
--  Import declarations and translations from rt_comm.h
--
--
--
--
--  -------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Characters.Handling;
with Ada.Real_Time;
with Unchecked_Deallocation;
with Ada.Exceptions;

with RTEP.Protocol.Packet;
with RTEP.Protocol.Buffers;
with RTEP.Protocol.Stations;
with RTEP.Protocol.System;
with RTEP.Protocol.Servers;
with RTEP_Debug_Functions; use RTEP_Debug_Functions;

package body RTEP.Protocol  is

   use type Packet.RTEP_Packet_Ac;
   use type MaRTE.Integer_Types.Int; -- Mario

   -------------------------------------
   --  INDEX
   --   1) GET_STATION functions
   --   2) SEND functions
   --   3) RECEIVE functions
   --   4) DISTRIBUTED MUTEX functions
   -------------------------------------

   --------------------------------
   --  1)  GET_STATION functions --
   --------------------------------

   --------------------
   -- Get_Station_ID --
   --------------------

   function Get_Station_ID return Station_ID is
      Current_Station : constant String := System.Get_Current_Station_Address;
      Sta_Id : Station_ID;
   begin
      for I in 1 .. RTEP.Number_Of_Stations loop
         Sta_Id := Get_Station_ID_By_Position (I);
         if Ada.Characters.Handling.To_Upper
           (Stations.Get_Station_Address (Sta_Id)) =
           Ada.Characters.Handling.To_Upper (Current_Station) then
            return Sta_Id;
         end if;
      end loop;
      raise Station_Not_Found;
   end Get_Station_ID;

   ----------------------------
   -- Get_Station_ID_By_Name --
   ----------------------------

   function Get_Station_ID_By_Name (Station_Name : in String)
                                  return Station_ID is
      Sta_Id : Station_ID;
   begin
      for I in 1 .. RTEP.Number_Of_Stations +
                    RTEP.Number_Of_Multicast_Addresses loop
         Sta_Id := Get_Station_ID_By_Position (I);
         if Ada.Characters.Handling.To_Upper
           (Stations.Get_Station_Name (Sta_Id)) =
           Ada.Characters.Handling.To_Upper (Station_Name) then
            return Sta_Id;
         end if;
      end loop;
      raise Station_Not_Found;
   end Get_Station_ID_By_Name;

   --------------------------------
   -- Get_Station_ID_By_Position --
   --------------------------------

   function Get_Station_ID_By_Position (Pos : in Position) return Station_ID is
   begin
      return Station_ID (Pos);
   end Get_Station_ID_By_Position;

   -------------------------
   --  2)  SEND functions --
   -------------------------

   -------------------------------
   -- Send_Info (with Priority) --
   -------------------------------

   procedure Send_Info (Destination_Station_ID : in Station_ID;
                        Channel_ID : in Channel;
                        Data : in Stream_Element_Array;
                        Data_Priority : in Priority)
   is
      Packet_Size : constant Stream_Element_Count := Data'Length;
      Packet_Ac : Packet.RTEP_Packet_Ac;
   begin
      if Stations.Valid_Station_Id (Destination_Station_ID) then
         if Packet_Size > RTEP.Max_RTEP_MTU then
            raise Info_Length_Overflow;
         end if;
         Packet_Ac := Packet.Get_New_RTEP_Packet;
         Packet.Put_Multicast_Type
            (Packet_Ac.all, RTEP.Protocol.No_Multicast);
      elsif Stations.Valid_Multicast_Id (Destination_Station_ID) then
         if Packet_Size > RTEP.Multicast_MTU then
            raise Info_Length_Overflow;
         end if;
         Packet_Ac := Packet.Get_New_RTEP_Packet;
         Packet.Put_Multicast_Type
            (Packet_Ac.all, RTEP.Protocol.Multicast_Message);
      else
         raise Station_Not_Valid;
      end if;
      Packet.Put_Station_Id (Packet_Ac.all, Destination_Station_ID);
      Packet.Put_Channel_Id (Packet_Ac.all, Channel_ID);
      Packet.Copy_Stream_Array_In_RTEP_Packet (Data, Packet_Ac.all);
      Packet.Set_Synchronyzed (Packet_Ac.all, False);
      Buffers.FP_Tx_Queue_Insert_Packet (Packet_Ac, Data_Priority);
   exception
--       when Storage_Error | Packet.No_More_Free_Packets |
--         Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
--         Packet.RTEP_Packet_Not_Allocated | Buffers.Full_Of_Packets |
--         Buffers.Empty => raise Unexpected_Error;
      when The_Error : others =>
         ERROR ("Unknown error:");
         ERROR (Ada.Exceptions.Exception_Name(The_Error));
         ERROR (Ada.Exceptions.Exception_Message(The_Error));
         raise Unexpected_Error;
   end Send_Info;

   --------------------------------
   -- Send_Info (through Server) --
   --------------------------------

   procedure Send_Info
     (Destination_Station_ID : in Station_ID;
      Channel_ID : in Channel;
      Data : in Stream_Element_Array;
      Id : in Server_ID;
      Blocking : in Boolean := False)
   is
      Packet_Size : constant Stream_Element_Count := Data'Length;
      Packet_Ac : Packet.RTEP_Packet_Ac;
      Timestamp : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      if Stations.Valid_Station_Id (Destination_Station_ID) then
         if Packet_Size > RTEP.Max_RTEP_MTU then
            raise Info_Length_Overflow;
         end if;
         Packet_Ac := Packet.Get_New_RTEP_Packet;
         Packet.Put_Multicast_Type
            (Packet_Ac.all, RTEP.Protocol.No_Multicast);
      elsif Stations.Valid_Multicast_Id (Destination_Station_ID) then
         if Packet_Size > RTEP.Multicast_MTU then
            raise Info_Length_Overflow;
         end if;
         Packet_Ac := Packet.Get_New_RTEP_Packet;
         Packet.Put_Multicast_Type
            (Packet_Ac.all, RTEP.Protocol.Multicast_Message);
      else
         raise Station_Not_Valid;
      end if;
      Packet.Put_Station_Id (Packet_Ac.all, Destination_Station_ID);
      Packet.Put_Channel_Id (Packet_Ac.all, Channel_ID);
      Packet.Put_Timestamp (Packet_Ac.all, Timestamp);
      Packet.Copy_Stream_Array_In_RTEP_Packet (Data, Packet_Ac.all);
      if Blocking then
         Packet.Set_Synchronyzed (Packet_Ac.all, True);
         --  Initialize Persistent Signals of the server (if they are not)
         PSS.Init (Can_I_Send (Id), True);
         PSS.Init (Is_Result_Available (Id), False);
         --  Only one thread allowed to send synchronized at the same time
         Check_NZ (PSS.Wait (Can_I_Send (Id)));
         Buffers.Server_Tx_Queues_Insert_Packet (Packet_Ac, Id);
         --  Wait to be Signaled by the Main Task
         Check_NZ (PSS.Wait (Is_Result_Available (Id)));
         Check_NZ (PSS.Signal (Can_I_Send (Id)));
      else
         Packet.Set_Synchronyzed (Packet_Ac.all, False);
         Buffers.Server_Tx_Queues_Insert_Packet (Packet_Ac, Id);
      end if;
   exception
--       when Storage_Error | Packet.No_More_Free_Packets |
--         Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
--         Packet.RTEP_Packet_Not_Allocated
--         | Buffers.Full_Of_Packets | Buffers.Empty =>
--          raise Unexpected_Error;
   when The_Error : others =>
      ERROR ("Unknown error:");
      ERROR (Ada.Exceptions.Exception_Name(The_Error));
      ERROR (Ada.Exceptions.Exception_Message(The_Error));
      raise Unexpected_Error;
   end Send_Info;

   -----------------------
   -- Generic Send Info --
   -----------------------
   --
   --    procedure Generic_Send_Info (Destination_Station_ID : in Station_ID;
   --                                 Channel_ID : in Channel;
   --                                 Data : in Data_Type;
   --                                 Data_Priority : in Priority) is
   --
   --       Packet_Ac : Packet.RTEP_Packet_Ac;
   --       Size : constant Integer := Integer ((Data'Size + 7) / 8);
   --    begin
   --
   --       if Stations.Valid_Station_Id (Destination_Station_ID) = False then
   --          raise Station_Not_Valid;
   --       end if;
   --       if Channel_ID > RTEP.Number_Of_Channels
   --         or Channel_ID = 0 then
   --          raise Invalid_Channel;
   --       end if;
   --
   --       if Size > RTEP.Max_RTEP_MTU then
   --          raise Info_Length_Overflow;
   --       end if;
   --
   --       Packet_Ac := Packet.Get_New_RTEP_Packet;
   --
   --       Packet.Put_Station_Id (Packet_Ac.all, Destination_Station_ID);
   --       Packet.Put_Channel_Id (Packet_Ac.all, Channel_ID);
   --       Data_Type'Write (Packet_Ac, Data);
   --       Channels.Channel_Insert (Tx_Channel, Packet_Ac, Data_Priority);
   --
   --
   --    exception
   --       when Storage_Error | Packet.No_More_Free_Packets |
   --         Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
   --         Packet.RTEP_Packet_Not_Allocated
   --         | Channels.Full_Of_Packets |  Channels.Empty =>
   --          raise Unexpected_Error;
   --
   --    end Generic_Send_Info;


   ----------------------------
   --  3)  RECEIVE functions --
   ----------------------------

   --------------------------
   --  Recv_Info (Streams) --
   --------------------------

   procedure Recv_Info
      (Source_Station_ID : out Station_ID;
       Channel_ID        : in Channel;
       Data              : out Stream_Element_Array;
       Last              : out Stream_Element_Offset;
       Data_Priority     : out Priority)
   is
      Packet_Ac : Packet.RTEP_Packet_Ac;
   begin
      Buffers.Rx_Queues_Extract_Packet
         (Channel_ID, Packet_Ac, Data_Priority);
      Source_Station_ID := Packet.Get_Station_Id (Packet_Ac.all);
      Packet.Read_RTEP_Packet (Packet_Ac.all, Data, Last);
      Packet.Destroy_RTEP_Packet (Packet_Ac);
   exception
      when Storage_Error | Packet.No_More_Free_Packets |
        Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
        Packet.RTEP_Packet_Not_Allocated
        | Buffers.Full_Of_Packets | Buffers.Empty =>
         raise Unexpected_Error;
   end Recv_Info;

   -----------------------------
   -- Try_Recv_Info (Streams) --
   -----------------------------

   procedure Try_Recv_Info
     (Source_Station_ID : out Station_ID;
      Channel_ID        : in Channel;
      Data              : out Stream_Element_Array;
      Last              : out Stream_Element_Offset;
      Data_Priority     : out Priority;
      Received          : out Boolean)
   is
      Packet_Ac : Packet.RTEP_Packet_Ac;
   begin
      --  We extract the packets in a non blocking day.
      Buffers.Rx_Queues_Extract_Packet
         (Channel_ID, Packet_Ac, Data_Priority, False);
      if Packet_Ac = null then
         Received := False;
         return;
      else
         Received := True;
      end if;
      Source_Station_ID := Packet.Get_Station_Id (Packet_Ac.all);
      Packet.Read_RTEP_Packet (Packet_Ac.all, Data, Last);
      Packet.Destroy_RTEP_Packet (Packet_Ac);
   exception
      when Storage_Error | Packet.No_More_Free_Packets |
        Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
        Packet.RTEP_Packet_Not_Allocated
        | Buffers.Full_Of_Packets |  Buffers.Empty =>
         raise Unexpected_Error;
   end Try_Recv_Info;

   --------------------------
   --  Recv_Info (Generic) --
   --------------------------

   procedure Generic_Recv_Info
     (Source_Station_ID : out Station_ID;
      Channel_ID : in Channel;
      Data : out Data_Type;
      Data_Priority : out Priority)
   is
      Packet_Ac : Packet.RTEP_Packet_Ac;
   begin
      Buffers.Rx_Queues_Extract_Packet
         (Channel_ID,Packet_Ac,Data_Priority);
      Data_Type'Read (Packet_Ac, Data);
      Source_Station_ID := Packet.Get_Station_Id (Packet_Ac.all);
      Packet.Destroy_RTEP_Packet (Packet_Ac);
   exception
      when Storage_Error | Packet.No_More_Free_Packets |
        Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
        Packet.RTEP_Packet_Not_Allocated
        | Buffers.Full_Of_Packets |  Buffers.Empty =>
         raise Unexpected_Error;
   end Generic_Recv_Info;

   ------------------------------
   --  Try_Recv_Info (Generic) --
   ------------------------------

   procedure Generic_Try_Recv_Info
      (Source_Station_ID : out Station_ID;
       Channel_ID        : in Channel;
       Data              : out Data_Type;
       Data_Priority     : out Priority;
       Received          : out Boolean)
   is
      Packet_Ac : Packet.RTEP_Packet_Ac;
   begin
      --  We extract the packets in a non blocking day.
      Buffers.Rx_Queues_Extract_Packet
         (Channel_ID, Packet_Ac, Data_Priority, False);
      if Packet_Ac = null then
         Received := False;
         return;
      else
         Received := True;
      end if;
      Data_Type'Read (Packet_Ac, Data);
      Source_Station_ID := Packet.Get_Station_Id (Packet_Ac.all);
      Packet.Destroy_RTEP_Packet (Packet_Ac);
   exception
      when Storage_Error | Packet.No_More_Free_Packets |
         Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
         Packet.RTEP_Packet_Not_Allocated
         | Buffers.Full_Of_Packets |  Buffers.Empty =>
         raise Unexpected_Error;
   end Generic_Try_Recv_Info;

   --------------
   -- Any_Info --
   --------------

   function Any_Info (Channel_ID : in Channel) return Boolean is
      Packet_Ac : Packet.RTEP_Packet_Ac;
      Prio : Priority;
   begin
      Buffers.Rx_Queues_Read_Packet (Channel_ID, Packet_Ac, Prio);
      if Packet."="(Packet_Ac, null) then
         return False;
      else
         return True;
      end if;
   exception
      when Storage_Error | Packet.No_More_Free_Packets |
        Packet.RTEP_Packet_Full | Packet.End_Of_RTEP_Packet |
        Packet.RTEP_Packet_Not_Allocated
        | Buffers.Full_Of_Packets |  Buffers.Empty =>
         raise Unexpected_Error;
   end Any_Info;

   -------------------------------------
   --  4) DISTRIBUTED MUTEX functions --
   -------------------------------------

   ----------------------------
   -- Init_Distributed_Mutex --
   ----------------------------

   procedure Init_Distributed_Mutex
      (M    : in Distributed_Mutex_Id) is
   begin
      The_Dist_Mutexes (M).Locked := False;
      The_Dist_Mutexes (M).Action := None;
      The_Dist_Mutexes (M).Group := Get_Station_ID_By_Name ("broadcast");
      --  Create Semaphore
      if MS.Initialize (The_Dist_Mutexes (M).Sem'Access, 0, 0) /= 0 then
         raise Unexpected_Error;
      end if;
   end Init_Distributed_Mutex;

   ----------------------------
   -- Lock_Distributed_Mutex --
   ----------------------------

   procedure Lock_Distributed_Mutex
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority) is
   begin
      if (The_Dist_Mutexes (M).Locked) and then
         (The_Dist_Mutexes (M).Holder = Get_Station_ID) then
         ERROR ("The Mutex is already locked");
         raise Unexpected_Error;
      end if;
      --  Set the action
      The_Dist_Mutexes (M).Prio := Prio;
      The_Dist_Mutexes (M).Action := Lock;
      --  Wait to be done
      if MS.Wait (The_Dist_Mutexes (M).Sem'Access) /= 0 then
         raise Unexpected_Error;
      end if;
   end Lock_Distributed_Mutex;

   ------------------------------
   -- Unlock_Distributed_Mutex --
   ------------------------------

   procedure Unlock_Distributed_Mutex
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority) is
   begin
      if (not The_Dist_Mutexes (M).Locked) then
         ERROR ("Trying to unlock a mutex that is not locked");
         raise Unexpected_Error;
      end if;

      if (The_Dist_Mutexes (M).Holder /= Get_Station_ID) then
         ERROR ("Trying to unlock a mutex that is not mine");
         raise Unexpected_Error;
      end if;

      --  Set the action
      The_Dist_Mutexes (M).Prio := Prio;
      The_Dist_Mutexes (M).Action := Unlock;
      --  Wait to be done
      if MS.Wait (The_Dist_Mutexes (M).Sem'Access) /= 0 then
         raise Unexpected_Error;
      end if;
   end Unlock_Distributed_Mutex;

   --------------------------------
   -- Finalize_Distributed_Mutex --
   --------------------------------

   procedure Finalize_Distributed_Mutex
      (M : in Distributed_Mutex_Id) is
   begin
      if MS.Destroy (The_Dist_Mutexes (M).Sem'Access) /= 0 then
         raise Unexpected_Error;
      end if;
   end Finalize_Distributed_Mutex;

end RTEP.Protocol;
