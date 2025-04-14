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
--               'r t e p _ p r o t o c o l - p a c k e t . a d b'
--
--                                     Ada
--
--
--  File 'rtep-protocol-packet.adb'                                By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--  RT_EP Packet Internal Storage element.
--
--
--
--
-----------------------------------------------------------------------------

with RTEP_Streams;
with MaRTE.Kernel.Mutexes; pragma Elaborate_All (MaRTE.Kernel.Mutexes);
with MaRTE.Integer_Types; use MaRTE.Integer_Types; -- for Int
with RTEP_Debug_Functions; use RTEP_Debug_Functions;

package body RTEP.Protocol.Packet is

   package Km renames MaRTE.Kernel.Mutexes;

   type Index is mod RTEP.Max_RTEP_Packet_At_A_Time;

   type RTEP_Packet_Pool is array (Index) of RTEP_Packet;
   type RTEP_Packet_Pool_Ac is array (Index) of RTEP_Packet_Ac;

   use type MaRTE.Integer_Types.Int;

   -------------------------
   -- Monitor_Packet_Poll --
   -------------------------
   --  This was a protected object and now it is a monitor using a mutex
   Packet_Pool    : RTEP_Packet_Pool;
   Packet_Pool_Ac : RTEP_Packet_Pool_Ac;
   Current_Free_Packet : Index := Index'Last;
   Packet_Mutex_Ref : Km.Mutex_Descriptor;

   procedure Monitor_Init_RTEP_Packet_Pool is
      Attr      : aliased Km.Attributes;
   begin
      for I in Index loop
         RTEP_Packet_Ref (Packet_Pool (I), Packet_Pool_Ac (I));
      end loop;
      -- Mutex initialization
      Packet_Mutex_Ref := new Km.Mutex;
      Check_NZ (Km.Pthread_Mutexattr_Init (Attr'access));
      Check_NZ (Km.Pthread_Mutexattr_Setprotocol
         (Attr'access, Km.Highest_Ceiling_Priority));
      Check_NZ (Km.Pthread_Mutexattr_Setprioceiling
         (Attr'access, RTEP.RTEP_Task_Prio));
      Check_NZ (Km.Pthread_Mutex_Init (Packet_Mutex_Ref, Attr'access));
      Check_NZ (Km.Pthread_Mutexattr_Destroy (Attr'access));
   end Monitor_Init_RTEP_Packet_Pool;

   procedure Monitor_Get_New_RTEP_Packet
      (New_Packet_Ac : out RTEP_Packet_Ac) is
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Packet_Mutex_Ref));
         New_Packet_Ac := Packet_Pool_Ac (Current_Free_Packet);
         if New_Packet_Ac = null then
            Check_NZ (Km.Pthread_Mutex_Unlock (Packet_Mutex_Ref));
            raise No_More_Free_Packets;
         end if;
         --  First we need to 'reset' the data inside the paket.
         RTEP_Streams.Reset_Read_Counter
            (RTEP_Streams.RTEP_Streams_Type (New_Packet_Ac.all));
         RTEP_Streams.Reset_Write_Counter
            (RTEP_Streams.RTEP_Streams_Type (New_Packet_Ac.all));
         --  Next we continue with the allocating.
         New_Packet_Ac.Packet_In_Use := True;
         Packet_Pool_Ac (Current_Free_Packet) := null;
         Current_Free_Packet := Current_Free_Packet - 1;
      Check_NZ (Km.Pthread_Mutex_Unlock (Packet_Mutex_Ref));
      return;
   end Monitor_Get_New_RTEP_Packet;

   procedure Monitor_Destroy_RTEP_Packet
      (Packet_Ac : in out RTEP_Packet_Ac) is
   begin
      Check_NZ (Km.Pthread_Mutex_Lock (Packet_Mutex_Ref));
         if (Packet_Ac = null)
            or else (Packet_Ac.Packet_In_Use = False) then  -- Mario
            --  or (Packet_Ac.Packet_In_Use = False) then
            --  In case we were destroying a destroyed Packet we do nothing.
            Check_NZ (Km.Pthread_Mutex_Unlock (Packet_Mutex_Ref));
            return;
         end if;
         Packet_Ac.Packet_In_Use := False;
         Current_Free_Packet := Current_Free_Packet + 1;
         if Packet_Pool_Ac (Current_Free_Packet) /= null then
            Check_NZ (Km.Pthread_Mutex_Unlock (Packet_Mutex_Ref));
            raise Storage_Error;
         end if;
         Packet_Pool_Ac (Current_Free_Packet) := Packet_Ac;
         Packet_Ac := null;
      Check_NZ (Km.Pthread_Mutex_Unlock (Packet_Mutex_Ref));
      return;
   end Monitor_Destroy_RTEP_Packet;

   function RTEP_Packet_In_Use (Packet : in RTEP_Packet)
                                 return Boolean is
   begin
      return Packet.Packet_In_Use;
   end RTEP_Packet_In_Use;

   --------------------------
   -- Get_New_RTEP_Packet --
   --------------------------
   function Get_New_RTEP_Packet return RTEP_Packet_Ac is
      New_Packet_Ac : RTEP_Packet_Ac;
   begin
      Monitor_Get_New_RTEP_Packet (New_Packet_Ac);
      return New_Packet_Ac;
   end Get_New_RTEP_Packet;

   --------------------------
   -- Destroy_RTEP_Packet --
   --------------------------
   procedure Destroy_RTEP_Packet (Packet_Ac : in out RTEP_Packet_Ac) is

   begin
      Monitor_Destroy_RTEP_Packet (Packet_Ac);
   end Destroy_RTEP_Packet;

   -----------------------
   --  Is_Packet_In_Use --
   -----------------------
   --  This function will return true if Packet is being
   --  used. This function is usefull to determine if there are memory
   --  allocated for the packet or not.
   function Is_Packet_In_Use
     (Packet : in RTEP_Packet) return Boolean is
   begin
      return RTEP_Packet_In_Use (Packet);
   end Is_Packet_In_Use;

   -----------------------
   --  RTEP_Packet_Ref --
   -----------------------
   --  RTEP_Packet_Ref will create an access to an RTEP_Packet.
   procedure RTEP_Packet_Ref (Packet : in out RTEP_Packet;
                               Packet_Ac : out RTEP_Packet_Ac) is
   begin
      Packet_Ac := Packet'Unchecked_Access;
   end RTEP_Packet_Ref;

   --------------------
   -- Put_Station_Id --
   --------------------
   procedure Put_Station_Id
     (Packet : in out RTEP_Packet;
       Station_Identifier : in Station_ID) is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;

      Packet.Station_Identifier := Station_Identifier;
   end Put_Station_Id;

   --------------------
   -- Get_Station_Id --
   --------------------
   function Get_Station_Id
     (Packet : in RTEP_Packet)
     return Station_ID is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      return Packet.Station_Identifier;
   end Get_Station_Id;

   --------------------
   -- Put_Channel_Id --
   --------------------
   procedure Put_Channel_Id
     (Packet : in out RTEP_Packet;
       Channel_Id : in Channel) is

   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      Packet.Channel_Id := Channel_Id;
   end Put_Channel_Id;

   --------------------
   -- Get_Channel_Id --
   --------------------
   function Get_Channel_Id
     (Packet : in RTEP_Packet)
     return Channel is

   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      return Packet.Channel_Id;
   end Get_Channel_Id;

   --------------------
   --  Put_Timestamp --
   --------------------
   --  Set the packet timestamp.
   procedure Put_Timestamp
     (Packet : in out RTEP_Packet;
      Timestamp   : in Ada.Real_Time.Time) is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      Packet.Timestamp := Timestamp;
   end Put_Timestamp;

   --------------------
   --  Get_Timestamp --
   --------------------
   --  Get the packet timestamp.
   function Get_Timestamp
     (Packet : in RTEP_Packet) return Ada.Real_Time.Time is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      return Packet.Timestamp;
   end Get_Timestamp;

   -------------------------
   --  Put_Multicast_Type --
   -------------------------
   --  Set if the message is multicast and what kind of multicast.
   procedure Put_Multicast_Type
      (Packet : in out RTEP_Packet;
       Mult_Type   : in Multicast_Type) is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      Packet.Multicast := Mult_Type;
   end Put_Multicast_Type;

   -------------------------
   --  Get_Multicast_Type --
   -------------------------
   --  Get the multicast type field of the packet.
   function Get_Multicast_Type
     (Packet : in RTEP_Packet) return Multicast_Type is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      return Packet.Multicast;
   end Get_Multicast_Type;

   -----------------------
   --  Set_Synchronyzed --
   -----------------------
   --  Set if this message
   procedure Set_Synchronyzed (Packet : in out RTEP_Packet;
                               Sync        : in Boolean) is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      Packet.Syncronize := Sync;
   end Set_Synchronyzed;

   -----------------------
   --  Is_Synchronyzed --
   -----------------------
   --  Returns true if the packet is synchronized
   function Is_Synchronyzed (Packet : in RTEP_Packet) return Boolean is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      return Packet.Syncronize;
   end Is_Synchronyzed;

   ---------------------------------------------
   -- RTEP_Packet_Read ( RTEP_Packet'Read  )--
   ---------------------------------------------
   --  RTEP_Packet_Read : Will raise End_Of_RTEP_Packet on error
   --  (we have arrive the end of the packet (the written packet).
   --  If the RTEP_Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Read
     (Packet  : in out RTEP_Packet;
      Item          : out Ada.Streams.Stream_Element_Array;
      Last          : out Ada.Streams.Stream_Element_Offset) is

   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;

      RTEP_Streams.Read
        (RTEP_Streams.RTEP_Streams_Type (Packet),
         Item, Last);
   end Read;

   -----------------------------------------------
   -- RTEP_Packet_Write ( RTEP_Packet'Write ) --
   -----------------------------------------------
   --  RTEP_Packet_Write: Will raise RTEP_Packet_Full on error
   --  (the packet is full thus we cant write more).

   procedure Write
     (Packet : in out RTEP_Packet;
      Item         : in     Ada.Streams.Stream_Element_Array) is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      RTEP_Streams.Write
        (RTEP_Streams.RTEP_Streams_Type (Packet), Item);
   end Write;

   ---------------------------------------
   -- Copy_Stream_Array_In_RTEP_Packet --
   ---------------------------------------
   --  This procedure will write the contents of Item in Stream.
   --  Updating the write offset properly.
   --  Will raise RTEP_Packet_Full if trying to write more than allowed.
   --  If the RTEP_Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Copy_Stream_Array_In_RTEP_Packet
     (Item : in Stream_Element_Array;
       Packet : in out RTEP_Packet) is

   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      RTEP_Streams.RTEP_Element_Array_To_Stream
        (Item, RTEP_Streams.RTEP_Streams_Type (Packet));
   end Copy_Stream_Array_In_RTEP_Packet;

   -----------------------
   -- Read_RTEP_Packet --
   -----------------------
   --  This procedure will return in Item the full buffer contents unreaded
   --  It will also return in Last the "last" written element.
   --  If clean provided, as True, will reset the Packet read counter.
   --  If not true (default), the function will update the read_counter to
   --  the end of written elements in the packet.
   --  On Error will raise End_Of_RTEP_Packet.
   --  If the RTEP_Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Read_RTEP_Packet (Packet : in out RTEP_Packet;
                                Item : out Stream_Element_Array;
                                Last : out Stream_Element_Offset;
                                Clean : in Boolean := False) is
   begin
      if Packet.Packet_In_Use = False then
         raise RTEP_Packet_Not_Allocated;
      end if;
      RTEP_Streams.RTEP_Stream_To_Element_Array
        (RTEP_Streams.RTEP_Streams_Type (Packet),
         Item, Last, Clean);
   end Read_RTEP_Packet;

end RTEP.Protocol.Packet;
