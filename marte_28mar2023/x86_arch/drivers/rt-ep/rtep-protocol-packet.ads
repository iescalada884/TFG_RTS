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
--               'r t e p - p r o t o c o l - p a c k e t . a d s'
--
--                                     Ada
--
--
--  File 'rtep-protocol-packet.ads'                                By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--  RT_EP Packet Internal Storage element.
--
--
--
--
--  -------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Real_Time;
with RTEP_Streams;

private package RTEP.Protocol.Packet is

   type RTEP_Packet is new RTEP_Streams.RTEP_Streams_Type with private;

   type RTEP_Packet_Ac is access all RTEP_Packet;

   --  Exceptions :
   --  RTEP_Packet_Full : If we try to write more info in the packet than it
   --                      fills.
   RTEP_Packet_Full : exception renames RTEP_Streams.RTEP_Stream_Is_Full;

   --  End_Of_Packet: If reading we try to read more than the end of the
   --                 written packet.
   End_Of_RTEP_Packet : exception renames RTEP_Streams.End_Of_RTEP_Stream;

   --  No_More_Free_Packets will raise if we ran out of free RT-EP packets.
   No_More_Free_Packets : exception;

   --  RTEP_Packet_Not_Allocated : Will raise if attempting to use a
   --                               non allocated RT-EP Packet.
   RTEP_Packet_Not_Allocated : exception;
   --  Storage_Error : If the memory management function find an inconsistency

   ------------------------------------
   --  Monitor_Init_RTEP_Packet_Pool --
   ------------------------------------
   procedure Monitor_Init_RTEP_Packet_Pool;

   ---------------------------
   --  Get_New_RTEP_Packet --
   ---------------------------
   --  This function return a pointer to a new RT-EP packet.
   --  On error will raise:
   --  No_More_Free_Packets
   function Get_New_RTEP_Packet return RTEP_Packet_Ac;

   ---------------------------
   --  Destroy_RTEP_Packet --
   ---------------------------
   --  Destroy_RTEP_Packet will free the RT-EP_Packet for further use.
   --  Changes the Packet_Ac value to null.
   --  On error will raise:
   --  Storage_Error
   procedure Destroy_RTEP_Packet
     (Packet_Ac : in out RTEP_Packet_Ac);

   -----------------------
   --  Is_Packet_In_Use --
   -----------------------
   --  This function will return true if the packet RTEP_Packet is being
   --  used. This function is usefull to determine if there are memory
   --  allocated for the packet or not.
   function Is_Packet_In_Use
     (Packet : in RTEP_Packet) return Boolean;

   ---------------------
   --  Put_Station_Id --
   ---------------------
   --  If the Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Put_Station_Id
     (Packet : in out RTEP_Packet;
      Station_Identifier : in Station_ID);

   --------------------
   -- Get_Station_Id --
   --------------------
   --  If the Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   function Get_Station_Id
     (Packet : in RTEP_Packet)
     return Station_ID;

   ---------------------
   --  Put_Channel_Id --
   ---------------------
   --  If the Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Put_Channel_Id
     (Packet : in out RTEP_Packet;
      Channel_Id : in Channel);

   ---------------------
   --  Get_Channel_Id --
   ---------------------
   --  If the Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   function Get_Channel_Id
     (Packet : in RTEP_Packet)
     return Channel;

   --------------------
   --  Put_Timestamp --
   --------------------
   --  Set the packet timestamp.
   procedure Put_Timestamp
     (Packet : in out RTEP_Packet;
      Timestamp   : in Ada.Real_Time.Time);

   --------------------
   --  Get_Timestamp --
   --------------------
   --  Get the packet timestamp.
   function Get_Timestamp
     (Packet : in RTEP_Packet) return Ada.Real_Time.Time;

   -------------------------
   --  Put_Multicast_Type --
   -------------------------
   --  Set if the message is multicast and what kind of multicast.
   procedure Put_Multicast_Type
      (Packet : in out RTEP_Packet;
       Mult_Type   : in Multicast_Type);

   -------------------------
   --  Get_Multicast_Type --
   -------------------------
   --  Get the multicast type field of the packet.
   function Get_Multicast_Type
     (Packet : in RTEP_Packet) return Multicast_Type;

   -----------------------
   --  Set_Synchronyzed --
   -----------------------
   --  Set if this message
   procedure Set_Synchronyzed (Packet : in out RTEP_Packet;
                               Sync        : in Boolean);

   -----------------------
   --  Is_Synchronyzed --
   -----------------------
   --  Returns true if the packet is synchronized
   function Is_Synchronyzed (Packet : in RTEP_Packet) return Boolean;

   ----------
   -- Read --
   ----------
   --  Read : Will raise End_Of_RTEP_Packet on error
   --  (we have arrive the end of the packet (the written packet).
   --  If the Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Read
     (Packet  : in out RTEP_Packet;
      Item         : out Ada.Streams.Stream_Element_Array;
      Last         : out Ada.Streams.Stream_Element_Offset);

   -----------
   -- Write --
   -----------
   --  Write: Will raise RTEP_Packet_Full on error
   --  (the packet is full thus we cant write more).
   --  If the RTEP_Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Write
     (Packet : in out RTEP_Packet;
      Item         : in     Ada.Streams.Stream_Element_Array);

   ----------------------------------------
   --  Copy_Stream_Array_In_RTEP_Packet --
   ----------------------------------------
   --  This procedure will write the contents of Item in Stream.
   --  Updating the write offset properly.
   --  Will raise RTEP_Packet_Full if trying to write more than allowed.
   --  If the Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Copy_Stream_Array_In_RTEP_Packet
     (Item : in Stream_Element_Array;
      Packet : in out RTEP_Packet);

   ------------------------
   --  Read_RTEP_Packet --
   ------------------------
   --  This procedure will return in Item the full buffer contents unreaded
   --  It will also return in Last the "last" written element.
   --  If clean provided, as True, will reset the Packet read counter.
   --  If not true (default), the function will update the read_counter to
   --  the end of written elements in the packet.
   --  On Error will raise End_Of_RTEP_Packet.
   --  If the Packet is not allocated will raise
   --  RTEP_Packet_Not_Allocated
   procedure Read_RTEP_Packet (Packet : in out RTEP_Packet;
                                Item : out Stream_Element_Array;
                                Last : out Stream_Element_Offset;
                                Clean : in Boolean := False);

private

   -----------------------
   --  RTEP_Packet_Ref --
   -----------------------
   --  RTEP_Packet_Ref will create an access to an RTEP_Packet.
   procedure RTEP_Packet_Ref (Packet : in out RTEP_Packet;
                               Packet_Ac : out RTEP_Packet_Ac);

   type RTEP_Packet is new RTEP_Streams.RTEP_Streams_Type with record
      --  Destination / Source Station
      Station_Identifier : Station_ID;
      --  Channel
      Channel_Id : Channel;
      --  Used by Servers in Packet_Sent to program replenishment operations
      Timestamp : Ada.Real_Time.Time;
      --  for multicast packets indicate the type of multicast
      Multicast : Multicast_Type;
      --  for sending with synchronization
      Syncronize : Boolean;
      --  is the packet ready or unallocated
      Packet_In_Use : Boolean := False;
   end record;

end RTEP.Protocol.Packet;
