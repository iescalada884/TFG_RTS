--  ------------------------------------------------------------------------
--  ----------------             RT-EP              ------------------------
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
--                     'r t e p . p r o t o c o l . a d s'
--
--                                     Ada
--
--
--  File 'rtep-protocol.ads'                                       By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--
--
-----------------------------------------------------------------------------
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;
with MaRTE.Integer_Types;

package RTEP.Protocol  is

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
   --  Returns the station id of the current station
   --  Exceptions: Station_Not_Found
   function Get_Station_ID return Station_ID;
   pragma Export (C, Get_Station_ID, "rtep_get_station_id");

   --  Returns the id of the station labeled Station_Name in the ring spec
   --  Exceptions: Station_Not_Found
   function Get_Station_ID_By_Name (Station_Name : in String) return Station_ID;

   --  Returns the id of the station placed in position Pos in the logical ring
   function Get_Station_ID_By_Position (Pos : in Position) return Station_ID;

   -------------------------
   --  2)  SEND functions --
   -------------------------
   --  Sends Data over the network to a specific Channel in a the Destination
   --  Address (could be multicast) at a given Priority or through a Server
   --  Exceptions: Station_Not_Valid, Station_Not_Found, Invalid_Channel,
   --  Unexpected_Error, Info_Length_Overflow

   --  Send at a given Priority
   procedure Send_Info
     (Destination_Station_ID : in Station_ID;
      Channel_ID             : in Channel;
      Data                   : in Stream_Element_Array;
      Data_Priority          : in Priority);

   --  Send through a given Server
   procedure Send_Info
     (Destination_Station_ID : in Station_ID;
      Channel_ID             : in Channel;
      Data                   : in Stream_Element_Array;
      Id                     : in Server_ID;
      Blocking               : in Boolean := False);

   --  Generic Send at a given Priority
   --  generic
   --     type Data_Type is private;
   --  procedure Generic_Send_Info
   --     (Destination_Station_ID : in Station_ID;
   --     Channel_ID              : in Channel;
   --     Data                    : in Data_Type;
   --     Data_Priority           : in Priority);

   ----------------------------
   --  3)  RECEIVE functions --
   ----------------------------
   --  Reads from the Channel_ID the message with highest priority
   --  Exceptions: Invalid_Channel, Unexpected_Error

   --  Blocking version with Streams
   procedure Recv_Info
     (Source_Station_ID : out Station_ID;
      Channel_ID        : in Channel;
      Data              : out Stream_Element_Array;
      Last              : out Stream_Element_Offset;
      Data_Priority     : out Priority);

   --  Non-Blocking version with Streams (if No Elements, Received is false)
   procedure Try_Recv_Info
     (Source_Station_ID : out Station_ID;
      Channel_ID        : in Channel;
      Data              : out Stream_Element_Array;
      Last              : out Stream_Element_Offset;
      Data_Priority     : out Priority;
      Received          : out Boolean);

   --  Blocking version Generic
   generic
      type Data_Type is private;
   procedure Generic_Recv_Info
      (Source_Station_ID : out Station_ID;
      Channel_ID         : in Channel;
      Data               : out Data_Type;
      Data_Priority      : out Priority);

   --  Non-Blocking version Generic (if No Elements, Received is false)
   generic
      type Data_Type is private;
   procedure Generic_Try_Recv_Info
     (Source_Station_ID : out Station_ID;
      Channel_ID        : in Channel;
      Data              : out Data_Type;
      Data_Priority     : out Priority;
      Received          : out Boolean);

   --  Any_Info checks if there is any data to be received in a specific Channel
   function Any_Info (Channel_ID : in Channel) return Boolean;

   -------------------------------------
   --  4) DISTRIBUTED MUTEX functions --
   -------------------------------------
   --  Only sequential calls for the same Mutex_Id are allowed. Concurrent
   --  calls to different Mutex_Id are allowed.

   --  Init_Distributed_Mutex (only called once)
   procedure Init_Distributed_Mutex
      (M    : in Distributed_Mutex_Id);

   --  Lock_Distributed_Mutex
   procedure Lock_Distributed_Mutex
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority);

   --  Unlock_Distributed_Mutex
   procedure Unlock_Distributed_Mutex
      (M    : in Distributed_Mutex_Id;
       Prio : in RTEP.Priority);

   --  Finalize_Distributed_Mutex
   procedure Finalize_Distributed_Mutex
      (M : in Distributed_Mutex_Id);

private

   -----------------------------------------------
   --  Multicast Types (sent through the Token) --
   -----------------------------------------------
   --  Type of Multicast message in the Spare Bytes of RTEP Tokens
   subtype Multicast_Type is Interfaces.Unsigned_8 range 0 .. 3;
   No_Multicast         : constant Multicast_Type := 0;
   Multicast_Message    : constant Multicast_Type := 1;
   Lock_Dist_Mutex      : constant Multicast_Type := 2;
   Unlock_Dist_Mutex    : constant Multicast_Type := 3;

   ------------------------
   --  Multicast MESSAGE --
   ------------------------
   --  This is the structure that will go in the spare bytes of RTEP Tokens
   type RTEP_Multicast_Msg is record
      Multicast_Addr : Interfaces.Unsigned_16;
      Channel_Id     : Interfaces.Unsigned_16;
      Prio           : Interfaces.Unsigned_8;
      Info_Length    : Interfaces.Unsigned_8;
      Info           : Stream_Element_Array (1 .. RTEP.Multicast_MTU);
   end record;
   pragma Pack (RTEP_Multicast_Msg);
   RTEP_Multicast_Msg_Length : constant := 6 + RTEP.Multicast_MTU;
   for RTEP_Multicast_Msg'Size use RTEP_Multicast_Msg_Length * 8;

   --  Function to convert from a STREAM to this structure
   type Multicast_Info_Type is new
      Stream_Element_Array (1 .. RTEP_Multicast_Msg_Length);

   type Multicast_Info_Ptr is access all Multicast_Info_Type;
   type Multicast_Msg_Ptr is access all RTEP_Multicast_Msg;

   function To_Multi_Msg is
     new Ada.Unchecked_Conversion (Multicast_Info_Ptr, Multicast_Msg_Ptr);

   ----------------------------
   --  LOCK AND UNLOCK Mutex --
   ----------------------------
   RTEP_Dist_Mutex_Length : constant := 4; -- 2 + 2  bytes

   type RTEP_Dist_Mutex is record
      Mutex_Id  : Distributed_Mutex_Id; --  2 bytes
      Holder    : Station_Id; --  2 bytes
   end record;
   -- pragma Pack (RTEP_Dist_Mutex);
   for RTEP_Dist_Mutex'Size use RTEP_Dist_Mutex_Length * 8;

   --  Function to convert a STREAM to the RTEP Dist Mutex structure
   type Dist_Mutex_Stream is new
     Stream_Element_Array (1 .. RTEP_Dist_Mutex_Length);

   type Dist_Mutex_Stream_Ptr is access all Dist_Mutex_Stream;
   type RTEP_Dist_Mutex_Ptr is access all RTEP_Dist_Mutex;

   function To_Dist_Mutex is new Ada.Unchecked_Conversion
      (Dist_Mutex_Stream_Ptr, RTEP_Dist_Mutex_Ptr);
   function To_Dist_Mutex is new Ada.Unchecked_Conversion
      (Multicast_Info_Ptr, RTEP_Dist_Mutex_Ptr);

end RTEP.Protocol;
