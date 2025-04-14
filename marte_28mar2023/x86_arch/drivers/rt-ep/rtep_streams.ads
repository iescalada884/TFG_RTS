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
--                       'r t _ e p _ s t r e a m s . a d s'
--
--                                     Ada
--
--
--  File 'RTEP_streams.ads'                                       By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  Streams that will store the Info that the protocol may handle.
--
--
--
--
-----------------------------------------------------------------------------


with RTEP;
with Ada.Streams; use Ada.Streams;


package RTEP_Streams is

   -----------------
   --  Exceptions --
   -----------------
   --  Stream_Is_Full : If we try to write more info in the stream than it
   --                   fills.
   RTEP_Stream_Is_Full : exception;
   --
   --  End_Of_Stream : If reading we arrive the end of the written stream.
   End_Of_RTEP_Stream : exception;

   -----------------------
   --   RT-EP Streams   --
   -----------------------

   type RTEP_Streams_Type is new Ada.Streams.Root_Stream_Type with private;

   type RTEP_Streams_Type_Ac is
     access all RTEP_Streams_Type;

   Max_RTEP_Data : constant := RTEP.Max_RTEP_MTU;

   --   This function will create an access to a Stream type.
   procedure Stream_Ptr (RTEP_Stream : in out  RTEP_Streams_Type;
                         RTEP_Stream_Ptr : out RTEP_Streams_Type_Ac);

   --  This function will reset the read counter to 0 so you can read the
   --  stream from the beginning.
   procedure Reset_Read_Counter (Stream : in out RTEP_Streams_Type);
   --  This function will reset the write counter to 0 so you can write
   --  the stream from the beginning.
   procedure Reset_Write_Counter (Stream : in out RTEP_Streams_Type);
   --  This function will return the Offset of the Stream that is the number
   --  of written Stream_Element in the stream.
   function Read_Stream_Offset (Stream : RTEP_Streams_Type)
                               return Stream_Element_Offset;

   -------------------------------------
   --  RTEP_Stream_To_Element_Array ---
   -------------------------------------
   --  This procedure will return in Item the full buffer contents unreaded
   --  It will also return in Last the "last" written element.
   --  If clean provided, as True, will reset the Stream read counter.
   --  If not true (default), the function will update the read_counter to
   --  the end of written elements in the stream.
   --  On Error will raise End_Of_RTEP_Stream.

   procedure RTEP_Stream_To_Element_Array (Stream : in out RTEP_Streams_Type;
                                            Item : out Stream_Element_Array;
                                            Last : out Stream_Element_Offset;
                                            Clean : in Boolean := False);

   -------------------------------------
   --  RTEP_Element_Array_To_Stream ---
   -------------------------------------
   --  This procedure will write the contents of Stream_Element in Stream.
   --  Updating the write offset properly.
   --  Will raise RTEP_Stream_Is_Full if trying to write more than allowed.

   procedure RTEP_Element_Array_To_Stream
     (Stream_Element : in Stream_Element_Array;
      Stream : in out RTEP_Streams_Type);


   --  Read: Will raise End_Of_Stream on error (we have arrive the end of the
   --  stream (the written stream) for that type, there is no type'length
   --  available in the stream .
   procedure Read (Stream  : in out RTEP_Streams_Type;
                   Item    :    out Ada.Streams.Stream_Element_Array;
                   Last    :    out Ada.Streams.Stream_Element_Offset);

   --  Write: Will raise Stream_Is_Full on error (the stream is full thus
   --  we cant write more).
   procedure Write (Stream  : in out RTEP_Streams_Type;
                    Item    : in     Ada.Streams.Stream_Element_Array);

   ---------------------------
   -- Some Useful Functions --
   ---------------------------
   function To_String (Stream : Stream_Element_Array) return String;

   function To_Stream_Element_Array (S : String) return Stream_Element_Array;

private

   type RTEP_Streams_Type is new Ada.Streams.Root_Stream_Type
     with record
        Data   : Stream_Element_Array (1 .. Max_RTEP_Data);
        Read_Offset  : Stream_Element_Count := 0;
        Wrote_Offset : Stream_Element_Count := 0;
     end record;
end RTEP_Streams;
