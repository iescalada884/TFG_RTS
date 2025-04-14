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
--                       'r t _ e p _ s t r e a m s . a d b'
--
--                                     Ada
--
--
--  File 'RTEP_streams.adb'                                       By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  Streams that will store the Info that the protocol may handle.
--
--
--
--
-----------------------------------------------------------------------------


package body RTEP_Streams is

   -----------------------
   --   RT-EP Streams ----
   -----------------------

   procedure Stream_Ptr (RTEP_Stream : in out  RTEP_Streams_Type;
                          RTEP_Stream_Ptr : out RTEP_Streams_Type_Ac) is
   begin

      RTEP_Stream_Ptr := RTEP_Stream'Unchecked_Access;

   end Stream_Ptr;

   procedure Read (Stream  : in out RTEP_Streams_Type;
                   Item    :    out Ada.Streams.Stream_Element_Array;
                   Last    :    out Ada.Streams.Stream_Element_Offset) is
   begin

      if Stream.Read_Offset + Item'Length > Stream.Wrote_Offset then
         raise End_Of_RTEP_Stream;
      else
         Item := Stream.Data (Stream.Data'First +
                              Stream.Read_Offset .. (Stream.Data'First +
                                                     Stream.Read_Offset
                                                     + Item'Length - 1));

         --  Last := Stream.Read_Offset + Item'Length - 1;
         Stream.Read_Offset := Stream.Read_Offset + Item'Length;
         --  According to ARM95  14.13.1 8: The index of the last stream
         --  element transferred is returned in Last. Last is less than
         --  Item'Last only if the end of the stream is reached.
         Last := Stream.Read_Offset - 1 + Stream.Data'First;
      end if;
   end Read;

   procedure Write (Stream  : in out RTEP_Streams_Type;
                    Item    : in     Ada.Streams.Stream_Element_Array) is
   begin

      if Item'Length + Stream.Wrote_Offset > Max_RTEP_Data then
         raise RTEP_Stream_Is_Full;
      else
         Stream.Data (Stream.Data'First + Stream.Wrote_Offset ..
                      (Stream.Data'First + Stream.Wrote_Offset +
                       Item'Length - 1))
           := Item;
         Stream.Wrote_Offset := Stream.Wrote_Offset + Item'Length;
      end if;
   end Write;


   --  This function will reset the read counter to 0 so you can read the
   --  stream from the beginning.
   procedure Reset_Read_Counter (Stream : in out RTEP_Streams_Type) is
   begin
      Stream.Read_Offset := 0;
   end Reset_Read_Counter;
   --  This function will reset the write counter to 0 so you can write
   --  the stream from the beginning.
   procedure Reset_Write_Counter (Stream : in out RTEP_Streams_Type) is
   begin
      Stream.Wrote_Offset := 0;
   end Reset_Write_Counter;


   function Read_Stream_Offset (Stream : RTEP_Streams_Type)
                               return Stream_Element_Offset is

   begin
      return Stream_Element_Offset (Stream.Wrote_Offset);
   end Read_Stream_Offset;
   ------------------------------------
   -- RTEP_Stream_To_Element_Array ---
   ------------------------------------
   procedure RTEP_Stream_To_Element_Array (Stream : in out RTEP_Streams_Type;
                                            Item : out Stream_Element_Array;
                                            Last : out Stream_Element_Offset;
                                           Clean : in Boolean := False) is

   begin
      if Stream.Read_Offset >= Stream.Wrote_Offset then
         raise End_Of_RTEP_Stream;
      end if;

      Last := Stream.Data'First + Stream.Read_Offset + Stream.Wrote_Offset - 1;

      Item (Item'First .. Last) :=
        Stream.Data (Stream.Data'First + Stream.Read_Offset ..
                     Stream.Data'First + Stream.Read_Offset +
                     Stream.Wrote_Offset - 1);

      if Clean then
         Reset_Read_Counter (Stream);
      else
         Stream.Read_Offset := Stream.Wrote_Offset;
      end if;

   end RTEP_Stream_To_Element_Array;

   -------------------------------------
   --  RTEP_Element_Array_To_Stream ---
   -------------------------------------
   --  This procedure will write the contents of Stream_Element in Stream.
   --  Updating the write offset properly.

   procedure RTEP_Element_Array_To_Stream
     (Stream_Element : in Stream_Element_Array;
      Stream : in out RTEP_Streams_Type) is
   begin
      if Stream_Element'Length + Stream.Wrote_Offset > Max_RTEP_Data then
         raise RTEP_Stream_Is_Full;
      else
         Stream.Data (Stream.Data'First + Stream.Wrote_Offset ..
                      (Stream.Data'First + Stream.Wrote_Offset +
                       Stream_Element'Length - 1))
           := Stream_Element;
         Stream.Wrote_Offset := Stream.Wrote_Offset + Stream_Element'Length;
      end if;

   end RTEP_Element_Array_To_Stream;

   ---------------------------
   -- Some Useful Functions --
   ---------------------------

   function To_String (Stream : Stream_Element_Array) return String is
      Result : String (1 .. Stream'Length);
   begin
      for Index in Result'Range loop
         Result (Index) :=
           Character'Val (Stream_Element'Pos
                          (Stream (Stream_Element_Offset (Index) +
                                   Stream'First - 1)));
      end loop;
      return Result;
   end To_String;

   function To_Stream_Element_Array (S : String) return Stream_Element_Array is
      Buffer  : Stream_Element_Array (1 .. S'Length);
   begin
      for Index in 1 .. Buffer'Length loop
         Buffer (Stream_Element_Offset (Index)) :=
           Stream_Element'Val (Character'Pos (S (Index)));
      end loop;
      return Buffer;
   end To_Stream_Element_Array;

end RTEP_Streams;
