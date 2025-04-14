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
--               'r t e p - p r o t o c o l - s y s t e m . a d b'
--
--                                     Ada
--
--
--  File 'rtep-protocol-system.adb'                                By Chema.
--                                                       Jose Maria Martinez
--                                                           <chema@gmx.net>
--
--  RT-EP System Independant functions. This functions is the external API
--  to the system.
--
--
--
--
-----------------------------------------------------------------------------

with Interfaces; use Interfaces;
with POSIX_IO; pragma Elaborate_All (POSIX_IO);
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings;
with Ada.Exceptions;
with Ada.Text_IO;
with POSIX;

with Ethernet_Driver;
with RTEP.Protocol.Stations;
with RTEP_Debug_Functions;
use RTEP_Debug_Functions;

package body RTEP.Protocol.System is

   ------------------------------------------
   --  0) INTERNAL TYPES AND CONSTANTS
   --  1) GLOBAL VARIABLES
   --  2) MACROS and GENERIC instantiations
   --  3) FUNCTIONS
   ------------------------------------------

   --------------------------------------
   --  0) INTERNAL TYPES AND CONSTANTS --
   --------------------------------------
   Max_Ethernet_Frame : constant := 1514; -- without the FCS

   type Ethernet_Address is new Stream_Element_Array (1 .. 6);
   for Ethernet_Address'Size use 6*8;

   subtype Station_String is String (1 .. 17);

   type Ethernet_Address_Octect is range 0 .. 16#FF#;

   type Ether_Head is record
      Dest_Addr : Ethernet_Address;
      Source_Addr : Ethernet_Address;
      Protocol : Interfaces.Unsigned_16;
      Dest_Sta : Interfaces.Unsigned_16;
   end record;
   pragma Pack (Ether_Head);
   for Ether_Head'Size use 16 * 8;

   type Ether_Head_Stream_Array is new Stream_Element_Array (1 .. 16);
   for Ether_Head_Stream_Array'Size use 16 * 8;

   --------------------------
   -- 1) GLOBAL VARIABLES --
   --------------------------
   Current_Station_Address : Ethernet_Address; --  stored at initialization
   Communication_Enabled : Boolean := False; --  is comunication enabled or not
   Network_Fd : POSIX_IO.File_Descriptor; --  Network File descriptor.
   Tx_Eth_Frame : Stream_Element_Array (1 .. Max_Ethernet_Frame);
   Rx_Eth_Frame : Stream_Element_Array (1 .. Max_Ethernet_Frame);

   -------------------------------------------
   -- 2) MACROS and GENERIC instantiations --
   -------------------------------------------
   --   a) Unchecked conversions
   function Ether_Head_To_Ether_Stream_Array is new Ada.Unchecked_Conversion
      (Ether_Head, Ether_Head_Stream_Array);

   function Ioctl_Addr_To_Ethernet_Address is new Ada.Unchecked_Conversion
      (Ethernet_Driver.Eth_Addr_Ioctl_Arg, Ethernet_Address);

   function Ether_Stream_Array_To_Ether_Head is new Ada.Unchecked_Conversion
      (Ether_Head_Stream_Array, Ether_Head);

   --   b) Generic instantiations
   procedure Ioctl_Net is new POSIX_IO.Generic_Ioctl
      (Ethernet_Driver.Ioctl_Cmd_Addr, Ethernet_Driver.Eth_Addr_Ioctl_Arg);

   procedure Ioctl_Protocol is new POSIX_IO.Generic_Ioctl
      (Ethernet_Driver.Ioctl_Cmd_Protocol, Ethernet_Driver.Eth_Proto_Arg);

   package Str_Int_IO is new Ada.Text_IO.Integer_IO (Ethernet_Address_Octect);

   --   c) Ethernet_Address_To_String_Address
   function Ethernet_Address_To_String_Address
     (Eth_Addr : in Ethernet_Address)
     return String is
      Index : Integer := 1;
      Str2 : String (1 .. 6);
      Sta_String : Station_String;
   begin
      for I in Ethernet_Address'Range loop
         Index := 1 + 3*(Integer (I) - 1);
         if Eth_Addr (I) > 16#F# then
            Str_Int_IO.Put (Str2, Ethernet_Address_Octect (Eth_Addr (I)), 16);
            Sta_String (Index .. Index + 1) := Str2 (4 .. 5);
         else
            Str_Int_IO.Put (Str2, Ethernet_Address_Octect (Eth_Addr (I)), 16);
            Sta_String (Index .. Index) := "0";
            Sta_String (Index + 1 .. Index + 1) := Str2 (5 .. 5);
         end if;
         if I /= 6 then
            Sta_String (Index + 2 .. Index + 2) := ":";
         end if;
      end loop;
      return String (Sta_String);
   end Ethernet_Address_To_String_Address;

   --   d) SHORT_little_endian_TO_big_endian
   function SHORT_little_endian_TO_big_endian
      (Protocol : in Interfaces.Unsigned_16) return Interfaces.Unsigned_16 is
   begin
      return (Shift_Right (Protocol, 8) and 16#00FF#) or
             (Shift_Left (Protocol, 8) and 16#FF00#);
   end SHORT_little_endian_TO_big_endian;

   -------------------
   --  3) FUNCTIONS --
   -------------------

   ----------------------------------
   --  Get_Current_Station_Address --
   ----------------------------------
   --  Will return in station the address in string format. The format will
   --  be the same as specified in the Logical Ring.
   --  In any problem determining the Address will raise Network_Error
   function Get_Current_Station_Address return String is
   begin
      if Communication_Enabled = False then
         raise Network_Error;
      end if;
      return Ethernet_Address_To_String_Address (Current_Station_Address);
   end Get_Current_Station_Address;

   --------------------------
   --  Set_Promiscous_Mode --
   --------------------------
   --  Will set the promiscous mode in the sistem. The system has to be
   --  able to receive all the frames transmited in the media.
   --  In case of error Network_Error will be raised
   procedure Set_Promiscous_Mode is
   begin
      --  no need to enable, it is enabled in the network driver
      --  and with switches we don't need promiscous mode
      if Communication_Enabled = False then
         raise Network_Error;
      end if;
   end Set_Promiscous_Mode;

   --------------------------------
   --  Open_RTEP_Communication --
   --------------------------------
   --  Will make the required adjustments in order to be able to read/write
   --  from/to the media. Consecutive calls to this open function will have
   --  no effect.
   --  On error opening the comunications Network_Error will be raised.
   procedure Open_RTEP_Communication is
      Ioctl_Net_Data : Ethernet_Driver.Eth_Addr_Ioctl_Arg;
      Ioctl_Net_Protocol : Ethernet_Driver.Eth_Proto_Arg :=
        Ethernet_Driver.Eth_Proto_Arg (RTEP.RTEP_Protocol_Number);
   begin
      Network_Fd := POSIX_IO.Open
        (POSIX.To_POSIX_String (RTEP.Device_Name), POSIX_IO.Read_Write);
      Communication_Enabled := True;
      --  Once oppened we get the MAC address of the interface.
      Ioctl_Net (Network_Fd, Ethernet_Driver.Eth_Hardware_Address,
                Ioctl_Net_Data);
      Current_Station_Address :=
         Ioctl_Addr_To_Ethernet_Address (Ioctl_Net_Data);
      --  Set the reception Filter
      Ioctl_Protocol (Network_Fd, Ethernet_Driver.Set_Protocol_Filter,
                         Ioctl_Net_Protocol);
   exception
      when others =>
         raise Network_Error;
   end Open_RTEP_Communication;

   --------------------------------
   --  Close_RTEP_Communication --
   --------------------------------
   --  Close the media. Closing an unopened communication will have no effect
   procedure Close_RTEP_Communication is
   begin
      POSIX_IO.Close (Network_Fd);
      Communication_Enabled := False;
   end Close_RTEP_Communication;

   ------------------------
   --  Send_RTEP_Packet --
   ------------------------
   --  Will send to destination Station the RTEP_Packet.
   --  On error Sending data Network_Error will be raised.
   procedure Send_RTEP_Packet
      (Dest_Station : in Station_ID;
       RTEP_Packet  : in Stream_Element_Array)
   is
      Last : Stream_Element_Offset;
      Ethernet_Head : Ether_Head;
      Last_Tx : Ada.Streams.Stream_Element_Offset;
      Dest_Addr : constant Ethernet_Address := (others => 16#FF#);
   begin
      if Communication_Enabled = False then
         raise Network_Error;
      end if;

      Ethernet_Head.Dest_Addr := Dest_Addr;
      Ethernet_Head.Source_Addr := Current_Station_Address;
      Ethernet_Head.Protocol := SHORT_little_endian_TO_big_endian
         (RTEP.RTEP_Protocol_Number);
      -- Insert the Destination Station Position into the Eth Frame
      Ethernet_Head.Dest_Sta :=
        Interfaces.Unsigned_16 (Stations.Get_Station_Position (Dest_Station));
      --  We insert the header in the frame.
      Tx_Eth_Frame (1 .. Ether_Head_Stream_Array'Length) :=
        Ada.Streams.Stream_Element_Array
        (Ether_Head_To_Ether_Stream_Array (Ethernet_Head));
      --  We Read The Info.
      Last := Ether_Head_Stream_Array'Length + RTEP_Packet'Length;
      Tx_Eth_Frame (Ether_Head_Stream_Array'Length + 1 .. Last) :=
        RTEP_Packet (1 .. RTEP_Packet'Length);
      --  We now write in the media Last elements.
      POSIX_IO.Write (Network_Fd, Tx_Eth_Frame (1 .. Last), Last_Tx);
   exception
      when E : others =>
         ERROR ("Send_RTEP_Packet(): ");
         ERROR (Ada.Exceptions.Exception_Message (E));
         raise Network_Error;
   end Send_RTEP_Packet;

   ---------------------------
   --  Receive_RTEP_Packet --
   ---------------------------
   --  Will receive an RTEP_Packet sended by the Source_Station to the
   --  Destination_Station
   --
   --  On error Receiving data Network_Error will be raised.
   procedure Receive_RTEP_Packet
     (Dest_Station   : out Station_ID;
      Source_Station : out Station_ID;
      RTEP_Packet    : out Stream_Element_Array;
      Last           : out Stream_Element_Offset)
   is
      Last_Rx : Ada.Streams.Stream_Element_Offset;
      Ethernet_Head : Ether_Head;
   begin
      if Communication_Enabled = False then
         raise Network_Error;
      end if;

      POSIX_IO.Read (Network_Fd, Rx_Eth_Frame, Last_Rx);

      Ethernet_Head := Ether_Stream_Array_To_Ether_Head (Ether_Head_Stream_Array
         (Rx_Eth_Frame (1 .. Ether_Head_Stream_Array'Length)));

      RTEP_Packet (1 .. Last_Rx - Ether_Head_Stream_Array'Length) :=
        Rx_Eth_Frame (Ether_Head_Stream_Array'Length + 1 .. Last_Rx);
      Last := Last_Rx - Ether_Head_Stream_Array'Length;

      Dest_Station := Get_Station_ID_By_Position
         (Integer(Ethernet_Head.Dest_Sta));
      Source_Station := Stations.Get_Station_ID_By_Address
         (Ethernet_Address_To_String_Address (Ethernet_Head.Source_Addr));
   exception
      when Station_Not_Found =>
            --  Dest_Sta or Source_Addr not in the ring. To discard the frame
            --  Source and Dest will be the current station.
            Dest_Station := Stations.Get_Next_Station (Get_Station_ID);
            Source_Station := Stations.Get_Next_Station (Get_Station_ID);
      when E : others =>
         ERROR ("Receive_RTEP_Packet(): ");
         ERROR (Ada.Exceptions.Exception_Message (E));
         raise Network_Error;
   end Receive_RTEP_Packet;

end RTEP.Protocol.System;
