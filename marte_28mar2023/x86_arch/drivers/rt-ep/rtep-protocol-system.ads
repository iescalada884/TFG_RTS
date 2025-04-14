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
--               'r t e p - p r o t o c o l - s y s t e m . a d s'
--
--                                     Ada
--
--
--  File 'rtep-protocol-system.ads'                                By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  RT-EP System Independant functions. This functions is the esternal API
--  to the system.
--
--
--
--
-----------------------------------------------------------------------------

private package RTEP.Protocol.System is

   --  Network_Error : Will raise if there is a problem setting dealing with
   --  the network interface.
   Network_Error : exception;


   ----------------------------------
   --  Get_Current_Station_Address --
   ----------------------------------
   --  Will return in station the address in string format. The format will
   --  be the same as specified in the Logical Ring.
   --  In any problem determining the Address will raise Network_Error
   function Get_Current_Station_Address return String;

   --------------------------
   --  Set_Promiscous_Mode --
   --------------------------
   --  Will set the promiscous mode in the sistem. The system has to be
   --  able to receive all the frames transmited in the media.
   --  In case of error Network_Error will be raised
   procedure Set_Promiscous_Mode;


   --------------------------------
   --  Open_RTEP_Communication --
   --------------------------------
   --  Will make the required adjustments in order to be able to read/write
   --  from/to the media. Consecutive calls to this open function will have
   --  no effect.
   --  On error opening the comunications Network_Error will be raised.

   procedure Open_RTEP_Communication;

   --------------------------------
   --  Close_RTEP_Communication --
   --------------------------------
   --  Will close the media. To close an unopened communicacion will have
   --  no effect.
   procedure Close_RTEP_Communication;


   ------------------------
   --  Send_RTEP_Packet --
   ------------------------
   --  Will send to destination Station the RTEP_Packet.
   --  On error Sending data Network_Error will be raised.
   procedure Send_RTEP_Packet (Dest_Station : in Station_ID;
                                RTEP_Packet : in Stream_Element_Array);

   ---------------------------
   --  Receive_RTEP_Packet --
   ---------------------------
   --  Will receive an RTEP_Packet sended by the Source_Station to the
   --  Destination_Station
   --  On error Receiving data Network_Error will be raised.
   procedure Receive_RTEP_Packet (Dest_Station : out Station_ID;
                                   Source_Station : out Station_ID;
                                   RTEP_Packet :  out Stream_Element_Array;
                                   Last : out Stream_Element_Offset);



end RTEP.Protocol.System;
