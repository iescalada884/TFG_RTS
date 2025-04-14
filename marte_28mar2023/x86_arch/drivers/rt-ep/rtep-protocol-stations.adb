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
--           'r t e p - p r o t o c o l - s t a t i o n s . a d b'
--
--                                     Ada
--
--
--  File 'rtep-protocol-stations.adb'                              By Chema.
--                                                       Jose Maria Martinez
--                                                       <chema@gmx.net>
--
--  RT-EP Stations handling. These functions will be used to deal with
--  stations within the protocol.
--
--
--
--
-----------------------------------------------------------------------------

with RTEP.Protocol.Stations.Ring_Spec;
with Var_Strings;
with Ada.Characters.Handling;
with RTEP_Debug_Functions; use RTEP_Debug_Functions;

package body RTEP.Protocol.Stations is

   ---------------------------
   --  Get_Station_Position --
   ---------------------------
   --  Will return the position on the logical ring (starting on the first
   --  token master) of the station identified by Id.

   function Get_Station_Position (Id :  Station_ID) return Position is
   begin
      return Position (Id);
   end Get_Station_Position;

   -----------------------
   --  Get_Station_Name --
   -----------------------
   --  Will return the name of the station as defined in the logical ring
   function Get_Station_Name (Id : Station_ID) return String is
   begin
      return Var_Strings.To_String (Ring_Spec.Stations_Table
                                    (Get_Station_Position (Id)).Station_Name);
   end Get_Station_Name;

   -------------------------
   --  Check_Token_Master --
   -------------------------
   --  This function will check if sta station is the first Token Master of
   --  the ring. Returns True if Id is the first Token Master and False if
   --  not
   function Check_Token_Master (Id : Station_ID) return Boolean is

   begin
      if Get_Station_Position (Id) = 1 then
         return True;
      else
         return False;
      end if;
   end Check_Token_Master;

   -----------------------
   --  Check_Station_id --
   -----------------------
   --  Will check the station identified by Id if it is valid. If the station
   --  isn't a valid one returns False.
   function Valid_Station_Id (Id : Station_ID) return Boolean is

   begin
      if Ring_Spec.Stations_Table (Get_Station_Position (Id)).Station_In_Ring then
         return True;
      else
         return False;
      end if;

   end Valid_Station_Id;

   -------------------------
   --  Inhibit_Station_Id --
   -------------------------
   --  Will inhibit the station identified by sta_id from the ring.
   procedure Inhibit_Station_Id (Id : Station_ID) is
   begin
      DEBUG ("Called Inhibit_Station_Id: "&Get_Station_Address (Id),
         Enable_DEBUG_Failure'First);
      Ring_Spec.Stations_Table
        (Get_Station_Position (Id)).Station_In_Ring := False;
   end Inhibit_Station_Id;

   -----------------------
   --  Get_Next_Station --
   -----------------------
   --  Will return the identifier of the valid station located after the
   --  station Id in the logical ring.
   --  If no more stations on the ring Station_Not_Found will be raised
   function Get_Next_Station (Id : Station_ID) return Station_ID is
      Current_Position : constant  Position := Get_Station_Position (Id);
      Pos : Position;
   begin
      Pos := Current_Position;
      loop
         Pos :=
           (Pos mod RTEP.Number_Of_Stations) + 1;
         exit when Valid_Station_Id (Get_Station_ID_By_Position (Pos));
      end loop;
      if Pos = Current_Position then
         raise Station_Not_Found;
      else
         return Get_Station_ID_By_Position (Pos);
      end if;

   end Get_Next_Station;

   ---------------------------
   --  Get_Previous_Station --
   ---------------------------
   --  Will return the identifier of the valid station located before the
   --  station Id in the logical ring.
   --  If no more stations on the ring Station_Not_Found will be raised
   function Get_Previous_Station (Id : Station_ID) return Station_ID is
      Current_Position : constant Position := Get_Station_Position (Id);
      Pos : Position;
   begin
      Pos := Current_Position;
      loop
         if Pos = 1 then
            --  We are on the first station of the list. We have to begin
            --  the search from the last station on the list
            Pos := RTEP.Number_Of_Stations;
         else
            Pos := Pos - 1;
         end if;
         exit when Valid_Station_Id (Get_Station_ID_By_Position (Pos));
      end loop;

      if Pos = Current_Position then
         raise Station_Not_Found;
      else
         return Get_Station_ID_By_Position (Pos);
      end if;
   end Get_Previous_Station;

   --------------------------
   --  Get_Station_Address --
   --------------------------
   --  Will return the Station Address in String format.
   function Get_Station_Address (Id : Station_ID) return String is

   begin
      return String (Ring_Spec.Stations_Table
                    (Get_Station_Position (Id)).Station_Addr);
   end Get_Station_Address;

   --------------------------------
   --  Get_Station_ID_By_Address --
   --------------------------------
   --  Will return the Station Id given the Station Address in String format.
   --  Raise in case of error raise Station_Not_Found

   function Get_Station_ID_By_Address (Str : String) return Station_ID is
      Sta_Id : Station_ID;
   begin

      for I in 1 .. RTEP.Number_Of_Stations
      loop
         Sta_Id := Get_Station_ID_By_Position (I);

         if Ada.Characters.Handling.To_Upper
           (Stations.Get_Station_Address (Sta_Id)) =
           Ada.Characters.Handling.To_Upper (Str) then
            return Sta_Id;
         end if;
      end loop;
      raise Station_Not_Found;

   end Get_Station_ID_By_Address;

   -------------------------
   --  Valid_Multicast_Id --
   -------------------------
   --  Will check if the Station_ID corresponds to a Multicast address
   function Valid_Multicast_Id (Id : Station_ID) return Boolean is
   begin
      if Ring_Spec.Stations_Table
            (Get_Station_Position (Id)).Station_Addr (1 .. 2) = "01" then
         return True;
      else
         return False;
      end if;
   end Valid_Multicast_Id;

   -------------------------
   --  Check_Subscription --
   -------------------------
   --  Will check if the Station_ID is subscribed to a Multicast address
  function Check_Subscription (Multicast_Id : Station_ID;
                               Id           : Station_ID) return Boolean is
  begin
     return Ring_Spec.Subscription_List (Get_Station_Position (Id),
                                         Get_Station_Position (Multicast_Id));
  end Check_Subscription;

end RTEP.Protocol.Stations;
