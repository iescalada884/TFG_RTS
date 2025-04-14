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
--            r t e p - p r o t o c o l - r i n g _ s p e c . a d s'
--
--                                     Ada
--
--
--  File 'rtep-protocol-ring_spec.ads'                               By Chema.
--                                                         Jose Maria Martinez
--                                                         <chema@gmx.net>
--  Rt_Ep Logical Ring Specification
--
--
--
--
-----------------------------------------------------------------------------

with Var_Strings;
pragma Elaborate_All (Var_Strings);
private package RTEP.Protocol.Stations.Ring_Spec is

   subtype Station_String is String (1 .. 17);

   type Station_Information is record
      Station_Addr : Station_String;
      Station_Name : Var_Strings.Var_String;
      Station_In_Ring : Boolean := True;
   end record;

   type Station_DNS is array
     (1 .. RTEP.Number_Of_Stations + RTEP.Number_Of_Multicast_Addresses)
     of Station_Information;

   Stations_Table :  Station_DNS :=
     (1 => (Station_Addr => "00:30:64:07:A2:63",
            Station_Name => Var_Strings.To_Var_String ("cubo1"),
            Station_In_Ring => True),
      2 => (Station_Addr => "00:30:64:07:A2:60",
            Station_Name => Var_Strings.To_Var_String ("cubo2"),
            Station_In_Ring => True),
      3 => (Station_Addr => "00:30:64:07:A2:62",
            Station_Name => Var_Strings.To_Var_String ("cubo3"),
            Station_In_Ring => True),
      4 => (Station_Addr => "00:30:64:05:77:77",
            Station_Name => Var_Strings.To_Var_String ("cubo4"),
            Station_In_Ring => False),
      5 => (Station_Addr => "00:30:64:04:63:78",
            Station_Name => Var_Strings.To_Var_String ("cubo5"),
            Station_In_Ring => False),
      6 => (Station_Addr => "00:30:64:06:B5:F2",
            Station_Name => Var_Strings.To_Var_String ("cubo6"),
            Station_In_Ring => False),
      --  Multicast Addresses (must start with "01")
      7 => (Station_Addr => "01:FF:FF:FF:FF:FF", -- NECESSARY FOR MUTEXES!!
            Station_Name => Var_Strings.To_Var_String ("broadcast"),
            Station_In_Ring => False),
      8 => (Station_Addr => "01:00:00:00:00:AA",
            Station_Name => Var_Strings.To_Var_String ("slaves"),
            Station_In_Ring => False)
      );

   ------------------------------
   --  Multicast Subscriptions --
   ------------------------------
   --  Each Station can be subscribed to several Addresses
   type Subscription_List_Type is array
     (1 .. RTEP.Number_Of_Stations,
      RTEP.Number_Of_Stations + 1 ..
      RTEP.Number_Of_Stations + RTEP.Number_Of_Multicast_Addresses)
     of Boolean;

   Subscription_List : Subscription_List_Type :=
     (1 => (7 => True, 8 => False),
      2 => (7 => True, 8 => True),
      3 => (7 => True, 8 => True),
      4 => (7 => True, 8 => True),
      5 => (7 => True, 8 => True),
      6 => (7 => True, 8 => True));

end RTEP.Protocol.Stations.Ring_Spec;
