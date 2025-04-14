-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                       Copyright (C) 2000                          --
--                 Universidad de Cantabria, SPAIN                   --
--                                                                   --
-- Authors: Michael Gonzalez Harbour        mgh@unican.es            --
--          Jose Javier Gutiérrez García    gutierjj@unican.es       --
--          Jose Carlos Palencia Gutiérrez  palencij@unican.es       --
--          Jose Maria Drake Moyano         drakej@unican.es         --
--          Julio Luis Medina Pasaje        medinajl@unican.es       --
--          Patricia López Martínez                                  --
--                                                                   --
-- This program is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This program is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this program; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-----------------------------------------------------------------------

generic
   type Element is private;
   with function "=" (E1,E2 : Element) return Boolean;
package Indexed_List_Generic is

   type List is private;
   type Link is access List;

   type Index is private;

   Null_Index : constant Index;

   subtype Iteration_Object is Index;

   procedure Add
     (Value      :        Element;
      The_List   : in out List     );
   -- Adds at the end of the list

   procedure Stack
     (Value      :        Element;
      The_List   : in out List     );
   -- Adds at the beginning of the list

   procedure Update
     (The_Index :        Index;
      New_Value :        Element;
      The_List  : in out List     );

   function Item
     (The_Index : Index;
      The_List  : List   )
      return Element;

   function Find
     (The_Value : Element;
      The_List  : List   )
      return Index;
   -- returnd Null_Index if not found

   function Size
     (The_List : List )
      return Natural;

   procedure Rewind
     (The_List : in List; Iterator : out Index);

   procedure Get_Next_Item
     (Value    : out Element;
      The_List : in List;
      Iterator : in out Index);
   -- May raise the No_More_Items exception. 
private

   type Node;

   type Index is access Node;

   Null_Index : constant Index:=null;

   type Node is record
      Value : Element;
      Next  : Index;
   end record;

   type List is
      record
         First, Last : Index:=null;
         Num      : Natural:=0;
      end record;

end Indexed_List_Generic;
