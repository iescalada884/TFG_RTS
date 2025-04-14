------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--           'S i n g l y _ L i n k e d _ L i s t s . M a p . S h o w'
--
--                                Body
--
--
--  File 'sll-map-show.adb'                                            by MAR.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------

package body MaRTE.SLL.Map.Show is

   procedure Show_On_Console (ML : in Map_List) is
      P : Element_Ac;
   begin
      for I in Index_Map loop
         Show_Unsigned_32 (ML.Map_Prio (I), 16);
      end loop;
      for I in reverse Priority loop
         if not Is_Empty (ML.PL (I)) then
            Show_String ("/"); Show_Unsigned_32 (Unsigned_32 (I), 10);
            P := Element_Ac (ML.PL (I));
            while P /= null loop
               Show_Element_On_Console (P);
               P := P.Next;
            end loop;
         end if;
      end loop;
   end Show_On_Console;

   procedure Show_On_Console_And_Check (ML : in Map_List) is
      P1, P2 : Element_Ac;
   begin
      Show_On_Console (ML);

      for I in reverse Priority loop
         if not Is_Empty (ML.PL (I)) then
            P1 := Element_Ac (ML.PL (I));
            while P1.Next /= null loop
               P2 := P1.Next;
               if P2 > P1 then
                  Show_String ("ERROR:");
                  Show_Element_On_Console (P1);
                  Show_String ("<=");
                  Show_Element_On_Console (P2);
                  pragma Assert (False);
               end if;
               P1 := P2;
            end loop;
         end if;
      end loop;
   end Show_On_Console_And_Check;

end MaRTE.SLL.Map.Show;
