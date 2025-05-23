------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--    'S i n g l y _ L i n k e d _ L i s t s . O r d e r _ A d v a n c e d'
--
--                                 Body
--
--
--  File 'sll-order_advanced.adb'                                      by Mar.
--
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
package body MaRTE.SLL.Order_Advanced is

   procedure Insert_Monopriority_List (Org  : in out List;
                                       Dest : in out List) is
      Org_Head : Element_Ac := Element_Ac (Org);
      Org_Tail : Element_Ac := Element_Ac (Org);
      P        : Element_Ac := Element_Ac (Dest);
   begin
      if Org_Head = null then
         return;
      end if;

      while Org_Tail.Next /= null loop
         Org_Tail := Org_Tail.Next;
      end loop;

      if Dest = null or else Org_Head > Element_Ac (Dest) then
         Org_Tail.Next := Element_Ac (Dest);
         Dest := List (Org_Head);
      else
         loop
            if (P.Next = null or else Org_Head > P.Next) then
               Org_Tail.Next := P.Next;
               P.Next := Org_Head;
               exit;
            end if;
            P := P.Next;
         end loop;
      end if;
   end Insert_Monopriority_List;

end MaRTE.SLL.Order_Advanced;
