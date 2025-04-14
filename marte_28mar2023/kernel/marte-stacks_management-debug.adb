------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--               'S t a c k s _ M a n a g e m e n t . D e b u g'
--
--                                 Body
--
--
--  File 'marte-stacks_management-debug.adb'                           By MAR.
--
--
--  Debug messages for the task stacks management.
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
with MaRTE.Debug_Messages;
with MaRTE.Direct_IO;

package body MaRTE.Stacks_Management.Debug is

   package DIO renames MaRTE.Direct_IO;
   use type INT.Unsigned_32;

   procedure Initialize_Stack (Id             : in Natural;
                               Stack_Top      : in Stack_Memory_Area_Ac;
                               Size_In_DWords : in Stack_Pos) is
   begin
      if not MaRTE.Debug_Messages.Stacks_Management_Messages'First then
         return;
      end if;

      DIO.Put (" | InitStack:");
      DIO.Put (Id);
      DIO.Put (" Sz:");
      DIO.Put (INT.Unsigned_32 (Size_In_DWords) * 4, Base => 16);
   end Initialize_Stack;

   procedure Show_Stack (S : in Stack_Id;
                         Is_OK : in Boolean;
                         Task_Stack_Top : in INT.Unsigned_32 := 0;
                         Use_Task_Stack_Top : in Boolean := False) is
      P1, P2, T, B, TT, Sz : INT.Unsigned_32;
   begin
      if not MaRTE.Debug_Messages.Stacks_Management_Messages'First then
         return;
      end if;

      P1 := S.Stack (Stack_Memory_Area'First);
      P2 := S.Stack (Stack_Memory_Area'First + 1);
      T := Get_Stack_Top_Address (S);
      B := Get_Stack_Base_Address (S);
      if Use_Task_Stack_Top then
         TT := Task_Stack_Top;
      end if;
      Sz  := INT.Unsigned_32 (S.Size_In_DWords) * 4;
      if not Is_Ok then
         DIO.Put (" Stack error:");
         DIO.Put (" P1:"); DIO.Put (P1, Base => 16);
         DIO.Put (" P2:"); DIO.Put (P2, Base => 16);
         DIO.Put (" T:"); DIO.Put (T, Base => 16);
         DIO.Put (" B:"); DIO.Put (B, Base => 16);
         DIO.Put (" Sz:"); DIO.Put (Sz, Base => 16);
         --  DIO.Put (" Sz:"); DIO.Put (Sz);
         DIO.Put (" T<TT<B:"); DIO.Put (T, Base => 16);
         if Use_Task_Stack_Top then
            DIO.Put ("<"); DIO.Put (TT, Base => 16);
         end if;
         DIO.Put ("<"); DIO.Put (B, Base => 16);
         DIO.New_Line;
--        else
--           DIO.Put (" |");
--           DIO.Put (" T<TT<B:"); DIO.Put (T, Base => 16);
--           DIO.Put ("<"); DIO.Put (TT, Base => 16);
--           DIO.Put ("<"); DIO.Put (B, Base => 16);
      end if;
   end;

end MaRTE.Stacks_Management.Debug;
