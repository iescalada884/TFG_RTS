------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--               'S t a c k s _ M a n a g e m e n t . D e b u g'
--
--                                 Spec
--
--
--  File 'marte-stacks_management-debug.ads'                           By MAR.
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
with MaRTE.Integer_Types;

private package MaRTE.Stacks_Management.Debug is

   package INT renames MaRTE.Integer_Types;

   procedure Initialize_Stack (Id             : in Natural;
                               Stack_Top      : in Stack_Memory_Area_Ac;
                               Size_In_DWords : in Stack_Pos);

   procedure Show_Stack (S : in Stack_Id;
                         Is_OK : in Boolean;
                         Task_Stack_Top : in INT.Unsigned_32 := 0;
                         Use_Task_Stack_Top : in Boolean := False);

end MaRTE.Stacks_Management.Debug;
