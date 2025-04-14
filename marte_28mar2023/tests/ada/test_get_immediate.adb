--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'T e s t _ G e t _ I m m e d i a t e'
--
--                               Ada Program
--
--
--  File 'test_get_immediate.adb'                                      By MAR.
--
--  Test if Get_Immedidate blocks only one task or the whole application.
--
--  In Linux and Linux_lib architectures the whole application gets blocked.
--  But it doesn't happen in the native run-time on Linux!!
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

pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy(Ceiling_Locking);
pragma Queuing_Policy(Priority_Queuing);

with Ada.Real_Time; use Ada.Real_Time;
with System;
with Text_IO; use Text_IO;
with Ada.Calendar;
with Ada.Dynamic_Priorities;
with Reports;

procedure Test_Get_Immediate is

   Main_Priority : constant := 10;
   pragma Priority (Main_Priority);

   Delay_Duration : constant := 1.0;

   After_Delay_Finishes : Boolean := False;
   pragma Volatile (After_Delay_Finishes);

   ------------------
   -- Task_Get_Key --
   ------------------

   task Task_Get_Key is
      pragma Priority (Main_Priority - 1);
   end Task_Get_Key;

   task body Task_Get_Key is
      C : Character;
   begin
      Put_Line ("    Task_Get_Key waits for character");

      select
         delay Delay_Duration;

         After_Delay_Finishes := True;
      then abort
         Get_Immediate (C);
         Put_Line ("    Task_Get_Key after getting character:" & C);
         Reports.Assert (False);
      end select;

      Put_Line ("    Task_Get_Key after NOT getting character");

      Put_Line ("  Task_Get_Key ends");
   exception
      when others =>
         Put_Line ("Exception in Task_Get_Key");
         Reports.Assert (False);
   end Task_Get_Key;

begin
   Put_Line ("Test_Get_Immediate");
   Reports.Init;

   --  Raises Task_Get_Key's priority

   Reports.Assert (not After_Delay_Finishes);
   Put_Line ("    - Main before raising task's priority");
   Ada.Dynamic_Priorities.Set_Priority (Main_Priority +1,
                                        Task_Get_Key'Identity);

   Reports.Assert (not After_Delay_Finishes);
   Put_Line ("    - Main after raising task's priority");

   Reports.Test_OK;
end Test_Get_Immediate;
