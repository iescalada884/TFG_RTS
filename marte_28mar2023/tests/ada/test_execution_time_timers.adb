--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--            'T e s t _ E x e c u t i o n _ T i m e _ T i m e r s'
--
--                               Ada Program
--
--
--  File 'test_execution_time_timers.adb'                            By MAR.
--
--  Test the execution time timers. Only one consumer task is created. This test
--  should be removed and replaced by 'test_execution_time_timers2.adb' when it
--  works OK
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
pragma Locking_Policy (Ceiling_Locking);

with Text_IO; use Text_IO;
with Ada.Execution_Time;
with Ada.Execution_Time.Timers;
with Ada.Real_Time;
with Ada.Task_Identification;
with System;
with Execution_Load_Loop;
with Execution_Time_Timers_Test_PO;
with Reports;

procedure Test_Execution_Time_Timers is
   pragma Priority (12);

   package ET renames Ada.Execution_Time;
   package PO renames Execution_Time_Timers_Test_PO;
   package RT renames Ada.Real_Time;

   Main_Prio : constant := 6;

   Task_Loops : constant Integer := 8;
   Task_Execution_Time : constant := 0.15;

   task type Consumer (Id : Character) is
      pragma Priority (Main_Prio - 1);
   end Consumer;

   task body Consumer is
      SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span;
   begin
      for I in 1 .. Task_Loops loop
         Ada.Execution_Time.Split (Ada.Execution_Time.Clock, SC, TS);
         --Put_Line ("Consumer " & Id & ":" & RT.Seconds_Count'Image (SC) & "s" &
         --          Duration'Image (RT.To_Duration (TS)) & "ns");
         Execution_Load_Loop.Eat (Task_Execution_Time);

         if Id = 'A' then
            PO.PO.Add_Loop1;
         else
            PO.PO.Add_Loop2;
         end if;
         delay Task_Execution_Time / 3.0;
      end loop;
      Put_Line ("Consuming task finishes");
   end Consumer;

   T1 : Consumer ('A');
   T2 : Consumer ('B');

   --  T_Id : aliased Ada.Task_Identification.Task_Id := Consuming'Identity;
   --  CPU_Timer : Ada.Execution_Time.Timers.Timer (T_Id'Access);
   --  When I do this here T_Id doesn't get a valid value (is that correct??)

begin
   Reports.Init;
   Put_Line ("Test_Execution_Time_Timers");

   PO.PO.Set_Execution_Time (Task_Execution_Time);

   declare
      T_Id1 : aliased Ada.Task_Identification.Task_Id := T1'Identity;
      CPU_Timer1 : Ada.Execution_Time.Timers.Timer (T_Id1'Access);
      T_Id2 : aliased Ada.Task_Identification.Task_Id := T2'Identity;
      CPU_Timer2 : Ada.Execution_Time.Timers.Timer (T_Id2'Access);
   begin
      --  Set first expiration of execution-time timers
      Ada.Execution_Time.Timers.Set_Handler
        (CPU_Timer1,
         Ada.Execution_Time.Time_Of
           (0, RT.To_Time_Span (Task_Execution_Time / 2.0)),
         PO.PO.CPU_Timer_Handler1'Access);
      Ada.Execution_Time.Timers.Set_Handler
        (CPU_Timer2,
         Ada.Execution_Time.Time_Of
           (0, RT.To_Time_Span (Task_Execution_Time / 2.0)),
         PO.PO.CPU_Timer_Handler2'Access);

      --  loop to test execution time accounting in a more complex situation
      for I in 1 .. Task_Loops loop
         Execution_Load_Loop.Eat (Task_Execution_Time * 1.2);
         delay Task_Execution_Time * 3.1;
         Put_Line ("Count1:" & Integer'Image(PO.PO.Get_Count1) &
                   "  Loops1:" & Integer'Image(PO.PO.Get_Loops1));
         Put_Line ("Count2:" & Integer'Image(PO.PO.Get_Count2) &
                   "  Loops2:" & Integer'Image(PO.PO.Get_Loops2));
         Reports.Assert (abs (PO.PO.Get_Count1 - (PO.PO.Get_Loops1 + 1)) <= 1);
         Reports.Assert (abs (PO.PO.Get_Count2 - (PO.PO.Get_Loops2 + 1)) <= 1);
      end loop;
   end;
   Put_Line ("Main task finishes");

   Reports.Test_OK;

end Test_Execution_Time_Timers;
