--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--     'T e s t _ G r o u p _ B u d g e t s _ C h a n g e _
--                                         T a s k s _ I n _ G r o u p'
--
--                               Ada Program
--
--
--  File 'test_group_budgets_change_tasks_in_group.adb'                By MAR.
--
--  Tests the Group budgets when tasks are added and removed.
--  Tests the invocation of Replenish from inside the handler
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
with Ada.Execution_Time.Group_Budgets;
with Ada.Real_Time;
with Ada.Dynamic_Priorities;

with Group_Budgets_Handler_PO;
with Execution_Load_Loop;
with Reports;

procedure Test_Group_Budgets_Change_Tasks_In_Group is

   package GBs renames Ada.Execution_Time.Group_Budgets;
   package PO renames Group_Budgets_Handler_PO;
   package RT renames Ada.Real_Time;

   Main_Prio : constant := 6;

   pragma Priority (Main_Prio);

   Task_Loops : constant Integer := 4;
   Task_Number : constant Integer := 5;
   Task_Execution_Time : constant := 0.15;
   Extra_Time : constant := Task_Execution_Time * 0.3;
   --  A bit more time to be sure the handler expires

   Handler_Count : Integer := 0;

   task type Time_Consumer (Id : Integer) is
      pragma Priority (Main_Prio - 1);
   end Time_Consumer;

   task body Time_Consumer is
   begin
      Put_Line ("Task" & Integer'Image (Id));
      for I in 1 .. Task_Loops loop
         Put_Line ("Task" & Integer'Image (Id) & " starts");
         Execution_Load_Loop.Eat (Task_Execution_Time);
         Put_Line ("Task" & Integer'Image (Id) & " ends");
         delay 0.0;  --  Yields processor
      end loop;
   exception
      when E : others =>
         Put_Line ("Exception in task" & Integer'Image (Id));
   end Time_Consumer;

   T1 : Time_Consumer (1);
   T2 : Time_Consumer (2);
   T3 : Time_Consumer (3);
   T4 : Time_Consumer (4);
   T5 : Time_Consumer (5);

   GB : GBs.Group_Budget;

begin
   Reports.Init;
   Put_Line ("Test_Group_Budgets_Handler");

   --  Create group

   GB.Add_Task (T1'Identity);
   GB.Add_Task (T2'Identity);
   GB.Add_Task (T3'Identity);
   GB.Add_Task (T4'Identity);
   GB.Add_Task (T5'Identity);

   Reports.Assert (GB.Budget_Has_Expired and PO.PO.Handler_Count = 0);

   PO.PO.Set_Period (Task_Execution_Time);
   GB.Set_Handler (PO.PO.Periodic_Handler'Access);

   --  Set group budget for one execution

   GB.Add (RT.To_Time_Span (Task_Execution_Time));

   Reports.Assert (not GB.Budget_Has_Expired and PO.PO.Handler_Count = 0);

   --  Allow to execute the 5 tasks

   delay Task_Execution_Time * Task_Number + Extra_Time;

   Reports.Assert (PO.PO.Handler_Count = 5);
   Handler_Count := PO.PO.Handler_Count;
   Put_Line ("Handler_Count:" & Integer'Image (Handler_Count));

   --  Remove 3 tasks

   GB.Remove_Task (T1'Identity);
   GB.Remove_Task (T3'Identity);
   GB.Remove_Task (T5'Identity);

   --  Allow to execute the 5 tasks

   delay Task_Execution_Time * Task_Number + Extra_Time;

   Reports.Assert (PO.PO.Handler_Count - Handler_Count = 2);
   Handler_Count := PO.PO.Handler_Count;
   Put_Line ("Handler_Count:" & Integer'Image (Handler_Count));

   --  Add 2 tasks

   GB.Add_Task (T3'Identity);
   GB.Add_Task (T1'Identity);

   --  Allow to execute the 5 tasks

   delay Task_Execution_Time * Task_Number + Extra_Time;

   Reports.Assert (PO.PO.Handler_Count - Handler_Count = 4);
   Handler_Count := PO.PO.Handler_Count;
   Put_Line ("Handler_Count:" & Integer'Image (Handler_Count));

   --  Allow tasks to terminate

   delay Task_Execution_Time * Task_Number * 2;

   Handler_Count := PO.PO.Handler_Count;

   --  Give some time, PO.Handler_Count shouldn't change

   delay Task_Execution_Time * 2;

   Reports.Assert (PO.PO.Handler_Count - Handler_Count = 0);
   Handler_Count := PO.PO.Handler_Count;
   Put_Line ("Handler_Count:" & Integer'Image (Handler_Count));

   Reports.Test_OK;

end Test_Group_Budgets_Change_Tasks_In_Group;
