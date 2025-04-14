--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--           'T e s t _ G r o u p _ B u d g e t s _ N o _ H a n d l e r'
--
--                               Ada Program
--
--
--  File 'test_group_budgets_no_handler.adb'                          By MAR.
--
--  Test the Group budgets when there is no handler programmed.
--  Group budgets are armed even if the handler is null. Expiration of budget
--  can be detected with Budget_Has_Expired.
--
--  In this test, 3 task are added to a group. The main task (not in the group)
--  checks if the budget expires or not.
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

with MaRTE_OS;
with Text_IO; use Text_IO;
with Ada.Execution_Time.Group_Budgets;
with Ada.Real_Time;

with Execution_Load_Loop;
with Reports;

procedure Test_Group_Budgets_No_Handler_No_Dot is

   package GBs renames Ada.Execution_Time.Group_Budgets;
   package RT renames Ada.Real_Time;

   pragma Priority (6);

   Task_Loops : constant Integer := 2;
   Task_Number : constant Integer := 3;
   Task_Execution_Time : constant := 0.25;

   task type Time_Consumer (Id : Integer) is
      pragma Priority (5);
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

   GB : GBs.Group_Budget;

begin
   Reports.Init;
   Put_Line ("Test_Group_Budgets_No_Handler");

   --  Create group

   GBs.Add_Task (GB, T1'Identity);
   GBs.Add_Task (GB, T2'Identity);
   GBs.Add_Task (GB, T3'Identity);

   Reports.Assert (GBs.Budget_Has_Expired (GB));

   --  Set group budget for 2 executions
   GBs.Add (GB, RT.To_Time_Span (Task_Execution_Time*2));

   Reports.Assert (not GBs.Budget_Has_Expired (GB));

   --  Main task "disturbs" the other tasks
   --  Budget will expire during the second call to delay

   for I in 1 .. Task_Loops loop
      Reports.Assert ((not GBs.Budget_Has_Expired (GB) and I <= 2)
                     or (GBs.Budget_Has_Expired (GB) and I > 2));

      Put_Line ("Main Task starts");
      Execution_Load_Loop.Eat (Task_Execution_Time);
      Put_Line ("Main Task ends");

      Reports.Assert ((not GBs.Budget_Has_Expired (GB) and I <= 2)
                     or (GBs.Budget_Has_Expired (GB) and I > 2));

      delay Task_Execution_Time*1.5;

      Reports.Assert ((not GBs.Budget_Has_Expired (GB) and I <= 1)
                     or (GBs.Budget_Has_Expired (GB) and I > 1));
   end loop;

   Reports.Assert (GBs.Budget_Has_Expired (GB));
   Reports.Test_OK;

end Test_Group_Budgets_No_Handler_No_Dot;
