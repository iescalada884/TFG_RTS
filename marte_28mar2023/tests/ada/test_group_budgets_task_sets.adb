--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--         'T e s t _ G r o u p _ B u d g e t s _ T a s k _ S e t s'
--
--                               Ada Program
--
--
--  File 'test_group_budgets_task_sets.adb'                          By MAR.
--
--  Test the Group budgets task sets: adding and removing task and membership
--  tests (Add_Task, Remove_Task, Is_Member, Is_A_group_Member and Members).
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
with Ada.Task_Identification;

with Reports;

procedure Test_Group_Budgets_Task_Sets is

   package GBs renames Ada.Execution_Time.Group_Budgets;

   Main_Prio : constant := 6;

   pragma Priority (Main_Prio);

   task type Group_Task (Id : Integer) is
      pragma Priority (Main_Prio - 1);
   end Group_Task;

   task body Group_Task is
   begin
      Put_Line ("Task" & Integer'Image (Id));
   exception
      when E : others =>
         Put_Line ("Exception in task" & Integer'Image (Id));
         Reports.Assert (False);
   end Group_Task;

   type Group_Task_Ac is access all Group_Task;

   T1 : aliased Group_Task (1);
   T2 : aliased Group_Task (2);
   T3 : aliased Group_Task (3);
   T4 : aliased Group_Task (4);
   T5 : aliased Group_Task (5);
   Ts : array (1 .. 5) of Group_Task_Ac := (T1'Access,
                                            T2'Access,
                                            T3'Access,
                                            T4'Access,
                                            T5'Access);

   type Bools is array (Ts'Range) of Boolean;

   GB1 : GBs.Group_Budget;
   GB2 : GBs.Group_Budget;

   procedure Check_Tasks_In_Groups (GB : GBs.Group_Budget;
                                    T_In_Group  : Bools;
                                    T_In_Other_Group : Bools) is
      T_Array : GBs.Task_Array := GB.Members;
      Actually_In_Group : Bools := (others => False);
      Found : Boolean;
      use type Ada.Task_Identification.Task_Id;
   begin
      --  Which task are in the group?

      for I in T_Array'Range loop
         Found := False;

         --  What task in Ts corresponds to T_Array (J)?

         for J in Ts'Range loop
            if T_Array (I) = Ts (J)'Identity then
               --  The task must be in the group

               Reports.Assert (T_In_Group (J));

               --  Task found

               Actually_In_Group (J) := True;
               Found := True;
               exit;
            end if;
         end loop;

         Reports.Assert (Found); -- task in group not in Ts
      end loop;
      --  At this point we know all the tasks in T_Array are in Ts and
      --  correspond to "true" values in T_In_Group

      --  Actually_In_Group and T_In_Group must be equal
      Reports.Assert (Actually_In_Group = T_In_Group);

      --  Check membership

      for I in Ts'Range loop
         --  Check group membership

         Reports.Assert (GB.Is_Member (Ts (I)'Identity) = T_In_Group (I));

         --  Check other group membership

         Reports.Assert (GBs.Is_A_Group_Member (Ts (I)'Identity) =
                           T_In_Group (I) or T_In_Other_Group (I));
      end loop;
   end Check_Tasks_In_Groups;

   --  Start of processing for Test_Group_Budgets_Task_Sets

begin
   Reports.Init;
   Put_Line ("Test_Group_Budgets_Task_Sets");

   --  Create first group

   Put_Line ("Create first group");
   GB1.Add_Task (Ts (1)'Identity);
   Check_Tasks_In_Groups (GB1, (1 => True, others => False),
                          (others => False));
   GB1.Add_Task (Ts (2)'Identity);
   Check_Tasks_In_Groups (GB1, (1 => True, 2 => True, others => False),
                          (others => False));
   GB1.Add_Task (Ts (3)'Identity);
   Check_Tasks_In_Groups (GB1,
                          (1 => True, 2 => True, 3 => True, others => False),
                          (others => False));

   --  Try to add a task that is already a member (nothing should happen)

   Put_Line ("Try to add a task that is already a member");
   GB1.Add_Task (Ts (2)'Identity);
   Check_Tasks_In_Groups (GB1,
                          (1 => True, 2 => True, 3 => True, others => False),
                          (others => False));

   --  Create second group

   Put_Line ("Create second group");
   GB2.Add_Task (Ts (4)'Identity);
   Check_Tasks_In_Groups (GB2,
                          (4 => True, others => False),
                          (1 => True, 2 => True, 3 => True, others => False));
   GB2.Add_Task (Ts (5)'Identity);
   Check_Tasks_In_Groups (GB2,
                          (4 => True, 5 => True, others => False),
                          (1 => True, 2 => True, 3 => True, others => False));

   --  Try to add a task that is a member of another group

   Put_Line ("Try to add a task that is a member of another group");
   begin
      GB1.Add_Task (Ts (4)'Identity);
      Reports.Assert (False); -- Should raise an exception
   exception
      when GBs.Group_Budget_Error =>
        null; -- The correct behaviour is to raise the exception
   end;
   Check_Tasks_In_Groups (GB1,
                          (1 => True, 2 => True, 3 => True, others => False),
                          (4 => True, 5 => True, others => False));

   --  Remove task from one group and put in the other

   Put_Line ("Remove task from one group and put in the other");
   GB1.Remove_Task (Ts (2)'Identity);
   Check_Tasks_In_Groups (GB1,
                          (1 => True, 3 => True, others => False),
                          (4 => True, 5 => True, others => False));

   GB2.Add_Task (Ts (2)'Identity);
   Check_Tasks_In_Groups (GB2,
                          (2 => True, 4 => True, 5 => True, others => False),
                          (1 => True, 3 => True, others => False));

   --  Try to remove a task that is NOT a member

   Put_Line ("Try to remove a task that is NOT a member");
   begin
      GB1.Remove_Task (Ts (4)'Identity);
      Reports.Assert (False); -- Should raise an exception
   exception
      when GBs.Group_Budget_Error =>
        null; -- The correct behaviour is to raise the exception
   end;
   Check_Tasks_In_Groups (GB1,
                          (1 => True, 3 => True, others => False),
                          (2 => True, 4 => True, 5 => True, others => False));

   --  Try to remove a task that has terminated

   Put_Line ("Try to remove a task that has terminated");
   delay 0.1;  -- Allow tasks to terminate
   begin
      GB1.Remove_Task (Ts (1)'Identity);
      Reports.Assert (False); -- Should raise an exception
   exception
      when Tasking_Error =>
        null; -- The correct behaviour is to raise the exception
   end;

   --  Check the group is empty now

   Reports.Assert (GB1.Members'Last = 0);

   --  Test membership with a terminated task

   Put_Line ("Test membership with a terminated task");
   declare
      Bool : Boolean;
   begin
      Bool := GB1.Is_Member (Ts (4)'Identity);
      Reports.Assert (False); -- Should raise an exception
   exception
      when Tasking_Error =>
        null; -- The correct behaviour is to raise the exception
   end;

   --  Test membership with any group with a terminated task

   Put_Line ("Test membership with any group with a terminated task");
   declare
      Bool : Boolean;
   begin
      Bool := GBs.Is_A_Group_Member (Ts (2)'Identity);
      Reports.Assert (False); -- Should raise an exception
   exception
      when Tasking_Error =>
        null; -- The correct behaviour is to raise the exception
   end;

   Reports.Test_OK;

exception
   when E : others =>
      Put_Line ("Exception in main");
      Reports.Assert (False);
end Test_Group_Budgets_Task_Sets;
