--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--               'T e s t _ D y n a m i c _ P r i o r i t i e s'
--
--                               Ada Program
--
--
--  File 'test_dynamic_priorities.adb'                                By MAR.
--
--  Test different situations between two tasks that dynamically change their
--  priorities:
--     - Raise task.
--
--  Also
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

with System;
with Ada.Dynamic_Priorities;
with Text_IO; use Text_IO;
with Execution_Load;
with Reports;

procedure Test_Dynamic_Priorities is

   Main_Priority : constant System.Priority :=
     System.Priority'First + 3;
   pragma Priority (Main_Priority);

   Protected_Procedure_Duration : constant := 0.2;

   Task_1_Step : Integer := 0;
   pragma Volatile (Task_1_Step);

   Task_2_Step : Integer := 0;
   pragma Volatile (Task_2_Step);

   ----------------------
   -- Protected_Object --
   ----------------------

   protected Protected_Object is
      pragma Priority (Main_Priority - 1);

      procedure Protected_Procedure;
   end Protected_Object;

   protected body Protected_Object is

      procedure Protected_Procedure is
      begin
         Put_Line ("    In Protected_Procedure");
         Execution_Load.Eat (Protected_Procedure_Duration);
         Put_Line ("    Leaving Protected_Procedure");
      end Protected_Procedure;

   end Protected_Object;

   --------------
   --  Task_2  --
   --------------

   task Task_2 is
      pragma Priority (Main_Priority - 2);
   end Task_2;

   --------------
   --  Task_1  --
   --------------

   task Task_1 is
      pragma Priority (Main_Priority - 1);
   end Task_1;

   task body Task_1 is
   begin
      Put_Line ("T1 step:" & Integer'Image (Task_1_Step));

      --  (1) Task_1 executes before Task_2

      Reports.Assert (Task_2_Step = 0);

      --  (2) Set prio of Task_2 to the same prio than Task_1: Task_1 is not
      --  preempted

      Put_Line ("T1 sets prio of T2 to its same prio");
      Ada.Dynamic_Priorities.Set_Priority
        (Ada.Dynamic_Priorities.Get_Priority, Task_2'Identity);
      Reports.Assert (Task_2_Step = 0);

      Task_1_Step := Task_1_Step + 1; -- 1
      Put_Line ("T1 step:" & Integer'Image (Task_1_Step));

      --  (3) Set prio of this task with the current value: the task go to the
      --  tail of the priority queue and Task_2 is chosen to execute

      Put_Line ("T1 re-sets its prio to the current value");
      Ada.Dynamic_Priorities.Set_Priority
        (Ada.Dynamic_Priorities.Get_Priority);

      --  (6) Executes after Task_2 raises Task_1 priority

      Reports.Assert (Task_2_Step = 1);

      Task_1_Step := Task_1_Step + 1; -- 2
      Put_Line ("T1 step:" & Integer'Image (Task_1_Step));

      --  (7) Task_1 sleeps for a while

      Put_Line ("T1 sleeps");
      delay Protected_Procedure_Duration / 2;

      --  (9) Task_1 returns to CPU while Task_2 is in the protected procedure
      --  and raises Task_2 priority: priority change has not effect until
      --  Task_2 leaves the protected operation (RM D.5.1 (10/2))

      Put_Line ("T1 Finishes sleep and raises T2 prio (postponed)");
      Reports.Assert (Task_2_Step = 2);
      Ada.Dynamic_Priorities.Set_Priority
        (Ada.Dynamic_Priorities.Get_Priority + 1, Task_2'Identity);
      Reports.Assert (Task_2_Step = 2);

      --  (10) Task_1 sleeps again to allow Task_2 to execute

      Task_1_Step := Task_1_Step + 1; -- 3
      Put_Line ("T1 step:" & Integer'Image (Task_1_Step));
      Put_Line ("T1 sleeps again");
      delay Protected_Procedure_Duration / 2;

      --  (14) Only after Task_2 terminates, Task_1 can execute again

      Put_Line ("T1 after final sleep");
      Reports.Assert (Task_2_Step = 4);

      Put_Line ("T1 terminates");

   exception
      when others =>
         Put_Line ("Exception in Task_1");
         Reports.Assert (False);
   end Task_1;

   --------------------
   --  Task_2 (body) --
   --------------------

   task body Task_2 is
   begin
      Put_Line ("  T2 step:" & Integer'Image (Task_2_Step));

      --  (4) Task_2 executes after Task_1 re-sets its priority

      Reports.Assert (Task_1_Step = 1);

      Task_2_Step := Task_2_Step + 1; -- 1
      Put_Line ("  T2 step:" & Integer'Image (Task_2_Step));

      --  (5) Raises Task_1 priority: Task_2 is preempted again

      Put_Line ("  T2: Raises Task_1 priority");
      Ada.Dynamic_Priorities.Set_Priority
        (Ada.Dynamic_Priorities.Get_Priority + 1, Task_1'Identity);

      --  (8) Executes after Task_1

      Reports.Assert (Task_1_Step = 2);

      Task_2_Step := Task_2_Step + 1; -- 2
      Put_Line ("  T2 step:" & Integer'Image (Task_2_Step));

      Put_Line ("  T2 calls protected procedure");
      Protected_Object.Protected_Procedure;  -- Preempted by Task_1

      --  (11) Executes after Task_1 sleeps again. Task_2 has its new priority

      Reports.Assert (Task_1_Step = 3);
      Reports.Assert
        (Ada.Dynamic_Priorities.Get_Priority =
           Ada.Dynamic_Priorities.Get_Priority (Task_1'Identity) + 1);

      Task_2_Step := Task_2_Step + 1; -- 3
      Put_Line ("  T2 step:" & Integer'Image (Task_2_Step));

      --  (12) Executes for a while but it is not preempted by Task_1 because
      --  its priority is bigger

      Execution_Load.Eat (Protected_Procedure_Duration * 2);
      Reports.Assert (Task_1_Step = 3);

      --  (13) Finishes to allow Task_1 to execute

      Task_2_Step := Task_2_Step + 1; -- 4
      Put_Line ("  T2 step:" & Integer'Image (Task_2_Step));
      Put_Line ("  T2 terminates");

   exception
      when others =>
         Put_Line ("Exception in Task_2");
         Reports.Assert (False);
   end Task_2;

begin
   Reports.Init;

   --  Lowers its priority to allow tasks to execute

   Ada.Dynamic_Priorities.Set_Priority (System.Priority'First);

   Reports.Test_OK;

end Test_Dynamic_Priorities;
