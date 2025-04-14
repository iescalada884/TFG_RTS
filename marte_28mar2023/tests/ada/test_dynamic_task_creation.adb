--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                         'T e s t _ D e l a y s'
--
--                               Ada Program
--
--
--  File 'test_dynamic_task_creation.adb'                             By MAR.
--
--  Test correct execution order when dynamic task creation is performed.
--
--  The main task (high priority) creates the "first task" (low priority) and
--  sleeps for a while. When awakes it preempts "first task" and creates the
--  "second task" (same priority than "first task") and sleeps again.
--  At this point "first task" should resume execution and only when "first
--  task" finishes "second task" can execute.
--
--  FAIL: the "second task" executes before "first task" finishes. It could be
--  because during elaboration the task gets a very high priority and takes a
--  very high priority mutex. When the task unlocks the mutex goes to the head
--  of its priority queue.
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

with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Real_Time;
with Execution_Load;
with Reports;

procedure Test_Dynamic_Task_Creation is
   package RT renames Ada.Real_Time;
   use type RT.Time;

   Main_Priority : constant := 10;
   pragma Priority (Main_Priority);

   Tasks_Priority : constant := Main_Priority - 1;

   First_Task_Exec_Time : constant := 2.0;
   Second_Task_Offset : constant := First_Task_Exec_Time / 2.0;

   -- Test start time

   Start_Time : constant RT.Time := RT.Clock;

   -----------------------
   -- Control Variables --
   -----------------------

   First_Task_Running : Boolean := False;
   Second_Task_Running : Boolean := False;

   -----------------
   -- Time_To_Str --
   -----------------

   function Time_To_Str (T : RT.Time) return String is
   begin
      return Duration'Image (RT.To_Duration (T - Start_Time));
   end Time_To_Str;

   ----------------
   -- First_Task --
   ----------------

   task type First_Task is
      pragma Priority (Tasks_Priority);
   end First_Task;

   task body First_Task is
   begin
      First_Task_Running := True;
      Put_Line (Time_To_Str (RT.Clock) & ": First_Task starts");

      Reports.Assert (not Second_Task_Running);
      Execution_Load.Eat (First_Task_Exec_Time);
      Reports.Assert (not Second_Task_Running);

      Put_Line (Time_To_Str (RT.Clock) & ": First_Task ends");
      First_Task_Running := False;
   exception
      when others =>
         Put_Line ("Exception in First_Task");
         Reports.Assert (False);
   end First_Task;

   -----------------
   -- Second_Task --
   -----------------

   task type Second_Task is
      pragma Priority (Tasks_Priority);
   end Second_Task;

   task body Second_Task is
   begin
      Reports.Assert (not First_Task_Running);
      Second_Task_Running := True;
      Put_Line (Time_To_Str (RT.Clock) & ": Second_Task starts");

      Reports.Assert (not First_Task_Running);

      Put_Line (Time_To_Str (RT.Clock) & ": Second_Task ends");
      Second_Task_Running := False;
      Reports.Assert (not First_Task_Running);
   exception
      when others =>
         Put_Line ("Exception in Second_Task");
         Reports.Assert (False);
   end Second_Task;

   FT : access First_Task;
   ST : access Second_Task;

begin
   Put_Line ("Test Dynamic Task Creation");
   Reports.Init;
   Reports.Assert (not First_Task_Running);
   Reports.Assert (not Second_Task_Running);

   --  Create First Task

   Put_Line (Time_To_Str (RT.Clock) & ": Main creates first task");
   FT := new First_Task;

   --  Wait to create second task

   Put_Line (Time_To_Str (RT.Clock) & ": Main waits to create second task");

   Reports.Assert (not First_Task_Running);
   Reports.Assert (not Second_Task_Running);

   delay until Start_Time + RT.To_Time_Span (Second_Task_Offset);

   Reports.Assert (First_Task_Running);
   Reports.Assert (not Second_Task_Running);

   --  Create Second Task

   Put_Line (Time_To_Str (RT.Clock) & ": Main creates second task");
   ST := new Second_Task;

   Reports.Assert (First_Task_Running);
   Reports.Assert (not Second_Task_Running);

   --  Allow tasks to finish
   delay until Start_Time + RT.To_Time_Span (First_Task_Exec_Time);
   Reports.Assert (not First_Task_Running);
   Reports.Assert (not Second_Task_Running);

   Reports.Test_OK;
end Test_Dynamic_Task_Creation;
