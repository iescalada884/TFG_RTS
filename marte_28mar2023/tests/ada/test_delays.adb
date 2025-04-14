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
--  File 'test_delays.adb'                                               By MAR.
--
--  Test absolute and relative delays using Duration, Ada.Calendar.Time and
--  Ada.Real_Time.Time.
--
--  Three tasks, one task using each delay mechanisism are started with an
--  offset between them. The test checks the tasks execute the same number of
--  times and always in the same order.
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
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Dynamic_Priorities;
with Ada.Calendar;
--  with Execution_Load;
with Reports;

procedure Test_Delays is

   Main_Priority : constant := 10;
   pragma Priority (Main_Priority);

   Delay_Duration : constant := 0.2;
   Offset : constant := Delay_Duration / 8; -- offset between the tasks
   Task_Loops : constant := 3;

   --  Task loop counters

   Task_Abs_Real_Time_Loop_Counter : Integer := 0;
   pragma Volatile (Task_Abs_Real_Time_Loop_Counter);

   Task_Abs_Calendar_Loop_Counter : Integer := 0;
   pragma Volatile (Task_Abs_Calendar_Loop_Counter);

   Task_Rel_Duration_Loop_Counter : Integer := 0;
   pragma Volatile (Task_Rel_Duration_Loop_Counter);

   Main_Counter : Integer := 0;
   pragma Volatile (Main_Counter);

   ------------------------
   -- Task_Abs_Real_Time --
   ------------------------

   task Task_Abs_Real_Time_1 is
      pragma Priority (Main_Priority - 1);
   end Task_Abs_Real_Time_1;

   task body Task_Abs_Real_Time_1 is
      Next_Activation : Ada.Real_Time.Time;
   begin
      Put_Line ("  Task_Abs_Real_Time_1 starts");
      delay Offset;
      Reports.Assert (Main_Counter = 1);

      Next_Activation := Ada.Real_Time.Clock;
      for I in 1 .. Task_Loops loop
         Put_Line ("    Task_Abs_Real_Time_1 loops");

         --  Increment loops counter

         Task_Abs_Real_Time_Loop_Counter :=
           Task_Abs_Real_Time_Loop_Counter + 1;

         --  Check execution order is correct

         Reports.Assert (Task_Rel_Duration_Loop_Counter = I - 1);
         Reports.Assert (Task_Abs_Calendar_Loop_Counter = I - 1);

         --  wait for next period

         Next_Activation := Next_Activation +
           Ada.Real_Time.To_Time_Span (Delay_Duration);
         delay until Next_Activation;

         Reports.Assert (Main_Counter = 2);
      end loop;

      Task_Abs_Real_Time_Loop_Counter :=
        Task_Abs_Real_Time_Loop_Counter + 1;
      Put_Line ("  Task_Abs_Real_Time_1 ends");
   exception
      when others =>
         Put_Line ("Exception in Task_Abs_Real_Time_1");
         Reports.Assert (False);
   end Task_Abs_Real_Time_1;

   -----------------------
   -- Task_Rel_Duration --
   -----------------------

   task Task_Rel_Duration_2 is
      pragma Priority (Main_Priority - 1);
   end Task_Rel_Duration_2;

   task body Task_Rel_Duration_2 is
      use type Ada.Calendar.Time;
      Next_Activation : Ada.Calendar.Time;
   begin
      Put_Line ("  Task_Rel_Duration_2 starts");
      delay Offset * 2.0;
      Reports.Assert (Main_Counter = 1);

      Next_Activation := Ada.Calendar.Clock;
      for I in 1 .. Task_Loops loop
         Put_Line ("    Task_Rel_Duration_2 loops");

         --  Increment loops counter

         Task_Rel_Duration_Loop_Counter :=
           Task_Rel_Duration_Loop_Counter + 1;

         --  Check execution order is correct

         Reports.Assert (Task_Abs_Real_Time_Loop_Counter = I);
         Reports.Assert (Task_Abs_Calendar_Loop_Counter = I - 1);

         --  wait for next period

         Next_Activation := Next_Activation + Delay_Duration;
         delay Next_Activation - Ada.Calendar.Clock;
         --  The race condition for the relative delay cannot happen in this
         --  example since the task cannot be preempted (all tasks with the
         --  same priority and policy FIFO_Within_Priorities)

         Reports.Assert (Main_Counter = 2);
      end loop;

      Task_Rel_Duration_Loop_Counter :=
        Task_Rel_Duration_Loop_Counter + 1;
      Put_Line ("  Task_Rel_Duration_2 ends");

   exception
      when others =>
         Put_Line ("Exception in Task_Rel_Duration_2");
         Reports.Assert (False);
   end Task_Rel_Duration_2;

   -----------------------
   -- Task_Abs_Calendar --
   -----------------------

   task Task_Abs_Calendar_3 is
      pragma Priority (Main_Priority - 1);
   end Task_Abs_Calendar_3;

   task body Task_Abs_Calendar_3 is
      use type Ada.Calendar.Time;
      Next_Activation : Ada.Calendar.Time;
   begin
      Put_Line ("  Task_Abs_Calendar_3 starts");
      delay Offset * 3.0;
      Reports.Assert (Main_Counter = 1);

      Next_Activation := Ada.Calendar.Clock;
      for I in 1 .. Task_Loops loop
         Put_Line ("    Task_Abs_Calendar_3 loops");

         --  Increment loops counter

         Task_Abs_Calendar_Loop_Counter :=
           Task_Abs_Calendar_Loop_Counter + 1;

         --  Check execution order is correct

         Reports.Assert (Task_Abs_Real_Time_Loop_Counter = I);
         Reports.Assert (Task_Rel_Duration_Loop_Counter = I);

         --  wait for next period

         Next_Activation := Next_Activation + Delay_Duration;
         delay until Next_Activation;

         Reports.Assert (Main_Counter = 2);
      end loop;

      Task_Abs_Calendar_Loop_Counter :=
        Task_Abs_Calendar_Loop_Counter + 1;
      Put_Line ("  Task_Abs_Calendar_3 ends");

   exception
      when others =>
         Put_Line ("Exception in Task_Abs_Calendar_3");
         Reports.Assert (False);
   end Task_Abs_Calendar_3;

begin
   Put_Line ("Test_Delays");
   Reports.Init;

   Reports.Assert (Task_Abs_Real_Time_Loop_Counter = 0);
   Reports.Assert (Task_Rel_Duration_Loop_Counter = 0);
   Reports.Assert (Task_Abs_Calendar_Loop_Counter = 0);

   --  "Launch" Task_Abs_Real_Time

   Ada.Dynamic_Priorities.Set_Priority (Main_Priority,
                                        Task_Abs_Real_Time_1'Identity);

   --  "Launch" Task_Rel_Duration

   Ada.Dynamic_Priorities.Set_Priority (Main_Priority,
                                        Task_Rel_Duration_2'Identity);

   --  "Launch" Task_Abs_Calendar

   Ada.Dynamic_Priorities.Set_Priority (Main_Priority,
                                        Task_Abs_Calendar_3'Identity);

   --  Periodically check task execution

   Main_Counter := 1;
   for I in 0 .. Task_Loops loop
      Put_Line ("    - Main loops");

      Reports.Assert (Task_Abs_Real_Time_Loop_Counter = I);
      Reports.Assert (Task_Rel_Duration_Loop_Counter = I);
      Reports.Assert (Task_Abs_Calendar_Loop_Counter = I);

      delay Delay_Duration;

      Main_Counter := 2;

      Reports.Assert (Task_Abs_Real_Time_Loop_Counter = I+1);
      Reports.Assert (Task_Rel_Duration_Loop_Counter = I+1);
      Reports.Assert (Task_Abs_Calendar_Loop_Counter = I+1);
   end loop;

   Reports.Test_OK;
end Test_Delays;
