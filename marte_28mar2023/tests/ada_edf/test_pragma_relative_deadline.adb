--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--          'T e s t _ P r a g m a _ R e l a t i v e _ D e a d l i n e'
--
--                               Ada Program
--
--
--  File 'test_pragma_relative_deadline.adb'                          By MAR.
--
--  Tests the pragma relative deadline in the main task y and in a task created
--  dynamically. Also checks that the deadlines of the task are OK.
--  Both tasks uses pragma Relative_Deadline and execute a loop
--  using Delay_Until_And_Set_Deadline.
--  At the initial and the other activations the expected deadline is compared
--  against the "real" deadline obtained with EDF.Get_Deadline.
--  It is also check if the most urgent task is the one which executes.
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
pragma Task_Dispatching_Policy (EDF_Across_Priorities);
--  pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
--  pragma Task_Dispatching_Policy (Round_Robin_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);

with Text_IO; use Text_IO;
with Ada.Dispatching.EDF;
with Ada.Real_Time;
with Ada.Task_Identification;

with Execution_Load;
with Reports;

procedure Test_Pragma_Relative_Deadline is

   package EDF renames Ada.Dispatching.EDF;
   package RT renames Ada.Real_Time;

   use type RT.Time;

   -----------------------
   -- Global parameters --
   -----------------------

   Main_Prio : constant := 6;
   pragma Priority (Main_Prio);

   Time_Base : constant := 0.2;
   Dif_Magin : constant := 0.01;

   Start_Time : constant RT.Time := RT.Clock;
   --  Initial time of the test

   type Current_Task_Name is (Main_Is_Current, EDF_Task_Is_Current);

   --  Parameters of main task

   Deadline_Main : RT.Time_Span := RT.To_Time_Span (Time_Base * 0.3);
   Abs_Deadline_Main : EDF.Deadline := Start_Time + Deadline_Main;
   Main_Task_Id : Ada.Task_Identification.Task_Id;

   --  Parameters of EDF task

   Task_Loops : constant Integer := 4;
   Deadline_Task_EDF : aliased RT.Time_Span :=
     RT.To_Time_Span (Time_Base * 0.5);
   Abs_Deadline_Task_EDF : EDF.Deadline := Start_Time + Deadline_Task_EDF;
   pragma Volatile (Abs_Deadline_Task_EDF);
   Execution_Time : constant Duration :=Time_Base * 0.1;

   Task_Finished : Boolean := False;
   --  To finish the test when the EDF task has already finished
   pragma Volatile (Task_Finished);

   Has_Main_Executed : Boolean := False;
   --  To be sure main executes before the EDF task
   pragma Volatile (Has_Main_Executed);

   Task_Suspended : Boolean := False;
   --  The task is suspended or not
   pragma Volatile (Task_Suspended);

   Main_Suspended : Boolean := True;
   --  The main task is suspended or not
   pragma Volatile (Main_Suspended);

   ----------------------
   -- Deadline of main --
   ----------------------

   pragma Relative_Deadline (Deadline_Main);

   ----------------------
   -- Local procedures --
   ----------------------

   procedure Show_And_Check_Deadline
     (EDF_Task  : Ada.Task_Identification.Task_Id;
      Main_Task : Ada.Task_Identification.Task_Id;
      Current_Task : Current_Task_Name);

   function To_Str (T : RT.Time) return String;

   -------------------
   -- EDF_Task_Type --
   -------------------

   task type EDF_Task_Type (Deadline : access RT.Time_Span) is
      pragma Priority (Main_Prio - 1);
      pragma Relative_Deadline (Deadline.all);
   end EDF_Task_Type;

   task body EDF_Task_Type is
      Next_Activation : RT.Time := RT.Clock;
      Eat_Pieces : constant := 3;
   begin
      Put_Line ("Task starts execution at " & To_Str (RT.Clock));

      Reports.Assert (Has_Main_Executed);
      --  Check main has already executed (has shorted deadline than this task)

      for I in 1 .. Task_Loops loop
         Put_Line ("Task starts new job at " & To_Str (RT.Clock));

         --  check and show new deadline

         Show_And_Check_Deadline (Ada.Task_Identification.Current_Task,
                                  Main_Task_Id, EDF_Task_Is_Current);

         --  eats CPU time

         Execution_Load.Eat (Execution_Time);

         --  wait for next period

         Next_Activation := Next_Activation + Deadline_Task_EDF;
         Put_Line ("Task ends job at " & To_Str (RT.Clock)
                   & " sleeps until " & To_Str (Next_Activation));

         Task_Suspended := True;
         EDF.Delay_Until_And_Set_Deadline
           (Delay_Until_Time => Next_Activation,
            Deadline_Offset => Deadline_Task_EDF);
         Abs_Deadline_Task_EDF := Next_Activation + Deadline_Task_EDF;
         Task_Suspended := False;
      end loop;
      Task_Finished := True;
   exception
      when E : others =>
         Put_Line ("Exception in task");
   end EDF_Task_Type;

   --------------
   -- EDF_Task --
   --------------

   EDF_Task : access EDF_Task_Type :=
     new EDF_Task_Type (Deadline_Task_EDF'Access);

   -----------------------------
   -- Show_And_Check_Deadline --
   -----------------------------

   procedure Show_And_Check_Deadline
     (EDF_Task : Ada.Task_Identification.Task_Id;
      Main_Task : Ada.Task_Identification.Task_Id;
      Current_Task : Current_Task_Name) is
   begin
      Put ("  EDF Task. Expected Deadline:");
      Put (Duration'Image
        (RT.To_Duration (Abs_Deadline_Task_EDF - Start_Time)));
      Put (" Actual Deadline:");
      Put (Duration'Image
        (RT.To_Duration (EDF.Get_Deadline (EDF_Task) - Start_Time)));
      New_Line;
      Put ("  Main Task. Expected Deadline:");
      Put (Duration'Image (RT.To_Duration (Abs_Deadline_Main - Start_Time)));
      Put (" Actual Deadline:");
      Put (Duration'Image
        (RT.To_Duration (EDF.Get_Deadline (Main_Task) - Start_Time)));
      New_Line;

      --  Check if expected deadline is equal to actual deadline

      Reports.Assert
        (Task_Suspended or else
         abs (RT.To_Duration (Abs_Deadline_Task_EDF
         - EDF.Get_Deadline (EDF_Task))) < Dif_Magin);
      Reports.Assert
        (Main_Suspended or else
         abs (RT.To_Duration (Abs_Deadline_Main
           - EDF.Get_Deadline (Main_Task))) < Dif_Magin);

      --  Check if the most urgent task is executing

      case Current_Task is
         when Main_Is_Current =>
            Reports.Assert (Task_Suspended or else
                            Abs_Deadline_Main < Abs_Deadline_Task_EDF);
         when EDF_Task_Is_Current =>
            Reports.Assert (Main_Suspended or else
                            Abs_Deadline_Task_EDF < Abs_Deadline_Main);
      end case;

   end Show_And_Check_Deadline;

   ------------
   -- To_Str --
   ------------

   function To_Str (T : RT.Time) return String is
   begin
      return Duration'Image (RT.To_Duration (T - Start_Time));
   end To_Str;

   --  Main local variables
   Next_Activation : RT.Time := RT.Clock;

begin
   Reports.Init;
   Put_Line ("Test_Pragma_Relative_Deadline");
   Main_Task_Id := Ada.Task_Identification.Current_Task;
   Put_Line (" Main starts execution at " & To_Str (RT.Clock));

   while not Task_Finished loop
      Put_Line ("Main starts new job at " & To_Str (RT.Clock));
      Show_And_Check_Deadline (EDF_Task'Identity,
                               Main_Task_Id,
                               Main_Is_Current);

      Has_Main_Executed := True;

      --  wait for next period

      Next_Activation := Next_Activation + Deadline_Main;
      Put_Line ("Main ends job at " & To_Str (RT.Clock)
                & " sleeps until " & To_Str (Next_Activation));

      Main_Suspended := True;
      EDF.Delay_Until_And_Set_Deadline
           (Delay_Until_Time => Next_Activation,
            Deadline_Offset => Deadline_Main);
      Abs_Deadline_Main := Next_Activation + Deadline_Main;
      Main_Suspended := False;
   end loop;

   Reports.Test_OK;

end Test_Pragma_Relative_Deadline;
