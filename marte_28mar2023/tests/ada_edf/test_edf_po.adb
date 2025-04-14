--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                         'T e s t _ E D F _ P O'
--
--                               Ada Program
--
--
--  File 'test_edf_po.adb'                                            By MAR.
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

with Execution_Load;
with Reports;

procedure Test_EDF_PO is

   package EDF renames Ada.Dispatching.EDF;
   package RT renames Ada.Real_Time;

   use type RT.Time;

   ----------------------
   -- Global constants --
   ----------------------

   Main_Prio : constant := 6;
   PO_Prio : constant := Main_Prio - 1;

   pragma Priority (Main_Prio);

   Task_Number : constant Integer := 2;
   Time_Base : constant := 0.2;
   Finished_Tasks : Integer := 0;
   pragma Volatile (Finished_Tasks);

   Start_Time : constant RT.Time := RT.Clock;

   ----------------
   -- Tasks Data --
   ----------------

   Deadlines : array (1 .. Task_Number) of aliased RT.Time_Span :=
     (1 => RT.To_Time_Span (Time_Base * 0.5),
      2 => RT.To_Time_Span (Time_Base * 4.0));
   Offsets : constant array (1 .. Task_Number) of Duration :=
     (1 => 0.0,
      2 => 0.0);
   Execution_Time : array (1 .. Task_Number) of Duration :=
     (1 => Time_Base * 0.1,
      2 => Time_Base);
   Execution_Time_In_Protected_Object : constant := Time_Base;
   Task_Loops : array (1 .. Task_Number) of Positive :=
     (1 => 16,
      2 =>  2);


   Abs_Deadlines : array (1 .. Task_Number) of EDF.Deadline :=
     (others => RT.Time_Last);
   pragma Volatile (Abs_Deadlines);
   pragma Volatile_Components (Abs_Deadlines);

   function Index_Of_Next_Deadline return Integer is
      Next : RT.Time := RT.Time_Last;
      Index_Of_Next : Integer := -1;
   begin
      for I in Abs_Deadlines'Range loop
         if Abs_Deadlines (I) <= Next then
            Next := Abs_Deadlines (I);
            Index_Of_Next := I;
         end if;
      end loop;

      return Index_Of_Next;
   end Index_Of_Next_Deadline;

   procedure Show_Deadlines is
   begin
      Put ("Deadlines:");
      for I in Abs_Deadlines'Range loop
         Put (Integer'Image (I) & "=>");
         if Abs_Deadlines (I) = RT.Time_Last then
            Put (" suspended");
         else
            Put (Duration'Image
                 (RT.To_Duration (Abs_Deadlines (I) - Start_Time)));
         end if;
         Put ("  ");
      end loop;
      New_Line;
   end Show_Deadlines;

   -------------
   -- Now_Str --
   -------------

   function To_Str (T : RT.Time) return String is
   begin
      return Duration'Image (RT.To_Duration (T - Start_Time));
   end To_Str;

   ----------------------
   -- Protected Object --
   ----------------------

   Task_In_PO : Boolean := False;
   pragma Volatile (Task_In_PO);

   protected Protected_Object is
      pragma Priority (PO_Prio);

      procedure Protected_Procedure (Exec_Time : Duration);

   end Protected_Object;

   protected body Protected_Object is

      procedure Protected_Procedure (Exec_Time : Duration) is
      begin
         Task_In_PO := True;
         Put_Line ("   In Protected_Procedure at " & To_Str (RT.Clock));
         Execution_Load.Eat (Exec_Time);
         Put_Line ("   Leaving Protected_Procedure at " & To_Str (RT.Clock));
         Task_In_PO := False;
      end Protected_Procedure;

   end Protected_Object;

   --------------
   -- EDF_Task --
   --------------

   task type EDF_Task (Id : Integer;
                       Deadline : access RT.Time_Span) is
      pragma Priority (Main_Prio - Id);
      --  pragma Relative_Deadline (Deadline.all);
   end EDF_Task;

   task body EDF_Task is
      Next_Activation : RT.Time := RT.Clock;
      Eat_Pieces : constant := 3;
   begin
      Put_Line ("Task" & Integer'Image (Id));
      EDF.Set_Deadline (RT.Clock + Deadline.all);

      if Offsets (Id) = 0.0 then
         Abs_Deadlines (Id) := RT.Clock + Deadline.all;
      else
         Abs_Deadlines (Id) := RT.Time_Last;
      end if;
      delay Offsets (Id);

      for I in 1 .. Task_Loops (Id) loop
         Put_Line (" Task" & Integer'Image (Id)
                   & " starts activation at " & To_Str (RT.Clock));

         --  record and show new deadline

         Abs_Deadlines (Id) := Next_Activation + Deadline.all;
         Show_Deadlines;

         --  eats CPU

         for I in 1 .. Eat_Pieces loop
            Reports.Assert (Index_Of_Next_Deadline = Id);
            Reports.Assert (not Task_In_PO);
            Execution_Load.Eat (Execution_Time (Id) / Eat_Pieces);
            Reports.Assert (Index_Of_Next_Deadline = Id);
            Reports.Assert (not Task_In_PO);
         end loop;

         --  Uses PO

         if Id = 2 then
            Protected_Object.Protected_Procedure
              (Execution_Time_In_Protected_Object);
         end if;
         Reports.Assert (not Task_In_PO);

         --  wait for next period

         Next_Activation := Next_Activation + Deadline.all;
         Put_Line (" Task" & Integer'Image (Id)
                   & " ends activation at " & To_Str (RT.Clock)
                   & " sleeps until " & To_Str (Next_Activation));
         Abs_Deadlines (Id) := RT.Time_Last;

         EDF.Delay_Until_And_Set_Deadline
           (Delay_Until_Time => Next_Activation,
            Deadline_Offset => Deadline.all);
      end loop;
      Finished_Tasks := Finished_Tasks + 1;
   exception
      when E : others =>
         Put_Line ("Exception in task" & Integer'Image (Id));
   end EDF_Task;

   EDF_Tasks : array (1 .. Task_Number) of access EDF_Task :=
     (1 => new EDF_Task (1, Deadlines (1)'Access),
      2 => new EDF_Task (2, Deadlines (2)'Access));

begin
   Reports.Init;
   Put_Line ("Test_EDF_Deadlines");

   while Finished_Tasks /= Task_Number loop
      delay Time_Base / 1.3;
   end loop;

   Reports.Test_OK;

end Test_EDF_PO;
