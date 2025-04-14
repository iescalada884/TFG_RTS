--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                       'T e s t _ E D F _ T a s k _ E n t r y'
--
--                               Ada Program
--
--
--  File 'test_edf_entry.adb'                        By MAR.
--
--  Tests task entries works with EDF.
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

with Ada.Text_IO;
with Ada.Dispatching.EDF;
with Ada.Real_Time;

with Execution_Load;
with Reports;

procedure Test_EDF_Task_Entry is

   package EDF renames Ada.Dispatching.EDF;
   package RT renames Ada.Real_Time;
   package TIO renames Ada.Text_IO;

   use type RT.Time;
   
   ------------------
   -- Step_Counter --
   ------------------
   
   Step_Counter : Natural := 0;
   pragma Volatile (Step_Counter);
   
   EDF_D_Past_Task_Has_Executed_Eat : Boolean := False;
   pragma Volatile (EDF_D_Past_Task_Has_Executed_Eat);
     
   ----------------------
   -- Global constants --
   ---------------------- 
   Time_Base : constant := 0.03;
   Task_Deadline_N : constant := Time_Base * 10;
   Task_Loops : constant := 9;
   Delay_Eat_Time : constant := Task_Deadline_N * (Task_Loops / 3);

   Main_Prio : constant := 60;
   Main_Deadline : constant RT.Time_Span := RT.To_Time_Span (Time_Base * 5);
   Task_D_Past_Prio : constant := 40;
   Task_D_Past_Deadline : constant RT.Time_Span := RT.To_Time_Span (Time_Base);
   Task_Prio : constant := 1;
   Task_Deadline : constant RT.Time_Span := RT.To_Time_Span (Task_Deadline_N);
   
   --------------------------------
   -- Main scheduling parameters --
   --------------------------------
   
   pragma Priority (Main_Prio);
   pragma Relative_Deadline (Main_Deadline);         

   ---------------------
   -- EDF_D_Past_Task --
   ---------------------

   task EDF_D_Past_Task is
      pragma Priority (Task_D_Past_Prio);
      pragma Relative_Deadline (Task_D_Past_Deadline);
   end EDF_D_Past_Task;

   task body EDF_D_Past_Task is
      Initial_Time : RT.Time := RT.Clock;
   begin
      Reports.Init;
      Reports.Assert (Step_Counter = 0);
      Step_Counter := Step_Counter + 1;
      
      TIO.Put_Line ("EDF_D_Past_Task Set_Deadline");
      EDF.Set_Deadline (Initial_Time + Task_D_Past_Deadline);
      Reports.Assert (Step_Counter = 1);
      Step_Counter := Step_Counter + 1;
      
      TIO.Put_Line ("EDF_D_Past_Task BEFORE delay");
      delay Delay_Eat_Time * 2;     
      TIO.Put_Line ("EDF_D_Past_Task AFTER delay and BEFORE eat");
      
      Reports.Assert (Step_Counter = 6);
      Step_Counter := Step_Counter + 1;
      
      EDF_D_Past_Task_Has_Executed_Eat := True;
      Execution_Load.Eat (Delay_Eat_Time);     
      TIO.Put_Line ("EDF_D_Past_Task AFTER eat");
      
      Reports.Assert (Step_Counter = 7);
      Step_Counter := Step_Counter + 1;
      
   exception
      when E : others =>
         TIO.Put_Line ("Exception in EDF_D_Past task");
   end EDF_D_Past_Task;

   --------------
   -- EDF_Task --
   --------------

   task type EDF_Task is
      pragma Priority (Task_Prio);
      pragma Relative_Deadline (Task_Deadline);
      entry Go;
   end EDF_Task;

   task body EDF_Task is
      Next_Activation : RT.Time := RT.Clock;
   begin
      Reports.Assert (Step_Counter = 3);
      Step_Counter := Step_Counter + 1;
      
      TIO.Put_Line ("EDF_Task BEFORE entry");
      accept Go do
         TIO.Put_Line ("EDF_Task IN entry");
      end Go;
      TIO.Put_Line ("EDF_Task AFTER entry");
      Reports.Assert (Step_Counter = 5);
      Step_Counter := Step_Counter + 1;
      
      for I in 1 .. Task_Loops loop
         TIO.Put_Line ("EDF_Task loop " & I'Img &
                         " Step_Counter:" & Step_Counter'Img);
         
         Reports.Assert ((not EDF_D_Past_Task_Has_Executed_Eat and
                           Step_Counter = 6) or
                          (EDF_D_Past_Task_Has_Executed_Eat and
                                Step_Counter = 8));
         
         Next_Activation := Next_Activation + Task_Deadline;
         EDF.Delay_Until_And_Set_Deadline (Delay_Until_Time => Next_Activation,
                                           Deadline_Offset  => Task_Deadline);
      end loop;
      
      Reports.Assert (EDF_D_Past_Task_Has_Executed_Eat);
      Reports.Assert (Step_Counter = 8);
   exception
      when E : others =>
         TIO.Put_Line ("Exception in EDF task");
   end EDF_Task;
   
   T1 : EDF_Task;

begin
   Reports.Init;
   TIO.Put_Line ("Test_EDF_Task_Entry ");
     
   TIO.Put_Line ("Main before first delay");
   Reports.Assert (Step_Counter = 2);
   Step_Counter := Step_Counter + 1;

   delay Delay_Eat_Time;
   
   Reports.Assert (Step_Counter = 4);
   
   TIO.Put_Line ("Main before calling entry");
   T1.Go;
     
   TIO.Put_Line ("Main before delay");
   Reports.Assert (Step_Counter = 4);
   Step_Counter := Step_Counter + 1;

   delay Delay_Eat_Time * 3;
     
   TIO.Put_Line ("Main after delay");
           
   Reports.Assert (EDF_D_Past_Task_Has_Executed_Eat);
   Reports.Assert (Step_Counter = 8);

   Reports.Test_OK;
         
end Test_EDF_Task_Entry;
