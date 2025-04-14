--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--       'T e s t _ E D F _ S u s p e n s i o n _ O b j e c t _ O p e n'
--
--                               Ada Program
--
--
--  File 'test_edf_suspension_object_open.adb'                      By MAR.
--
--  Tests operation 
--  Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline
--  when the suspension object is open and checks the new deadline was
--  set OK.
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
pragma Locking_Policy (Ceiling_Locking);

with Text_IO; use Text_IO;
with Ada.Dispatching.EDF;
with Ada.Synchronous_Task_Control.EDF;
with Ada.Real_Time;

with Execution_Load;
with Reports;

procedure Test_EDF_Suspension_Object_Open is

   package EDF renames Ada.Dispatching.EDF;
   package RT renames Ada.Real_Time;
   package STC renames Ada.Synchronous_Task_Control;

   use type RT.Time;
   
   ------------------
   -- Step_Counter --
   ------------------
   
   Step_Counter : Natural := 0;
   pragma Volatile (Step_Counter);
   
   ------------------------
   -- Suspension Objects --
   ------------------------
   
   Susp_Object_1 : STC.Suspension_Object;   
   Susp_Object_2 : STC.Suspension_Object;
     
   ----------------------
   -- Global constants --
   ----------------------

   Main_Prio : constant := 5;

   Deadline_Short : constant RT.Time_Span := RT.To_Time_Span (20.0);
   Deadline_Long : constant RT.Time_Span := RT.To_Time_Span (40.0);
   
   pragma Priority (Main_Prio);
   pragma Relative_Deadline (Deadline_Long);

   ----------------
   -- EDF_Task_1 --
   ----------------

   task EDF_Task_1 is
      pragma Priority (Main_Prio + 1);
      pragma Relative_Deadline (Deadline_Short);
   end EDF_Task_1;

   task body EDF_Task_1 is
   begin
      Reports.Init;
      
      Put_Line ("EDF_Task_1 before sleeping for a while");  
      delay 1.0;   
      Put_Line ("EDF_Task_1 after sleeping for a while");
      
      Reports.Assert (Step_Counter = 1);
      Step_Counter := Step_Counter + 1;
      
      --  Open suspension object
      
      STC.Set_True (Susp_Object_1);
      
      Reports.Assert (Step_Counter = 2);
      Step_Counter := Step_Counter + 1;
      
      Put_Line ("EDF_Task_1 before Suspend_Until_True");     
      --  Inmediately activated with long deadline
      STC.EDF.Suspend_Until_True_And_Set_Deadline (Susp_Object_1,
						   Deadline_Long);
      Put_Line ("EDF_Task_1 after Suspend_Until_True");
      
      Reports.Assert (Step_Counter = 4);
      Step_Counter := Step_Counter + 1;
      
   exception
      when E : others =>
         Put_Line ("Exception in EDF task 1");
   end EDF_Task_1;

begin
   Reports.Init;
   Put_Line ("Test_EDF_Suspension_Object_Open");
   
   Reports.Assert (Step_Counter = 0);
   Step_Counter := Step_Counter + 1;
             
   Put_Line ("Main before loop");
   while Step_Counter = 1 loop
      null;
   end loop;             
   Put_Line ("Main after loop");
      
   Reports.Assert (Step_Counter = 3);
   Step_Counter := Step_Counter + 1;
     
   Put_Line ("Main before sleeping for a while");  
   delay 1.0;   
   Put_Line ("Main after sleeping for a while");
          
   Reports.Assert (Step_Counter = 5);

   Reports.Test_OK;

end Test_EDF_Suspension_Object_Open;
