--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--      'T e s t _ E D F _ S u s p e n s i o n _ O b j e c t _ S i m p l e'
--
--                               Ada Program
--
--
--  File 'test_edf_suspension_object_simple.adb'                      By MAR.
--
--  Tests operation 
--  Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline.
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
with Ada.Synchronous_Task_Control.EDF;
with Ada.Real_Time;

with Execution_Load;
with Reports;

procedure Test_EDF_Suspension_Object_Simple is

   package EDF renames Ada.Dispatching.EDF;
   package RT renames Ada.Real_Time;
   package STC renames Ada.Synchronous_Task_Control;

   use type RT.Time;
   
   -----------------------
   -- Suspension Object --
   -----------------------
   
   Susp_Object : STC.Suspension_Object;
     
   ----------------------
   -- Global constants --
   ----------------------

   Main_Prio : constant := 6;

   pragma Priority (Main_Prio);

   Deadline : constant RT.Time_Span := RT.To_Time_Span (2.0);

   --------------
   -- EDF_Task --
   --------------

   task EDF_Task is
      pragma Priority (Main_Prio + 1);
      pragma Relative_Deadline (Deadline);
   end EDF_Task;

   task body EDF_Task is
   begin
      Put_Line ("EDF_Task before suspended in Suspension Object");

      STC.Suspend_Until_True (Susp_Object);
      
      Put_Line ("EDF_Task after suspended in Suspension Object");
      
   exception
      when E : others =>
         Put_Line ("Exception in EDF task");
   end EDF_Task;

begin
   Reports.Init;
   Put_Line ("Test_EDF_Deadlines");
     
   Put_Line ("Main before Set_True");

   STC.Set_True (Susp_Object);
     
   Put_Line ("Main after Set_True");

   Reports.Test_OK;

end Test_EDF_Suspension_Object_Simple;
