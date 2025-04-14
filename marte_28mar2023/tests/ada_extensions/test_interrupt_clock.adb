--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'T e s t _ I n t e r r u p t _ C l o c k'
--
--                               Ada Program
--
--
--  File 'test_interrupt_clock.adb'                                  By MAR.
--
--  Test effect of long interrupt handlers on task execution time clocks. Uses
--  a periodic timing event with a long execution time to disturb the main
--  task.
--
--  The sum of the time executed by the task plus the time executed by
--  the interrupt handlers must be equal to the elapsed time.
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
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Real_Time.Timing_Events; use Ada.Real_Time.Timing_Events;
with Ada.Execution_Time;
with System;

with PO_Test_Interrupt_Clock;
with Execution_Load_Loop;
with Reports;

procedure Test_Interrupt_Clock is

   use type Ada.Real_Time.Time, Ada.Execution_Time.CPU_Time;
   package PO renames PO_Test_Interrupt_Clock;
   package RT renames Ada.Real_Time;
   package EXETIM renames Ada.Execution_Time;

   --  Test constants

   Handler_Period : constant := 0.2;
   Handler_Eat : constant := 0.1;
   Periods_To_Count : constant := 7;

   --  Timing event

   TE : Ada.Real_Time.Timing_Events.Timing_Event;

   --  Times

   Initial_Time, Final_Time : Ada.Real_Time.Time;
   Total_Time : Duration;
   Initial_Exec_Time, Final_Exec_Time : EXETIM.CPU_Time;
   Total_Exec_Time : Duration;
   Initial_Intr_Time, Final_Intr_Time : EXETIM.CPU_Time;
   Total_Intr_Time : Duration;

   Cancelled : Boolean := False;

begin
   Reports.Init;
   Put_Line ("Test_Interrupt_Clock");

   --  Set handler period and execution time

   PO.PO.Set_Parameters (Handler_Period, Handler_Eat);

   --  Set handler for absolute time

   Ada.Real_Time.Timing_Events.Set_Handler
     (TE,
      Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Handler_Period),
      PO.PO.Handler'Access);

   --  Get inital time

   Initial_Time := RT.Clock;
   Initial_Exec_Time := EXETIM.Clock;
   Initial_Intr_Time := EXETIM.Clock_For_Interrupts;

   --  Eat several handler periods

   Execution_Load_Loop.Eat (Handler_Period * Periods_To_Count);

   --  Cancel handler

   RT.Timing_Events.Cancel_Handler (TE, Cancelled);
   Reports.Assert (Cancelled);

   --  Get final time

   Final_Time := RT.Clock;
   Final_Exec_Time := EXETIM.Clock;
   Final_Intr_Time := EXETIM.Clock_For_Interrupts;
   Total_Time := RT.To_Duration (Final_Time - Initial_Time);
   Total_Exec_Time := RT.To_Duration (Final_Exec_Time - Initial_Exec_Time);
   Total_Intr_Time := RT.To_Duration (Final_Intr_Time - Initial_Intr_Time);

   Put_Line ("Total time:" & Duration'Image (Total_Time));
   Put_Line ("Total exec. time:" & Duration'Image (Total_Exec_Time));
   Put_Line ("Total intr. time:" & Duration'Image (Total_Intr_Time));

   --  Check if he sum of the time executed by the task plus the time executed
   --  by the interrupt handlers must be equal to the elapsed time.

   Reports.Assert
     (abs (Total_Time - Total_Exec_Time - Total_Intr_Time) < 0.01);

   Reports.Test_OK;

end Test_Interrupt_Clock;
