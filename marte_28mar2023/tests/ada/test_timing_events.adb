--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                  'T e s t _ T i m i n g _ E v e n t s'
--
--                               Ada Program
--
--
--  File 'test_timing_events.adb'                                   By MAR.
--
--  Test the Timing Events.
--
--  A timing event is set. First to expire just once and then as a periodic
--  handler (the handler set the timing event again).
--
--  The main procedure checks if the number of expirations is the expected.
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
with System;

with Timing_Events_PO;
with Execution_Load;
with Reports;

procedure Test_Timing_Events is

   use type Ada.Real_Time.Time;
   package PO renames Timing_Events_PO;

   Handler_Period : constant := 0.2;
   Periods_To_Count : constant := 7;

   TE : Ada.Real_Time.Timing_Events.Timing_Event;

begin
   Reports.Init;
   Put_Line ("Test_Timing_Events");

   --  Set handler for absolute time

   Ada.Real_Time.Timing_Events.Set_Handler
     (TE,
      Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Handler_Period),
      PO.PO.Handler'Access);

   delay Handler_Period * 3;

   --  Check event isn't set and has expired only once

   Reports.Assert (TE.Time_Of_Event = Ada.Real_Time.Time_First);
   Reports.Assert (PO.PO.Handler_Count = 1);

   --  Set handler for relative time. Handler is periodic from this point

   PO.PO.Set_Period (Handler_Period);

   Ada.Real_Time.Timing_Events.Set_Handler
     (TE,
      Ada.Real_Time.To_Time_Span (Handler_Period),
      PO.PO.Handler'Access);

   --  Sleep for several handler periods

   delay Handler_Period * Periods_To_Count + Handler_Period * 0.5;

   --  Check event is set and the number of expirations is correct

   Reports.Assert (TE.Time_Of_Event /= Ada.Real_Time.Time_First);
   Reports.Assert
     (abs (PO.PO.Handler_Count - (Periods_To_Count + 1)) = 0);

   --  Eat several handler periods

   Execution_Load.Eat (Handler_Period * Periods_To_Count);

   --  Check event is set and the number of expirations is correct

   Reports.Assert (TE.Time_Of_Event /= Ada.Real_Time.Time_First);
   Reports.Assert
     (abs (PO.PO.Handler_Count - (Periods_To_Count * 2 + 1)) <= 1);

   Put_Line ("MAR: Mutual exclusion: check not done!!");

   Reports.Test_OK;

end Test_Timing_Events;
