--  Test for all architectures
------------------------------------------------------------------------------
-- -------------------        M a R T E     O S        -------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--             'T e s t _ T i m i n g _ E v e n t _ A n d _ A T C'
--
--                               Ada Program
--
--
--  File 'test_timing_event_and_atc.adb'                              By MAR.
--
--  Test the ATC produced by a timing event on the same task that is executing
--  the interrupt.
--
--  The task issues a selec-then-abort with the select alternative guarded
--  by a protected entry.
--
--  The abortable part programs a timing event and executes longer than the
--  event expiration time. The timing event is served in the context of the
--  task and it opens the entry. So the run-time perform a long jump during
--  the timing event execution that skips the final part of the interrupt
--  handler. This problem has been solved calling procedure SCHD.Before_ATC.
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

with Timing_Event_And_ATC_PO;
with Execution_Load;
with Reports;

procedure Test_Timing_Event_And_ATC is

   use type Ada.Real_Time.Time;
   package PO renames Timing_Event_And_ATC_PO;

   Handler_Rel_Time : constant := 0.5;

   TE : Ada.Real_Time.Timing_Events.Timing_Event;
   --  Initial_Time : Time;

begin
   Reports.Init;
   Put_Line ("Test_Timing_Event_And_ATC");
   --  Initial_Time := Clock;

   --  select then abort: abort condition is open by the handler

   for I in 1 .. 2 loop

      PO.PO.Reset;

      select
         PO.PO.Abort_Computation;
         Put_Line ("Computation aborted");
         --Duration'Image (Clock - Initial_Time));

      then abort
         --  Arm timing event

         Ada.Real_Time.Timing_Events.Set_Handler
           (TE,
            Ada.Real_Time.To_Time_Span (Handler_Rel_Time),
            PO.PO.Handler'Access);

         --  Perform computation

         Put_Line ("Computation starts");
         Execution_Load.Eat (Handler_Rel_Time * 3.0);
         Put_Line ("Computation ends");
         Reports.Assert (False);
      end select;

   end loop;

   Reports.Test_OK;

end Test_Timing_Event_And_ATC;
