--  Test for all architectures
------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--             'T e s t _ P O _ B r o a d c a s t _ S i g n a l'
--
--                               Ada Program
--
--
--  File 'po_broadcast_signal.adb'                                      By MAR.
--
--  Example taken from John Barnes' book "Programming in Ada"
--  (pg. 432) that implements a broadcast signal using protected
--  entries and requeuing.
--
--  The expected output of program is:
--
--  Waiter  1 waits...   <-- Three tasks wait in entry 'Wait'
--  Waiter  2 waits...
--  Waiter  3 waits...
--  Main task signals PO <--Main task calls 'Signal' and is requeued in 'Reset'
--  Wait                 <-- The three tasks executes body of entry 'Wait'
--  Wait
--  Wait
--  Reset                <-- After that main task executes body of 'Reset'
--  Waiter  3 after wait... <-- Tasks finish execution
--  Waiter  1 after wait...
--  Waiter  2 after wait...
--  Main task signals PO again <-- Main task calls 'Signal' again, since there
--  Main task finishes   <--  are no tasks waiting it finishes immediately
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
-------------------------------------------------------------------------------

with MaRTE_OS;
with Text_IO; use Text_IO;
with Reports;

procedure Test_PO_Broadcast_Signal is
   pragma Priority (10);

   Before_Signal : Boolean := True;
   pragma Volatile (Before_Signal);

   Before_Reset : Boolean := True;
   pragma Volatile (Before_Reset);

   -------------------------
   --  Protected 'Event'  --
   -------------------------
   protected Event is
      entry Wait;
      entry Signal;
   private
      entry Reset;
      Occurred : Boolean := False;
   end Event;

   protected body Event is
      entry Wait when Occurred is
      begin
         Reports.Assert (not Before_Signal and then Before_Reset);
         Put_Line ("Wait");
      end Wait;

      entry Signal when True is
      begin
         Before_Signal := False;
         if Wait'Count > 0 then
            Occurred := true;
            requeue Reset;
         end if;
      end Signal;

      entry Reset when Wait'Count = 0 is
      begin
         Put_Line ("Reset");
         Occurred := False;
         Before_Reset := False;
      end Reset;
   end Event;

   ------------------
   -- Waiter tasks --
   ------------------
   task type Waiter (Id : Integer) is
      pragma Priority (3);
   end Waiter;

   task body Waiter is
   begin
      Reports.Init;
      Reports.Assert (Before_Signal and then Before_Reset);
      Put_Line ("Waiter " & Integer'Image (Id) & " waits...");
      Event.Wait;
      Reports.Assert (not Before_Signal and then not Before_Reset);
      Put_Line ("Waiter " & Integer'Image (Id) & " after wait...");
   end Waiter;

   T1 : Waiter (1);
   T2 : Waiter (2);
   T3 : Waiter (3);


begin
   Reports.Init;
   delay 0.5;
   Put_Line ("Main task signals PO");
   Event.Signal;
   Reports.Assert (not Before_Signal and then not Before_Reset);
   delay 0.5;
   Put_Line ("Main task signals PO again");
   Event.Signal;
   Put_Line ("Main task finishes");
   Reports.Test_OK;
end Test_PO_Broadcast_Signal;

