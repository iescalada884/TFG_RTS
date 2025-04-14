
with Ada.Real_Time.Timing_Events;
with System;

package Timing_Events_PO is

   protected PO is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler (E : in out Ada.Real_Time.Timing_Events.Timing_Event);
      procedure PP;
      function Handler_Count return Integer;
      procedure Set_Period (P : Duration);

   private
      Count : Integer := 0;
      Periodic : Boolean := False;
      Period : Ada.Real_Time.Time_Span;
   end PO;
end Timing_Events_PO;
