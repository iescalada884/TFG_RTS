
with Ada.Real_Time.Timing_Events;
with System;

package PO_Test_Interrupt_Clock is

   protected PO is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler (E : in out Ada.Real_Time.Timing_Events.Timing_Event);
      function Handler_Count return Integer;
      procedure Set_Parameters (P : Duration; Eat : Duration);

   private
      Count : Integer := 0;
      Period : Ada.Real_Time.Time_Span;
      Exec_Time : Duration;
   end PO;
end PO_Test_Interrupt_Clock;
