
with Ada.Real_Time.Timing_Events;
with System;

package Timing_Event_And_ATC_PO is

   protected PO is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler (E : in out Ada.Real_Time.Timing_Events.Timing_Event);

      procedure Reset;

      entry Abort_Computation;

   private
      Aborted : Boolean := False;
   end PO;
end Timing_Event_And_ATC_PO;
