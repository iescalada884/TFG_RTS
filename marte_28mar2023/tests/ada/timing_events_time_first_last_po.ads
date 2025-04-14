
with Ada.Real_Time.Timing_Events;
with System;

package Timing_Events_Time_First_Last_PO is

   protected PO is

      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler_Never
	(Evento : in out Ada.Real_Time.Timing_Events.Timing_Event);

      procedure Handler_Always
	(Evento : in out Ada.Real_Time.Timing_Events.Timing_Event);

      function Is_Handlers_Execution_OK return Boolean;

   private
      Handler_Never_Executed : Boolean := False;
      Handler_Always_Executed : Boolean := False;
   end PO;

end Timing_Events_Time_First_Last_PO;
