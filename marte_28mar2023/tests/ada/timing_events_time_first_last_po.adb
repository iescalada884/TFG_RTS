
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time;

package body Timing_Events_Time_First_Last_PO is

   protected body PO is

      procedure Handler_Never
	(Evento : in out Ada.Real_Time.Timing_Events.Timing_Event)
      is
      begin
         Ada.Text_IO.Put_Line("Handler_Never");
         Handler_Never_Executed := True;
      end Handler_Never;

      procedure Handler_Always
	(Evento : in out Ada.Real_Time.Timing_Events.Timing_Event)
      is
      begin
         Ada.Text_IO.Put_Line("Handler_Always");
         Handler_Always_Executed := True;
      end Handler_Always;

      function Is_Handlers_Execution_OK return Boolean is
      begin
         return not Handler_Never_Executed and Handler_Always_Executed;
      end Is_Handlers_Execution_OK;

   end PO;

end Timing_Events_Time_First_Last_PO;
