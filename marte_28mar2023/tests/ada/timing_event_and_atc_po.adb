
with Text_IO; use Text_IO;

package body Timing_Event_And_ATC_PO is

   protected body PO is

      procedure Handler
        (E : in out Ada.Real_Time.Timing_Events.Timing_Event) is
      begin
         Put_Line ("In handler");
         Aborted := True;
      end Handler;

      procedure Reset is
      begin
         Aborted := False;
      end Reset;

      entry Abort_Computation when Aborted is
      begin
         Put_Line (" PP Abort_Computation");
      end Abort_Computation;
   end PO;
end Timing_Event_And_ATC_PO;
