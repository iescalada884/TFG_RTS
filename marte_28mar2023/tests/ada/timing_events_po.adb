
with Text_IO; use Text_IO;
--  with Execution_Load;
with Ada.Real_Time;

package body Timing_Events_PO is

   protected body PO is

      procedure Handler
        (E : in out Ada.Real_Time.Timing_Events.Timing_Event) is
         use Ada.Real_Time;
      begin
         Put_Line ("In handler");
         Count := Count + 1;
         if Periodic then
            --  periodic reprogramming after first expiration
            Ada.Real_Time.Timing_Events.Set_Handler
              (E,
               Period,
               Handler'Access);
         end if;
      end Handler;

      procedure PP is
      begin
         Put_Line (" PP starts");
         --  Execution_Load.Eat (10.0);
         for I in Integer'Range loop
            null;
         end loop;
         Put_Line (" PP ends");
      end PP;

      function Handler_Count return Integer is
      begin
         return Count;
      end Handler_Count;

      procedure Set_Period (P : Duration) is
      begin
         Periodic := True;
         Period := Ada.Real_Time.To_Time_Span (P);
      end Set_Period;
   end PO;
end Timing_Events_PO;
