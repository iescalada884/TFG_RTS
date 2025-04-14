
with Text_IO; use Text_IO;
with Execution_Load_loop;
with Ada.Real_Time;

package body PO_Test_Interrupt_Clock is

   protected body PO is

      procedure Handler
        (E : in out Ada.Real_Time.Timing_Events.Timing_Event) is
         use Ada.Real_Time;
      begin
         Put_Line ("In handler");
         Count := Count + 1;

         Execution_Load_Loop.Eat (Exec_Time);

         --  periodic reprogramming after first expiration

         Ada.Real_Time.Timing_Events.Set_Handler
           (E,
            Period,
            Handler'Access);
      end Handler;

      function Handler_Count return Integer is
      begin
         return Count;
      end Handler_Count;

      procedure Set_Parameters (P : Duration; Eat : Duration) is
      begin
         Period := Ada.Real_Time.To_Time_Span (P);
         Exec_Time := Eat;
      end Set_Parameters;
   end PO;
end PO_Test_Interrupt_Clock;
