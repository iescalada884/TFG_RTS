
with Ada.Execution_Time.Timers;
with System;

package Execution_Time_Clocks_Test_PO is

   protected PO is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure CPU_Timer_Handler (E : in out Ada.Execution_Time.Timers.Timer);
      procedure PP;
   end PO;
   
end Execution_Time_Clocks_Test_PO;
