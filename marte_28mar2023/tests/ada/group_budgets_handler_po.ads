
with Ada.Execution_Time.Group_Budgets;
with Ada.Real_Time;
with System;

package Group_Budgets_Handler_PO is

   package GBs renames Ada.Execution_Time.Group_Budgets;

   protected PO is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure Handler (GB : in out GBs.Group_Budget);
      procedure Periodic_Handler (GB : in out GBs.Group_Budget);
      procedure PP;
      function Handler_Count return Integer;

      procedure Set_Period (ET : Duration);

   private
      Count : Integer := 0;

      Period : Ada.Real_Time.Time_Span;
   end PO;

end Group_Budgets_Handler_PO;
