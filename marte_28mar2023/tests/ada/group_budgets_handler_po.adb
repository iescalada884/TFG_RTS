
with Text_IO; use Text_IO;
--  with Execution_Load;

package body Group_Budgets_Handler_PO is

   protected body PO is

      procedure Handler (GB : in out GBs.Group_Budget) is
      begin
         Put_Line ("In handler");
         Count := Count + 1;
      end Handler;

      procedure Periodic_Handler (GB : in out GBs.Group_Budget) is
      begin
         Put_Line ("In Periodic handler");
         Count := Count + 1;

         --  Reprogram timer

         GBs.Replenish (GB, Period);
         --  GB.Replenish (Period);    Doesn't compile with GPL-10 !!!!!!
      end Periodic_Handler;

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

      procedure Set_Period (ET : Duration) is
      begin
         Period := Ada.Real_Time.To_Time_Span (ET);
      end Set_Period;
   end PO;
end Group_Budgets_Handler_PO;
