
with Text_IO; use Text_IO;
with Execution_Load_Loop;
with Ada.Real_Time;
with Ada.Execution_Time.Timers;

package body Execution_Time_Clocks_Test_PO is

   package RT renames Ada.Real_Time;

   protected body PO is

      procedure CPU_Timer_Handler
        (E : in out Ada.Execution_Time.Timers.Timer) is
         use Ada.Real_Time;
      begin
         Put_Line ("In handler");
         Ada.Execution_Time.Timers.Set_Handler (E,
                                                Ada.Execution_Time.Time_Of (1, RT.To_Time_Span (0.5)),
                                                --RT.To_Time_Span (1.5),
                                                CPU_Timer_Handler'Access);
      end CPU_Timer_Handler;

      procedure PP is
      begin
         Put_Line (" PP starts");
         Execution_Load_Loop.Eat (10.0);
         Put_Line (" PP ends");
      end PP;

   end PO;

end Execution_Time_Clocks_Test_PO;
