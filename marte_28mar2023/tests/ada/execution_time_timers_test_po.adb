
with Text_IO; use Text_IO;
with Execution_Load_Loop;
with Ada.Real_Time;
with Ada.Execution_Time.Timers;

package body Execution_Time_Timers_Test_PO is

   package RT renames Ada.Real_Time;

   protected body PO is

      procedure CPU_Timer_Handler1
        (E : in out Ada.Execution_Time.Timers.Timer) is
         use Ada.Real_Time;
      begin
         Count1 := Count1 + 1;
         Put_Line ("In handler 1");
         Ada.Execution_Time.Timers.Set_Handler (E,
                                                Exec_Time,
                                                CPU_Timer_Handler1'Access);
      end CPU_Timer_Handler1;

      procedure CPU_Timer_Handler2
        (E : in out Ada.Execution_Time.Timers.Timer) is
         use Ada.Real_Time;
      begin
         Count2 := Count2 + 1;
         Put_Line ("In handler 2");
         Ada.Execution_Time.Timers.Set_Handler (E,
                                                Exec_Time,
                                                CPU_Timer_Handler2'Access);
      end CPU_Timer_Handler2;

      procedure PP is
      begin
         Put_Line (" PP starts");
         Execution_Load_Loop.Eat (10.0);
         Put_Line (" PP ends");
      end PP;

      function Get_Count1 return Integer is
      begin
         return Count1;
      end Get_Count1;

      function Get_Count2 return Integer is
      begin
         return Count2;
      end Get_Count2;

      procedure Add_Loop1 is
      begin
         Loops1 := Loops1 + 1;
      end Add_Loop1;

      function Get_Loops1 return Integer is
      begin
         return Loops1;
      end Get_Loops1;

      procedure Add_Loop2 is
      begin
         Loops2 := Loops2 + 1;
      end Add_Loop2;

      function Get_Loops2 return Integer is
      begin
         return Loops2;
      end Get_Loops2;

      procedure Set_Execution_Time (ET : Duration) is
      begin
         Exec_Time := RT.To_Time_Span (ET);
      end Set_Execution_Time;

   end PO;

end Execution_Time_Timers_Test_PO;
