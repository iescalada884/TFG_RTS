
with Ada.Execution_Time.Timers;
with Ada.Real_Time;
with System;

package Execution_Time_Timers_Test_PO is

   protected PO is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);

      procedure CPU_Timer_Handler1 (E : in out Ada.Execution_Time.Timers.Timer);

      procedure CPU_Timer_Handler2 (E : in out Ada.Execution_Time.Timers.Timer);

      procedure PP;

      function Get_Count1 return Integer;

      function Get_Count2 return Integer;

      procedure Add_Loop1;

      function Get_Loops1 return Integer;

      procedure Add_Loop2;

      function Get_Loops2 return Integer;

      procedure Set_Execution_Time (ET : Duration);

   private
      Count1 : Integer := 0;
      Count2 : Integer := 0;

      Loops1 : Integer := 0;
      Loops2 : Integer := 0;

      Exec_Time : Ada.Real_Time.Time_Span;
   end PO;

end Execution_Time_Timers_Test_PO;
