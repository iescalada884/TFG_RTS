with Ada.Text_IO; use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Delay_Until is

   -- Task declarations
   task Task1;

   task body Task1 is
      Period : constant Time_Span := To_Time_Span(1.0);
      Next_Release : Time := Clock;
   begin
      loop
         Put_Line("Tarea 1");
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Task1;
begin
loop
   delay 2.0; -- Main loop delay to allow task execution
end loop;
end Delay_Until;