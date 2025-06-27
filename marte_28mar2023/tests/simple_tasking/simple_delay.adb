with Ada.Text_IO; use Ada.Text_IO;

procedure Simple_Delay is

   -- Task declarations
   task Task1;
   --  task Task2;

   --  -- Task bodies

   --  task body Task2 is
   --  begin
   --     loop
   --        Put_Line("Tarea 2");
   --        delay 1.0;
   --     end loop;
   --  end Task2;

   task body Task1 is
   begin
      loop
         Put_Line("Tarea 1");
         delay 0.1;
      end loop;
   end Task1;
begin
loop
   delay 2.0; -- Main loop delay to allow task execution
end loop;
end Simple_Delay; 