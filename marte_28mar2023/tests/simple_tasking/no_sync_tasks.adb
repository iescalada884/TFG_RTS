with Ada.Text_IO; use Ada.Text_IO;
with System;

procedure No_Sync_Tasks is

   task Task1;
   task Task2;

   task body Task1 is
   begin
      Put_Line("Hola desde la tarea 1");
   end Task1;

   task body Task2 is
   begin
      Put_Line("Hola desde la segunda tarea de MarteOS");
   end Task2;

begin
   New_Line;
   Put ("Lanzadas 2 tareas independientes: ");
   New_Line (2);

end No_Sync_Tasks;
