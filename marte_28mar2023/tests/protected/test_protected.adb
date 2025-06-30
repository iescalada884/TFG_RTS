with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;

procedure Test_Protected is

   protected type Shared_Resource is
      procedure Increment;
      function Get_Value return Integer;
   private
      Counter : Integer := 0;
   end Shared_Resource;

   protected body Shared_Resource is
      procedure Increment is
      begin
         Counter := Counter + 1;
      end Increment;

      function Get_Value return Integer is
      begin
         return Counter;
      end Get_Value;
   end Shared_Resource;

   Resource : Shared_Resource;

   task  Task1;

   task body Task1 is
   begin
      Put_Line("Task 1 started");
      for I in 1 .. 5 loop
         Put_Line("Task 1 incrementing resource");
         --Resource.Increment;
         delay 0.1; 
      end loop;
   end Task1;

   task  Task2;
   task body Task2 is
   begin
      Put_Line("Task 2 started");
      for I in 1 .. 5 loop
         Put_Line("Task 2 incrementing resource");
         --Resource.Increment;
         delay 0.1;
      end loop;
      delay 1.0;
   end Task2;

begin
   Put_Line("Testing Protected Objects with Multiple Tasks in Ada");

   -- Wait for tasks to complete
   delay 5.0;

   -- Test Get_Value Function
   if Resource.Get_Value /= 10 then
      Put_Line("Test Failed: Expected 10, Got " & Integer'Image(Resource.Get_Value));
   else
      Put_Line("Test Passed: Counter is 10");
   end if;

exception
   when E : others =>
      Put_Line("Test Failed: Exception " & Exception_Information(E));
end Test_Protected;
