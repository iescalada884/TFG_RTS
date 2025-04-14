--  Test for all architectures
------------------------------------------------------------------------------
--
--  Counts the number of context switches between round robin tasks
--  and compares that number with the expected value
--  (text_duration/quantum)
--
------------------------------------------------------------------------------

--pragma Task_Dispatching_Policy (Round_Robin_Within_Priorities);

pragma Priority_Specific_Dispatching (Round_Robin_Within_Priorities,
                                      0, 20);

with System;
with Ada.Real_Time;
with Ada.Dispatching.Round_Robin;
with Text_IO; use Text_IO;

with Reports;

procedure Test_Round_Robin_Simple is

   package RT renames Ada.Real_Time;

   Tasks_Prio : constant System.Any_Priority := 10;
   Main_Prio : constant System.Any_Priority := 21;

   Quantum : constant Duration :=
     RT.To_Duration (Ada.Dispatching.Round_Robin.Actual_Quantum (Tasks_Prio));

   pragma Priority (Main_Prio);

   Test_Duration : constant Duration := 20 * Quantum;
   Number_Of_Tasks : constant := 2;

   Last_Task_Id : Natural := 0;
   pragma Volatile (Last_Task_Id);

   CS_Count : array (1 .. Number_Of_Tasks) of Natural := (others =>0);
   pragma Volatile (CS_Count);
   pragma Volatile_Components (CS_Count);

   End_Test : Boolean := False;
   pragma Volatile (End_Test);

   Start : Boolean := False;
   pragma Volatile (Start);

   ----------------------
   -- Round_Robin_Task --
   ----------------------
   task type Round_Robin_Task (Prio : System.Priority; Id : Natural) is
      pragma Priority (Prio);
   end Round_Robin_Task;

   task body Round_Robin_Task is
   begin
      Put_Line ("RR task " & Natural'Image (Id) & " starts");
      loop
         exit when End_Test;
         if Last_Task_Id /= Id then
            --  context switch detected
            Last_Task_Id := Id;
            CS_Count (Id) := CS_Count (Id) + 1;
            Put_Line ("RR task " & Natural'Image (Id) & ": CC");
         end if;
         --  Put_Line ("RR task " & Natural'Image (Id));
         --  for I in 1 .. 2_000_000 loop  null; end loop;
      end loop;
      Put_Line ("RR task " & Natural'Image (Id) & " ends");
   exception
      when E : others =>
         Put_Line ("Exception in RR task " & Natural'Image (Id));
   end Round_Robin_Task;

   T1 : Round_Robin_Task (Tasks_Prio, 1);
   T2 : Round_Robin_Task (Tasks_Prio, 2);

   CS_Expected, Total_CS_Count : Natural := 0;

begin
   Reports.Init;
   Put_Line ("Test_Round_Robin_Simple");

   delay Test_Duration;  --  to allow other round-robin task to execute
   Put_Line ("Main ends");

   End_Test := True;

   CS_Expected := Integer (Test_Duration/Quantum);

   --  Show results

   for I in CS_Count'Range loop
      Put_Line ("CS count" & Integer'Image (I) & ":"
                & Natural'Image (CS_Count (I)));
      Total_CS_Count := Total_CS_Count + CS_Count (I);
   end loop;
   Put_Line ("Total CS count:" & Natural'Image (Total_CS_Count));
   Put_Line ("Total expected CS count:" & Natural'Image (CS_Expected));

   --  Allow error of one CS

   for I in 2 .. CS_Count'Last loop
      Reports.Assert (abs (CS_Count (I - 1) - CS_Count (I)) <= 1);
   end loop;
   Reports.Assert (abs (CS_Expected-Total_CS_Count) <= 1);

   Reports.Test_OK;

exception
   when E : others =>
      Put_Line ("Exception in main task ");
end Test_Round_Robin_Simple;
