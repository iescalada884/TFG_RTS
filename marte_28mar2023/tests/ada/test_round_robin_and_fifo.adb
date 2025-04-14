--  Test for all architectures
------------------------------------------------------------------------------
--
--  FIFO for high priorities and RR for low priority
--
--  Main task at FIFO priority and some task at RR priority
--
--  The test counts the number of context switches (CS) between round robin
--  tasks and compares that number with the expected value (Time for RR
--  priority level/quantum)
--
------------------------------------------------------------------------------
pragma Priority_Specific_Dispatching (Round_Robin_Within_Priorities,
                                      10, 10);
with System;
with Ada.Real_Time;
with Ada.Dispatching.Round_Robin;
with Text_IO; use Text_IO;
with Execution_Load;

with Reports;

procedure Test_Round_Robin_And_FIFO is

   package RT renames Ada.Real_Time;

   RR_Prio : constant System.Any_Priority := 10;
   FIFO_Prio : constant System.Any_Priority := 40;

   Quantum : constant Duration :=
     RT.To_Duration (Ada.Dispatching.Round_Robin.Actual_Quantum (RR_Prio));

   --  Main task at FIFO priority

   pragma Priority (FIFO_Prio);

   --  Execution parameters of main task

   Exec_Time : constant Duration := Quantum * 2.3;
   Delay_Time : constant Duration := Quantum * 3;
   Main_Task_Loops : constant := 10;

   --  Global data

   Number_Of_Tasks : constant := 3;

   Last_Task_Id : Natural := 0;
   pragma Volatile (Last_Task_Id);

   CS_Count : array (1 .. Number_Of_Tasks) of Natural := (others =>0);
   pragma Volatile (CS_Count);
   pragma Volatile_Components (CS_Count);

   Id_Main : constant Natural := 0;  --  Id of main task

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
      Reports.Init;

      while not Start loop  null; end loop;
      loop
         exit when End_Test;
         if Last_Task_Id /= Id then
            --  context switch detected

            if Last_Task_Id /= Id_Main then
               --  doesn't count CS from main task

               CS_Count (Id) := CS_Count (Id) + 1;
               --  Put_Line ("CS in RR task " & Natural'Image (Id));
            end if;

            Last_Task_Id := Id;
         end if;
         --  Put_Line ("RR task " & Natural'Image (Id));
         --  for I in 1 .. 200_000 loop  null; end loop;
      end loop;
   exception
      when E : others =>
         Put_Line ("Exception in RR task " & Natural'Image (Id));
   end Round_Robin_Task;

   T1 : Round_Robin_Task (RR_Prio, 1);
   T2 : Round_Robin_Task (RR_Prio, 2);
   T3 : Round_Robin_Task (RR_Prio, 3);

   CS_Expected, Total_CS_Count : Natural := 0;

begin
   Reports.Init;
   Put_Line ("Test_Round_Robin_And_FIFO");


   --  Execute and leave some time for RR tasks

   Start := True;  --  Start test
   for I in 1 .. Main_Task_Loops loop
      Last_Task_Id := Id_Main;

      Execution_Load.Eat (Exec_Time);

      --  Never a CS in main task

      Reports.Assert (Last_Task_Id = Id_Main);

      --  Allow RR task to execute

      --  Put_Line ("Main calling delay");
      delay Delay_Time;
      --  Put_Line ("Main executing");
   end loop;
   End_Test := True;

   CS_Expected := Integer (Main_Task_Loops * Delay_Time / Quantum);

   --  Show results

   for I in CS_Count'Range loop
      Put_Line ("CS count" & Integer'Image (I) & ":"
                & Natural'Image (CS_Count (I)));
      Total_CS_Count := Total_CS_Count + CS_Count (I);
   end loop;
   Put_Line ("Total CS count:" & Natural'Image (Total_CS_Count));
   Put_Line ("Total expected CS count:" & Natural'Image (CS_Expected));

   --  Allow error of one CS

   for I in CS_Count'First + 1 .. CS_Count'Last loop
      Reports.Assert (abs (CS_Count (I - 1) - CS_Count (I)) <= 1);
   end loop;
   Reports.Assert (abs (CS_Expected-Total_CS_Count) <= 1);

   Reports.Test_OK;

exception
   when E : others =>
      Put_Line ("Exception in main task ");
end Test_Round_Robin_And_FIFO;
