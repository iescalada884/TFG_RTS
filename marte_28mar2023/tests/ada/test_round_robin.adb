--  Test for all architectures
------------------------------------------------------------------------------
--
--  Counts the number of context switches between round robin tasks
--  and compares that number with the expected value
--  (text_duration/quantum)
--
------------------------------------------------------------------------------

pragma Task_Dispatching_Policy (Round_Robin_Within_Priorities);

with System;
with Ada.Real_Time;
with Ada.Dispatching.Round_Robin;
with Text_IO; use Text_IO;

with Reports;

procedure Test_Round_Robin is

   package RT renames Ada.Real_Time;

   Prio : constant System.Any_Priority := 10;

   Quantum : constant Duration :=
     RT.To_Duration (Ada.Dispatching.Round_Robin.Actual_Quantum (Prio));

   pragma Priority (Prio);

   Test_Duration : constant Duration := 30.4 * Quantum;
   Number_Of_Tasks : constant := 3;

   Last_Task_Id : Natural := 0;
   pragma Volatile (Last_Task_Id);

   CS_Count : array (1 .. Number_Of_Tasks) of Natural := (others =>0);
   pragma Volatile (CS_Count);
   pragma Volatile_Components (CS_Count);

   Main_CS_Count : Natural := 0;
   pragma Volatile (Main_CS_Count);

   End_Test : Boolean := False;
   pragma Volatile (End_Test);

   Start : Boolean := False;
   pragma Volatile (Start);

   Start_Time : RT.Time;
   Last_Time : RT.Time;

   function Time_From_Start return String is
      use type RT.Time;
      Now : constant RT.Time := RT.Clock;
      Dif_With_Last : constant RT.Time_Span := Now - Last_Time;
   begin
      Last_Time := Now;
      return Duration'Image (RT.To_Duration (Now - Start_Time)) &
        "(dif:" &  Duration'Image (RT.To_Duration (Dif_With_Last)) & ")";
   end Time_From_Start;

   ----------------------
   -- Round_Robin_Task --
   ----------------------
   task type Round_Robin_Task (Prio : System.Priority; Id : Natural) is
      pragma Priority (Prio);
   end Round_Robin_Task;

   task body Round_Robin_Task is
   begin
      Reports.Init;
      Put_Line ("Start RR task " & Natural'Image (Id));
      --  delay 0.0;  --  to allow other round-robin tasks to start
      while not Start loop  null; end loop;
      loop
         exit when End_Test;
         if Last_Task_Id /= Id then
            --  context switch detected
            Last_Task_Id := Id;
            CS_Count (Id) := CS_Count (Id) + 1;
            Put_Line ("RR task " & Natural'Image (Id) & ": CC at: " &
                        Time_From_Start);
         end if;
         --  Put_Line ("RR task " & Natural'Image (Id));
         --  for I in 1 .. 200_000 loop  null; end loop;
      end loop;
   exception
      when E : others =>
         Put_Line ("Exception in RR task " & Natural'Image (Id));
   end Round_Robin_Task;

   T1 : Round_Robin_Task (Prio, 1);
   T2 : Round_Robin_Task (Prio, 2);
   T3 : Round_Robin_Task (Prio, 3);

   Id : constant Natural := 0;

   CS_Expected, Total_CS_Count : Natural := 0;

begin
   Reports.Init;
   Put_Line ("Test_Round_Robin");

   --  delay 0.0;  --  to allow other round-robin task to start
   Put_Line ("Main");

   select
      delay Test_Duration;
      Put_Line ("Main: End_Test");
      End_Test := True;
   then abort
      Start_Time := RT.Clock;
      Last_Time := Start_Time;

      Start := True;  --  Start test
      loop
         if Last_Task_Id /= Id then
            --  context switch detected
            Last_Task_Id := Id;
            Main_CS_Count := Main_CS_Count + 1;
            Put_Line ("Main: CC at: " & Time_From_Start);
         end if;
         --  Put_Line ("Main task");
         --  for I in 1 .. 400_000 loop null; end loop;
      end loop;
   end select;

   CS_Expected := Integer (Test_Duration/Quantum);

   --  Show results

   Put_Line ("Main CS count:" & Natural'Image (Main_CS_Count));
   Total_CS_Count := Main_CS_Count;
   for I in CS_Count'Range loop
      Put_Line ("CS count" & Integer'Image (I) & ":"
                & Natural'Image (CS_Count (I)));
      Total_CS_Count := Total_CS_Count + CS_Count (I);
   end loop;
   Put_Line ("Total CS count:" & Natural'Image (Total_CS_Count));
   Put_Line ("Total expected CS count:" & Natural'Image (CS_Expected));

   --  Allow error of one CS

   for I in CS_Count'Range loop
      Reports.Assert (abs (Main_CS_Count - CS_Count (I)) <= 1);
   end loop;
   Reports.Assert (abs (CS_Expected-Total_CS_Count) <= 1);

   Reports.Test_OK;

exception
   when E : others =>
      End_Test := True;
      Put_Line ("Exception in main task ");
end Test_Round_Robin;
