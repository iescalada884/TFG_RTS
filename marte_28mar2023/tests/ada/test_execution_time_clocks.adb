--  Test for all architectures
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);

with Text_IO; use Text_IO;
with Ada.Execution_Time;
with Ada.Execution_Time.Timers;
with Ada.Real_Time;

with Reports;
with Execution_Load_Loop;

procedure Test_Execution_Time_Clocks is
   pragma Priority (12);

   package RT renames Ada.Real_Time;
   package EXETIM renames Ada.Execution_Time;

   use type EXETIM.CPU_Time, RT.Time_Span;

   Task_Loops : constant := 4;
   Consumed_Time : constant := 0.1;
   Error_Margin : constant := Consumed_Time / 3;

   -------------------
   -- Show_CPU_Time --
   -------------------

   procedure Show_CPU_Time (T : EXETIM.CPU_Time; Msg : String) is
      SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span;
   begin
      EXETIM.Split (T, SC, TS);
      Put_Line (Msg & RT.Seconds_Count'Image (SC) & "s" &
                Duration'Image (RT.To_Duration (TS)) & "ns");
   end Show_CPU_Time;

   -------------------------
   -- Check_Dif_In_Margin --
   -------------------------

   procedure Check_Dif_In_Margin (T1 : EXETIM.CPU_Time;
                                  T2 : EXETIM.CPU_Time) is
   begin
      if T1 > T2 then
         if T1 - T2 > RT.To_Time_Span (Error_Margin) then
            Put_Line ("Error: dif:"
                      & Duration'Image (RT.To_Duration (T1 - T2))
                      & " > margen:"
                      & Duration'Image (Error_Margin));
            Reports.Assert (False);
         end if;
      else
         if T2 - T1 > RT.To_Time_Span (Error_Margin) then
            Put_Line ("Error: dif:"
                      & Duration'Image (RT.To_Duration (T2 - T1))
                      & " > margen:"
                      & Duration'Image (Error_Margin));
            Reports.Assert (False);
         end if;
      end if;
   end Check_Dif_In_Margin;

   ------------------------
   -- Consumer task type --
   ------------------------
   task type Consumer (Id : Character) is
      pragma Priority (10);
   end Consumer;

   task body Consumer is
      T, Expected_Time : EXETIM.CPU_Time;
   begin
      --  Show_CPU_Time (EXETIM.Clock, "Initial time task " & Id & ":");

      Expected_Time := EXETIM.Clock;
      for I in 1 .. Task_Loops loop
         T := EXETIM.Clock;
         --  Show_CPU_Time (T, "Task " & Id & " before eat ");

         --  Check if consumed time is OK

         Check_Dif_In_Margin (T, Expected_Time);

         --  Consume CPU time

         Execution_Load_Loop.Eat (Consumed_Time);

         --  Check if consumed time is OK

         T := EXETIM.Clock;
         Expected_Time := Expected_Time + RT.To_Time_Span (Consumed_Time);
         --  Show_CPU_Time (T, "Task " & Id & " after eat ");
         --  Show_CPU_Time (Expected_Time, "Task " & Id & " Expected_Time after eat ");
         Check_Dif_In_Margin (T, Expected_Time);

         --  sleep for a while

         delay Consumed_Time * 1.5;

         --  Check if consumed time is OK

         T := EXETIM.Clock;
         --  Show_CPU_Time (T, "Task " & Id & " after delay ");
         Check_Dif_In_Margin (T, Expected_Time);
      end loop;

      Put_Line ("Consumer task " & Id & " finishes");

   exception
      when others =>
         Put_Line ("Exception caugth");
         Reports.Assert (False);
   end Consumer;

   T1 : Consumer ('A');
   T2 : Consumer ('B');

begin
   Reports.Init;
   Put_Line ("Test_Execution_Time_Clocks");

   --  loop to test execution time accounting in a more complex situation

   for I in 1 .. Task_Loops * 3 loop
      Execution_Load_Loop.Eat (Consumed_Time);
      delay Consumed_Time;
   end loop;

   Put_Line ("Main task finishes");

   Reports.Test_OK;

end Test_Execution_Time_Clocks;
