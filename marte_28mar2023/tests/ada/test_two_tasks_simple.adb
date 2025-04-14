--  Test for all architectures

--  Test the task with higher priority executes before the main.

pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy(Ceiling_Locking);
pragma Queuing_Policy(Priority_Queuing);
with MaRTE_OS;

with Text_IO; use Text_IO;
with Reports;

procedure Test_Two_Tasks_Simple is

   pragma Priority (10);

   High_Prio_Task_Executing : Boolean := False;
   pragma Volatile (High_Prio_Task_Executing);

   High_Prio_Task_Finished : Boolean := False;
   pragma Volatile (High_Prio_Task_Finished);

   Main_Task_Executing : Boolean := False;
   pragma Volatile (Main_Task_Executing);

   ----------------------
   --  High_Prio_Task  --
   ----------------------
   task High_Prio_Task is
      pragma Priority (20);
   end High_Prio_Task;

   task body High_Prio_Task is
   begin
      High_Prio_Task_Executing := True;

      Reports.Init;

      if Main_Task_Executing then
         Reports.Assert (False);
      end if;

      Text_IO.Put_Line ("High_Prio_Task starting...");
      for I in 1 .. 1_000 loop
         for J in 1 .. 10_000 loop
            null;
         end loop;
      end loop;

      Text_IO.Put_Line ("High_Prio_Task finishing...");
      High_Prio_Task_Finished := True;
   end High_Prio_Task;

begin
   Main_Task_Executing := True;

   Reports.Init;
   Text_IO.Put_Line ("Main_Task starting...");

   if not High_Prio_Task_Finished then
      Text_IO.Put_Line ("Error: high priority task has not executed");
      Reports.Assert (False);
   end if;

   Text_IO.Put_Line ("Main_Task finishing...");
   Reports.Test_OK;

end Test_Two_Tasks_Simple;
