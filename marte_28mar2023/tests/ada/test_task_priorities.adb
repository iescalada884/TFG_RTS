--  Test for all architectures
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy(Ceiling_Locking);
pragma Queuing_Policy(Priority_Queuing);
with MaRTE_OS;

with Ada.Real_Time; use Ada.Real_Time;
with System;
with Text_IO; use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar;
with Execution_Load;
with Reports;

procedure Test_Task_Priorities is

   pragma Priority (0);

   High_Prio_Task_Executing : Boolean := False;
   pragma Volatile (High_Prio_Task_Executing);

   High_Prio_Task_Finished : Boolean := False;
   pragma Volatile (High_Prio_Task_Finished);

   Low_Prio_Task_Finished : Boolean := False;
   pragma Volatile (Low_Prio_Task_Finished);

   Error_Detected : Boolean := False;
   pragma Volatile (Error_Detected);

   ----------------------
   --  High_Prio_Task  --
   ----------------------
   task High_Prio_Task is
      pragma Priority (20);
   end High_Prio_Task;

   task body High_Prio_Task is
      Next_Activation : Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Period : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.To_Time_Span (0.2);
   begin
      Reports.Init;
      Text_IO.Put_Line ("Test starting. It will take a few sec...");
      for I in 1 .. 3 loop
         High_Prio_Task_Executing := True;
         Execution_Load.Eat (0.15);
         High_Prio_Task_Executing := False;

         Next_Activation := Next_Activation + Period;
         delay until Next_Activation;
      end loop;
      High_Prio_Task_Finished := True;
   end High_Prio_Task;

   ---------------------
   --  Low_Prio_Task  --
   ---------------------
   task Low_Prio_Task is
      pragma Priority (10);
   end Low_Prio_Task;

   task body Low_Prio_Task is
   begin
      Reports.Init;
      loop
         Error_Detected := High_Prio_Task_Executing;
         exit when High_Prio_Task_Finished or Error_Detected;
      end loop;
      Low_Prio_Task_Finished := True;
   end Low_Prio_Task;

begin
   Reports.Init;
   if (High_Prio_Task_Finished and Low_Prio_Task_Finished and
       not Error_Detected) then
      null;
      Reports.Test_OK;
   else
      if not (High_Prio_Task_Finished and Low_Prio_Task_Finished) then
         Text_IO.Put_Line ("Error: main procedure with low priority " &
                           "executed before high priority task finishes");
         Text_IO.Put_Line ("Waiting for task to finish...");
         loop
            exit when High_Prio_Task_Finished and Low_Prio_Task_Finished;
            Reports.Assert (False);
         end loop;
      end if;
      if Error_Detected then
         Text_IO.Put_Line ("Error: high priority task has been preempted " &
                           "by low priority task");
         Reports.Assert (False);
      end if;
   end if;

end Test_Task_Priorities;
