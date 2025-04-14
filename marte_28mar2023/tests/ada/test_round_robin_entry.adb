--  Test for all architectures
------------------------------------------------------------------------------
--
--  Check a task that has exhausted it budget while executing an entry is not
--  moved to the tail of the ready queue for its priority level until the entry
--  finishes. (RM D.2.5 (14/2))
--
--  The acceptor task, the caller task an a third task share the same
--  priority level. The acceptor spends more than a quantum in the
--  entry but it is not re-queued until it finishes.
--
------------------------------------------------------------------------------

pragma Priority_Specific_Dispatching (Round_Robin_Within_Priorities, 10, 10);
pragma Locking_Policy(Ceiling_Locking);
pragma Queuing_Policy(Priority_Queuing);

with System;
with Ada.Real_Time;
with Ada.Dispatching.Round_Robin;
with Ada.Dynamic_Priorities;
with Ada.Task_Identification;
with Text_IO; use Text_IO;
with Execution_Load;

with Reports;

procedure Test_Round_Robin_Entry is

   package RT renames Ada.Real_Time;
   package TI renames Ada.Task_Identification;
   use type Ada.Task_Identification.Task_Id;

   Prio : constant System.Any_Priority := 10;

   pragma Priority (Prio + 1);

   Quantum : constant Duration :=
     RT.To_Duration (Ada.Dispatching.Round_Robin.Actual_Quantum (Prio));

   --  Global data

   Last_Task_Id : TI.Task_Id := TI.Null_Task_Id;
   pragma Volatile (Last_Task_Id);

   Finished_Count : Natural := 0;
   pragma Volatile (Finished_Count);

   -----------------
   -- Caller_Task --
   -----------------

   task Caller_Task is
      pragma Priority (Prio);
   end Caller_Task;

   -------------------
   -- Acceptor_Task --
   -------------------

   task Acceptor_Task is
      pragma Priority (Prio);
      entry E1;
   end Acceptor_Task;

   task body Acceptor_Task is
   begin
      Put_Line ("   Acceptor_Task starts");
      accept E1 do
         Put_Line ("   Acceptor_Task:in E1 ");

         Reports.Assert (Last_Task_Id /= TI.Current_Task);
         --  Reports.Assert (Last_Task_Id = Caller_Task'Identity);
         Last_Task_Id := TI.Current_Task;

         Execution_Load.Eat (3.5 * Quantum); -- quantum expires

         Reports.Assert (Last_Task_Id = TI.Current_Task);

         Put_Line ("   Acceptor_Task: leaves E1 ");
      end E1;

      Execution_Load.Eat (0.8 * Quantum); -- quantum expires

      Reports.Assert (Last_Task_Id /= TI.Current_Task);

      Put_Line ("   Acceptor_Task ends");
      Finished_Count := Finished_Count + 1;

   exception
      when others =>
         Put_Line ("Exception in Acceptor_Task");
         Reports.Assert (False);
   end Acceptor_Task;

   ----------------------
   -- Caller_Task body --
   ----------------------

   task body Caller_Task is
   begin
      Put_Line (" Caller_Task starts");

      delay 0.5; --  To give time to 'Acceptor_Task' to reach the accept

      Last_Task_Id := TI.Current_Task;

      pragma Debug (Put_Line (" Caller_Task:before E1 "));
      Acceptor_Task.E1;
      pragma Debug (Put_Line (" Caller_Task:after E1 "));

      Reports.Assert (Last_Task_Id /= TI.Current_Task);

      Put_Line (" Caller_Task ends");
      Finished_Count := Finished_Count + 1;

   exception
      when others =>
         Put_Line ("Exception in Acceptor_Task");
         Reports.Assert (False);
   end Caller_Task;

   ----------------------
   -- Round_Robin_Task --
   ----------------------

   task Round_Robin_Task is
      pragma Priority (Prio);
   end Round_Robin_Task;

   task body Round_Robin_Task is
   begin
      Put_Line ("  Round_Robin_Task starts");
      select
         delay 3.0;
         Put_Line ("  Round_Robin_Task ends delay");
      then abort
         loop
            pragma Debug (Put_Line ("  Round_Robin_Task"));
            Last_Task_Id := TI.Current_Task;

            Execution_Load.Eat (0.1 * Quantum); -- quantum expires
         end loop;
      end select;

      Put_Line ("  Round_Robin_Task ends");

      --  The two other tasks should have finished already

      Reports.Assert (Finished_Count = 2);

      Reports.Test_OK;

   exception
      when others =>
         Put_Line ("Exception in RR task");
         Reports.Assert (False);
   end Round_Robin_Task;

begin
   Reports.Init;

   null;

end Test_Round_Robin_Entry;
