--  Test for all architectures
------------------------------------------------------------------------------
--
--  Check a task that has exhausted it budget is not moved to the tail of the
--  ready queue for its priority level while executing a protected operation
--  (RM D.2.5 (14/2))
--
--  Two task share the same priority. One of then executes a protected
--  operation and the test checks the context switch never happens during that
--  protected operation
--
--  Quantum or execution load doesn't works well. When it works this will
--  replace Test_Round_Robin_PO.adb
--
------------------------------------------------------------------------------

pragma Task_Dispatching_Policy (Round_Robin_Within_Priorities);
--pragma Priority_Specific_Dispatching (Round_Robin_Within_Priorities, 10, 10);
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

procedure Test_Round_Robin_PO_Fails is

   package RT renames Ada.Real_Time;
   package TI renames Ada.Task_Identification;
   use type Ada.Task_Identification.Task_Id;

   Prio : constant System.Any_Priority := 10;

   pragma Priority (Prio + 1); -- main starts been FIFO

   Quantum : constant Duration :=
     RT.To_Duration (Ada.Dispatching.Round_Robin.Actual_Quantum (Prio));

   --  Global data

   Last_Task_Id : TI.Task_Id := TI.Null_Task_Id;
   pragma Volatile (Last_Task_Id);

   Finished_Protected_Operation : Boolean := False;
   pragma Volatile (Finished_Protected_Operation);

   Main_Task_Id : TI.Task_Id := TI.Null_Task_Id;

   ----------------------
   -- Round_Robin_Task --
   ----------------------

   task Round_Robin_Task is
      pragma Priority (Prio);
   end Round_Robin_Task;

   ----------------------
   -- Protected_Object --
   ----------------------

   protected Protected_Object is
      pragma Priority (Prio);

      procedure Protected_Procedure (Exec_Time : Duration);

   end Protected_Object;

   protected body Protected_Object is

      procedure Protected_Procedure (Exec_Time : Duration) is
      begin
         --  Put_Line ("In Protected_Procedure");
         Reports.Assert (Last_Task_Id = Round_Robin_Task'Identity);
         Execution_Load.Eat (Exec_Time);
         Reports.Assert (Last_Task_Id = Round_Robin_Task'Identity);
         Finished_Protected_Operation := True;
         --  Put_Line ("  Leaving Protected_Procedure");
      end Protected_Procedure;

   end Protected_Object;

   ---------------------------
   -- Round_Robin_Task body --
   ---------------------------

   task body Round_Robin_Task is
   begin
      --  Executes after main

      Put_Line ("Round_Robin_Task: Executes after main");
      Reports.Assert (Last_Task_Id = Main_Task_Id);
      Last_Task_Id := TI.Current_Task;

      --  Executes more than a quantum

      Execution_Load.Eat (1.5 * Quantum); -- quantum expires

      --  Executes more than a quantum in a protected operation

      Put_Line ("Round_Robin_Task: Executes protected operation");
      Reports.Assert (Last_Task_Id = Main_Task_Id);
      Last_Task_Id := TI.Current_Task;
      Protected_Object.Protected_Procedure (40.0 * Quantum);

      --  Just after leaving the protected operation the context switch is
      --  preformed

      Put_Line ("Round_Robin_Task: after leaving the protected operation");
      Reports.Assert (Last_Task_Id = Main_Task_Id);
      Last_Task_Id := TI.Current_Task;

   exception
      when E : others =>
         Put_Line ("Exception in RR task");
         Reports.Assert (False);
   end Round_Robin_Task;

begin
   Reports.Init;
   Put_Line ("Test_Round_Robin_PO 1234");
   Main_Task_Id := TI.Current_Task;

   --  Check this is the first task to execute

   Reports.Assert (Last_Task_Id = TI.Null_Task_Id);
   Last_Task_Id := TI.Current_Task;

   --  Lowers its priority becoming a RR task

   Put_Line ("Main lowers its priority becoming a RR task");
   Ada.Dynamic_Priorities.Set_Priority (Prio);
   Reports.Assert (Last_Task_Id = Round_Robin_Task'Identity);

   --  Executes after quantum of Round_Robin_Task expires

   Put_Line ("Main executes after quantum of Round_Robin_Task expires");
   Reports.Assert (not Finished_Protected_Operation);
   Reports.Assert (Last_Task_Id = Round_Robin_Task'Identity);
   Last_Task_Id := TI.Current_Task;
   Execution_Load.Eat (1.8 * Quantum); -- quantum expires

   --  Executes after Round_Robin_Task has finished the protected operation

   Put_Line ("Main executes after protected operation");
   Reports.Assert (Finished_Protected_Operation);
   Reports.Assert (Last_Task_Id = Round_Robin_Task'Identity);
   Last_Task_Id := TI.Current_Task;

   Execution_Load.Eat (Quantum); -- quantum expires

   Reports.Assert (Last_Task_Id = Round_Robin_Task'Identity);

   Reports.Test_OK;

exception
   when E : others =>
      Put_Line ("Exception in main task ");
      Reports.Assert (False);
end Test_Round_Robin_PO_Fails;
