--  Test for all architectures

pragma Task_Dispatching_Policy (FIFO_Within_Priorities);

with MaRTE_OS;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Reports;

procedure Test_Exceptions_In_Task is

   pragma Priority (0);
   
   E : exception;
   
   Task_1_Finished : Boolean := False;
   pragma Volatile (Task_1_Finished);
   
   task Task_1  is
      pragma Priority (16);
   end Task_1;
   
   task body Task_1 is
      procedure Q is
      begin
	 Put_Line ("raise E");
	 raise E;
      end Q;

      procedure P is
      begin
	 Q;
	 Reports.Assert (False);
      exception
	 when E =>
	    Put_Line ("OK: caught E in P");
	    raise Program_Error;
	 when X : others =>
	    Put_Line ("caught " & Exception_Name (X) & "P");
	    Reports.Assert (False);
      end P;

   begin
      Reports.Init;
      P;
      Reports.Assert (False);
   exception
      when E =>
	 Put_Line ("caught E in task's body");
	 Reports.Assert (False);
	 
      when Program_Error =>	 
	 Put_Line ("OK: caught Program_Error in task's body");
	 Task_1_Finished := True;
	 
      when X : others =>
	 Put_Line ("caught " & Exception_Name (X)
		     & " in task's body");
	 Reports.Assert (False);
   end Task_1;
   
begin
   loop
      exit when Task_1_Finished;
   end loop;
     
   Reports.Test_OK;
end Test_Exceptions_In_Task;
