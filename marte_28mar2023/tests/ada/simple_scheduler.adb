with Text_IO; use Text_IO;
with Reports;

package body Simple_Scheduler is

   ----------------------------------------------------------------------------
   --  Simple Scheduler Operations  -------------------------------------------
   ----------------------------------------------------------------------------

   ------------
   --  Init  --
   ------------
   procedure Init (Sched : out Simple_Scheduler) is
      Mask : Ada.Application_Scheduling.Event_Mask;
      pragma Unreferenced (Sched);
   begin
      Put_Line ("Simple scheduler Init (step:" &
                Integer'Image (Step) & ")");
      Reports.Assert (Step=3);
      Step:=Step+1;
      --  Set event mask
      Ada.Application_Scheduling.Fill (Mask);
      Ada.Application_Scheduling.Delete
        (Ada.Application_Scheduling.New_Task, Mask);
--        Ada.Application_Scheduling.Delete
--          (Ada.Application_Scheduling.Terminate_Task, Mask);
--        Ada.Application_Scheduling.Delete
--          (Ada.Application_Scheduling.Execution_Timer_Expiration, Mask);
--        Ada.Application_Scheduling.Delete
--          (Ada.Application_Scheduling.Task_Notification, Mask);

      Ada.Application_Scheduling.Set_Event_Mask (Mask);
   end Init;

   ----------------
   --  New_Task  --
   ----------------
   procedure New_Task
     (Sched   : in out Simple_Scheduler;
      Tid     : in     Ada.Task_Identification.Task_Id;
      Actions : in out Ada.Application_Scheduling.Scheduling_Actions) is
   begin
      Put_Line ("Simple scheduler New_Task (step:" &
                Integer'Image (Step) & ")");
      Reports.Assert (Step=5);
      Step:=Step+1;
      --  Accept task
      Ada.Application_Scheduling.Add_Accept (Actions, Tid);
      Ada.Application_Scheduling.Add_Ready (Actions, Tid);
   end New_Task;

   -------------
   --  Error  --
   -------------
   procedure Error
     (Sched   : in out Simple_Scheduler;
      Tid     : in     Ada.Task_Identification.Task_Id;
      Cause   : in     Ada.Application_Scheduling.Error_Cause;
      Actions : in out Ada.Application_Scheduling.Scheduling_Actions) is
   begin
      Put_Line ("Simple scheduler Error: " &
                Ada.Application_Scheduling.Error_Cause'Image (Cause));
      Reports.Assert (False);
   end Error;

end Simple_Scheduler;
