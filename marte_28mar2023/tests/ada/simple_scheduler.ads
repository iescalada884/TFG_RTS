with Ada.Application_Scheduling;
with MaRTE.SLL;
with Ada.Task_Identification;

package Simple_Scheduler is

   Step : Integer := 1;
   --  step counter to check everything happens in the appropriate order

   type Simple_Scheduler is
     new Ada.Application_Scheduling.Scheduler with null record;

   procedure Init (Sched : out Simple_Scheduler);

   procedure New_Task
     (Sched   : in out Simple_Scheduler;
      Tid     : in     Ada.Task_Identification.Task_Id;
      Actions : in out Ada.Application_Scheduling.Scheduling_Actions);

--     procedure Terminate_Task
--       (Sched   : in out Simple_Scheduler;
--        Tid     : in     Ada.Task_Identification.Task_Id;
--        Actions : in out Ada.Application_Scheduling.Scheduling_Actions);

--     procedure Execution_Timer_Expiration
--       (Sched         : in out Simple_Scheduler;
--        Expired_Timer : in out Ada.Real_Time.Execution_Time.Timer;
--        Actions       : in out Ada.Application_Scheduling.Scheduling_Actions);
--     --  End of task's budget for this period

--     procedure Task_Notification
--       (Sched   : in out Simple_Scheduler;
--        Tid     : in     Ada.Task_Identification.Task_Id;
--        Actions : in out Ada.Application_Scheduling.Scheduling_Actions);
--     --  Start of new task's period

   procedure Error
     (Sched   : in out Simple_Scheduler;
      Tid     : in     Ada.Task_Identification.Task_Id;
      Cause   : in     Ada.Application_Scheduling.Error_Cause;
      Actions : in out Ada.Application_Scheduling.Scheduling_Actions);

--     --  {Javier} pragma Application_Scheduler
--     --  (Simple_Scheduler, P, Stack_Resource_Policy);

--     procedure Display_Running_Task_Data (Sched : in Simple_Scheduler);

--  private

--     ------------------------------
--     --  Type 'Simple_Task_Data'  --
--     ------------------------------
--     type Simple_Task_Data is tagged record
--        Id         : Natural;  --  For debugging
--        Importance : Task_Importance;
--        Period     : Ada.Real_Time.Time_Span;
--        Ready_Time : Ada.Real_Time.Time;
--        Budget     : Ada.Real_Time.Time_Span;
--        Task_Id    : aliased Ada.Task_Identification.Task_Id;
--        Timer      : access Ada.Real_Time.Execution_Time.Timer;
--     end record;

--     package Task_Data_Lists is new MaRTE.SLL (Simple_Task_Data);
--     subtype Task_Data_Ac is Task_Data_Lists.Element_Ac;

--     ------------------------------
--     --  Type 'Simple_Scheduler'  --
--     ------------------------------
--     type Simple_Scheduler is new Ada.Application_Scheduling.Scheduler
--       with record
--          List             : Task_Data_Lists.List;
--          Start_Time       : Ada.Real_Time.Time;  --  For debugging
--          Total_Importance : Natural;
--     end record;

end Simple_Scheduler;
