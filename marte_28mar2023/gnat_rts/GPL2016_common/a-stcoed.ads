with Ada.Real_Time;

package Ada.Synchronous_Task_Control.EDF is

   procedure Suspend_Until_True_And_Set_Deadline
      (S  : in out Suspension_Object;
       TS : Ada.Real_Time.Time_Span);

   --  Blocks the calling task until the state of the object S is True;
   --  at that point the task becomes ready with a deadline of
   --  Ada.Real_Time.Clock + TS, and the state of the object becomes
   --  False

end Ada.Synchronous_Task_Control.EDF;
