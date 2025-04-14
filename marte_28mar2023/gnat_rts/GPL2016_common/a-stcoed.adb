
with Ada.Exceptions;

with System.Tasking;
with System.Task_Primitives.Operations;

package body Ada.Synchronous_Task_Control.EDF is

   -----------------------------------------
   -- Suspend_Until_True_And_Set_Deadline --
   -----------------------------------------

   procedure Suspend_Until_True_And_Set_Deadline
      (S  : in out Suspension_Object;
       TS : Ada.Real_Time.Time_Span) is
   begin
      --  This is a potentially blocking (see ARM D.10, par. 10.1/3), so that
      --  if pragma Detect_Blocking is active then Program_Error must be
      --  raised if this operation is called from a protected action.

      if System.Tasking.Detect_Blocking
        and then System.Tasking.Self.Common.Protected_Action_Nesting > 0
      then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity, "potentially blocking operation");
      end if;

      System.Task_Primitives.Operations.Suspend_Until_True_And_Set_Deadline
        (S.SO, Ada.Real_Time.To_Duration (TS));
   end Suspend_Until_True_And_Set_Deadline;

end Ada.Synchronous_Task_Control.EDF;
