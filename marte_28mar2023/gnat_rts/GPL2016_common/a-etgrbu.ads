------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . E X E C U T I O N _ T I M E . G R O U P _ B U D G E T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This is a MaRTE OS version of this package

with System.Handlers_Support;

package Ada.Execution_Time.Group_Budgets is

   type Group_Budget is tagged limited private;

   type Group_Budget_Handler is access
      protected procedure (GB : in out Group_Budget);

   type Task_Array is
      array (Positive range <>) of Ada.Task_Identification.Task_Id;

   Min_Handler_Ceiling : constant System.Any_Priority :=
                           System.Any_Priority'First;
   --  Initial value is an arbitrary choice ???

   procedure Add_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   procedure Remove_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id);

   function Is_Member
     (GB : Group_Budget;
      T  : Ada.Task_Identification.Task_Id) return Boolean;

   function Is_A_Group_Member
     (T : Ada.Task_Identification.Task_Id) return Boolean;

   function Members (GB : Group_Budget) return Task_Array;

   procedure Replenish
     (GB : in out Group_Budget;
      To : Ada.Real_Time.Time_Span);

   procedure Add
     (GB       : in out Group_Budget;
      Interval : Ada.Real_Time.Time_Span);

   function Budget_Has_Expired (GB : Group_Budget) return Boolean;

   function Budget_Remaining
     (GB : Group_Budget) return Ada.Real_Time.Time_Span;

   procedure Set_Handler
     (GB      : in out Group_Budget;
      Handler : Group_Budget_Handler);

   function Current_Handler (GB : Group_Budget) return Group_Budget_Handler;

   procedure Cancel_Handler
     (GB        : in out Group_Budget;
      Cancelled : out Boolean);

   Group_Budget_Error : exception;

private
   type Group_Budget is new System.Handlers_Support.Base_Timing_Event
   with record
      Handler : Group_Budget_Handler;
      Set : aliased System.Handlers_Support.MaRTE_Thread_Set_T;
   end record;

   overriding
   procedure Initialize (GB : in out Group_Budget);
   --  Create the operating sistem resuources: task set and timed handler

   overriding
   procedure Finalize (GB : in out Group_Budget);
   --  Destroy the operating system resources and free the tasks from this
   --  group

end Ada.Execution_Time.Group_Budgets;
