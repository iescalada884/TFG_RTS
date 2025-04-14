------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . E X E C U T I O N _ T I M E . G R O U P _ B U D G E T S     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  MaRTE OS version

with Interfaces.C;
with Ada.Real_Time;
with System.OS_Interface;
with System.Task_Primitives.Operations;
with System.Tasking;
with Ada.Unchecked_Conversion;

package body Ada.Execution_Time.Group_Budgets is

   package OSI renames System.OS_Interface;
   package HS renames System.Handlers_Support;
   package TPO renames System.Task_Primitives.Operations;

   use type Interfaces.C.int;

   type Group_Budget_Ac is access all Group_Budget;

   -----------------------
   --  Handler_Wrapper  --
   -----------------------
   procedure Handler_Wrapper
     (Event : HS.Base_Timing_Event_Ac;
      TE    : access HS.MaRTE_Timed_Handler_T);
   pragma Convention (C, Handler_Wrapper);

   procedure Handler_Wrapper
     (Event : HS.Base_Timing_Event_Ac;
      TE    : access HS.MaRTE_Timed_Handler_T) is
      pragma Unreferenced (TE);
   begin
      --  A group budget with the handler cleared can expire (RM D.14.2 (21/2))

      if Group_Budget_Ac (Event).Handler /= null then
         --  Call user's handler passing the event itself as parameter

         Group_Budget_Ac (Event).Handler (Group_Budget_Ac (Event).all);
      end if;

      --  Group budget handlers are not cleared after expiration (RM D.14.2
      --  (21/2))

   end Handler_Wrapper;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (GB : in out Group_Budget) is
      Ret : Interfaces.C.int;
      Clock : aliased OSI.clockid_t;
   begin
      GB.Handler := null;

      --  Create thread set

      Ret := HS.Marte_Thread_Set_Create (GB.Set'Access);
      if Ret /= 0 then
         raise Storage_Error;
      end if;

      --  Get group clock

      Ret := HS.Marte_Getgroupclockid (GB.Set, Clock'Access);
      if Ret /= 0 then
         Ret := HS.Marte_Thread_Set_Destroy (GB.Set);
         raise Storage_Error;
      end if;

      --  Create timed handler

      Ret := HS.MaRTE_Timed_Handler_Init (GB.Timed_Handler'Access,
                                          Clock,
                                          Handler_Wrapper'Access,
                                          GB'Address,
                                          System.Address'Size);
      if Ret /= 0 then
         Ret := HS.Marte_Thread_Set_Destroy (GB.Set);
         raise Storage_Error;
      end if;

      GB.Based_On_Group_Clock := True;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (GB : in out Group_Budget) is
      Ret : Interfaces.C.int;
   begin
      --  Marte_Thread_Set_Destroy removes tasks from the set and destroyes the
      --  associated timed handler (so it isn't necessary to call
      --  HS.Finalize)

      Ret := HS.Marte_Thread_Set_Destroy (GB.Set);
      pragma Assert (Ret = 0);
   end Finalize;

   --------------
   -- Add_Task --
   --------------

   procedure Add_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id) is
      Ret : Interfaces.C.int;
   begin
      --  ???  How to know a task has terminated? (RM D.14.2(33/2))

      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      Ret := HS.Marte_Thread_Set_Add_Thread (GB.Set, HS.To_OSI_Thread_Id (T));

      if Ret = HS.NO_SUCH_PROCESS then
         raise Tasking_Error; --  Assume the cause is the task has terminated
      end if;

      if Ret = HS.OPERATION_NOT_SUPPORTED then
         raise Group_Budget_Error; -- T is a member of some other group
      end if;

      pragma Assert (Ret = 0);
   end Add_Task;

   -----------------
   -- Remove_Task --
   -----------------

   procedure Remove_Task
     (GB : in out Group_Budget;
      T  : Ada.Task_Identification.Task_Id) is
      Ret : Interfaces.C.int;
   begin
      --  ???  How to know a task has terminated? (RM D.14.2(33/2))

      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      Ret := HS.Marte_Thread_Set_Del_Thread (GB.Set, HS.To_OSI_Thread_Id (T));

      if Ret = HS.NO_SUCH_PROCESS then
         raise Tasking_Error; --  Assume the cause is the task has terminated
      end if;

      if Ret = HS.INVALID_ARGUMENT then
         raise Group_Budget_Error; -- T is not a member of the group
      end if;

      pragma Assert (Ret = 0);
   end Remove_Task;

   ---------------
   -- Is_Member --
   ---------------

   function Is_Member
     (GB : Group_Budget;
      T  : Ada.Task_Identification.Task_Id) return Boolean is
      Ret : Interfaces.C.int;
      Is_Member : aliased Interfaces.C.int;
   begin
      --  ???  How to know a task has terminated? (RM D.14.2(33/2))

      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      Ret := HS.Marte_Thread_Set_Is_Member (GB.Set, HS.To_OSI_Thread_Id (T),
                                            Is_Member'Access);
      if Ret = HS.NO_SUCH_PROCESS then
         raise Tasking_Error; --  Assume the cause is the task has terminated
      end if;

      pragma Assert (Ret = 0);
      return Is_Member = 1;
   end Is_Member;

   -----------------------
   -- Is_A_Group_Member --
   -----------------------

   function Is_A_Group_Member
     (T : Ada.Task_Identification.Task_Id) return Boolean is
      Ret : Interfaces.C.int;
      Set : aliased HS.MaRTE_Thread_Set_T;
      use type HS.MaRTE_Thread_Set_T;
   begin
      --  ??? How to know a task has terminated? (RM D.14.2(33/2))

      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      Ret := HS.Marte_Thread_Set_Get_Set (HS.To_OSI_Thread_Id (T),
                                          Set'Access);
      if Ret = HS.NO_SUCH_PROCESS then
         --  Assume the cause is the task has terminated

         raise Tasking_Error;
      end if;

      pragma Assert (Ret = 0);
      return Set /= HS.Null_MaRTE_Thread_Set;
   end Is_A_Group_Member;

   -------------
   -- Members --
   -------------

   function Members (GB : Group_Budget) return Task_Array is
      function To_Ada_Task_Id is
        new Ada.Unchecked_Conversion (System.Tasking.Task_Id,
                                      Ada.Task_Identification.Task_Id);
      Task_Array_Ac : access Task_Array;
      T_Id : aliased OSI.Thread_Id;
      Ret : Interfaces.C.int;
      Task_Count : Integer := 0;
   begin
      --  ??? How to achieve atomicity with the rest of operations?

      --  Count the number of tasks in the group

      Ret := HS.Marte_Thread_Set_First (GB.Set, T_Id'Access);
      pragma Assert (Ret = 0 or else Ret = HS.NO_SUCH_PROCESS);

      while Ret /= HS.NO_SUCH_PROCESS loop
         Task_Count := Task_Count + 1;

         Ret :=  HS.Marte_Thread_Set_Next (GB.Set, T_Id'Access); -- next thread
         pragma Assert (Ret = 0 or else Ret = HS.NO_SUCH_PROCESS);
      end loop;

      --  Create the array

      Task_Array_Ac := new Task_Array (1 .. Task_Count);

      --  Get the first thread in set

      Ret := HS.Marte_Thread_Set_First (GB.Set, T_Id'Access);
      pragma Assert (Ret = 0 or else Ret = HS.NO_SUCH_PROCESS);

      --  Get the following threads

      for I in Task_Array_Ac'Range loop
         pragma Assert (Ret = 0);
         Task_Array_Ac (I) := To_Ada_Task_Id (TPO.Pthread_T_To_Task_Id (T_Id));

         --  Get next thread in set
         Ret :=  HS.Marte_Thread_Set_Next (GB.Set, T_Id'Access);
         pragma Assert (Ret = 0 or else Ret = HS.NO_SUCH_PROCESS);
      end loop;

      pragma Assert (Ret = HS.NO_SUCH_PROCESS); -- reached the end of group

      --  Return the array

      return Task_Array_Ac.all;
   end Members;

   ---------------
   -- Replenish --
   ---------------

   procedure Replenish
     (GB : in out Group_Budget;
      To : Ada.Real_Time.Time_Span) is
      Ret : Interfaces.C.int;
      Ts : aliased OSI.timespec;
      use type Ada.Real_Time.Time_Span;
   begin
      if To < Ada.Real_Time.Time_Span_Zero then
         raise Group_Budget_Error;
      end if;
      Ts := OSI.To_Timespec (Ada.Real_Time.To_Duration (To));
      Ret := HS.MaRTE_Timed_Handler_Set (GB.Timed_Handler'Access,
                                         0, -- relative
                                         Ts'Access);
      pragma Assert (Ret = 0);
   end Replenish;

   ---------
   -- Add --
   ---------

   procedure Add
     (GB       : in out Group_Budget;
      Interval :        Ada.Real_Time.Time_Span) is
      Ret : Interfaces.C.int;
      Ts : aliased OSI.timespec;
   begin
      Ts := OSI.To_Timespec (Ada.Real_Time.To_Duration (Interval));
      Ret := HS.MaRTE_Timed_Handler_Set_Interval (GB.Timed_Handler'Access,
                                                  Ts'Access);
      pragma Assert (Ret = 0);
   end Add;

   ------------------------
   -- Budget_Has_Expired --
   ------------------------

   function Budget_Has_Expired (GB : Group_Budget) return Boolean is
      Ret : Interfaces.C.int;
      Expired : aliased Interfaces.C.int;
   begin
      Ret := HS.MaRTE_Timed_Handler_Expired (GB.Timed_Handler'Access,
                                             Expired'Access);
      pragma Assert (Ret = 0);
      return Expired = 1;
   end Budget_Has_Expired;

   ----------------------
   -- Budget_Remaining --
   ----------------------

   function Budget_Remaining
     (GB : Group_Budget) return Ada.Real_Time.Time_Span is
      Ret : Interfaces.C.int;
      TS : aliased OSI.timespec;
   begin
      Ret :=
        HS.MaRTE_Timed_Handler_Time_To_Expiration (GB.Timed_Handler'Access,
                                                   TS'Access);
      pragma Assert (Ret = 0);

      return Ada.Real_Time.To_Time_Span (OSI.To_Duration (TS));
   end Budget_Remaining;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (GB      : in out Group_Budget;
      Handler : Group_Budget_Handler) is
   begin
      GB.Handler := Handler;
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (GB : Group_Budget) return Group_Budget_Handler is
   begin
      return GB.Handler;
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (GB        : in out Group_Budget;
      Cancelled : out Boolean)
   is
      Ret : Interfaces.C.int;
   begin
      --  Disable MaRTE "timed event"

      Ret := HS.MaRTE_Timed_Handler_Disable (GB.Timed_Handler'Access);
      pragma Assert (Ret = 0);

      Cancelled := GB.Handler /= null;
      GB.Handler := null;
   end Cancel_Handler;

end Ada.Execution_Time.Group_Budgets;
