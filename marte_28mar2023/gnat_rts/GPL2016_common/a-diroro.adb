------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . D I S P A T C H I N G . R O U N D _ R O B I N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This is a MaRTE OS version of this package

package body Ada.Dispatching.Round_Robin is

   -----------------
   -- Set_Quantum --
   -----------------

   procedure Set_Quantum
     (Pri     : System.Priority;
      Quantum : Ada.Real_Time.Time_Span) is
      pragma Unreferenced (Pri, Quantum);
   begin
      null;
   end Set_Quantum;

   -----------------
   -- Set_Quantum --
   -----------------

   procedure Set_Quantum
     (Low, High : System.Priority;
      Quantum   : Ada.Real_Time.Time_Span) is
      pragma Unreferenced (Low, High, Quantum);
   begin
      null;
   end Set_Quantum;

   --------------------
   -- Actual_Quantum --
   --------------------

   function Actual_Quantum
     (Pri : System.Priority) return Ada.Real_Time.Time_Span is
      pragma Unreferenced (Pri);
   begin
      return Default_Quantum;
   end Actual_Quantum;

   --------------------
   -- Is_Round_Robin --
   --------------------

   function Is_Round_Robin (Pri : System.Priority) return Boolean is

      Dispatching_Policy : Character;
      pragma Import (C, Dispatching_Policy, "__gl_task_dispatching_policy");
      --  Value of the pragma Dispatching_Policy:
      --    'F' for FIFO_Within_Priorities
      --    'R' for Round_Robin_Within_Priorities
      --    'E' for EDF_Across_Priorities
      --    'N' for Non_Preemptive_Within_Priorities
      --    ' ' for none.

      function Get_Policy (Prio : System.Any_Priority) return Character;
      pragma Import (C, Get_Policy, "__gnat_get_specific_dispatching");
      --  Get priority specific dispatching policy

      Priority_Specific_Policy : constant Character := Get_Policy (Pri);
      --  Upper case first character of the policy name corresponding to the
      --  task as set by a Priority_Specific_Dispatching pragma.

   begin
      return Dispatching_Policy = 'R'
        or else Priority_Specific_Policy = 'R';
   end Is_Round_Robin;

end Ada.Dispatching.Round_Robin;
