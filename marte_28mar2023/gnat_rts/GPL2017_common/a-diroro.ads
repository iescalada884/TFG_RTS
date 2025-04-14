------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . D I S P A T C H I N G . R O U N D _ R O B I N           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with System;
with Ada.Real_Time;

package Ada.Dispatching.Round_Robin is

   Default_Quantum : constant Ada.Real_Time.Time_Span;

   procedure Set_Quantum
     (Pri     : System.Priority;
      Quantum : Ada.Real_Time.Time_Span);

   procedure Set_Quantum
     (Low, High : System.Priority;
      Quantum   : Ada.Real_Time.Time_Span);

   function Actual_Quantum
     (Pri : System.Priority) return Ada.Real_Time.Time_Span;

   function Is_Round_Robin (Pri : System.Priority) return Boolean;

private
   MaRTE_Quantum : constant Duration;
   pragma Import (Ada, MaRTE_Quantum,
                  "marte__kernel__scheduler__round_robin_quantum");

   Default_Quantum : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.To_Time_Span (MaRTE_Quantum);
end Ada.Dispatching.Round_Robin;
