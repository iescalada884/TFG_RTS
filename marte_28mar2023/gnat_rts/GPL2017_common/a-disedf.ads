------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . D I S P A T C H I N G . E D F                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  MaRTE OS version of this package

with Ada.Real_Time;
with Ada.Task_Identification;

package Ada.Dispatching.EDF is
   --  pragma Preelaborate; MaRTE OS

   subtype Deadline is Ada.Real_Time.Time;

   Default_Deadline : constant Deadline := Ada.Real_Time.Time_Last;

   procedure Set_Deadline
      (D : Deadline;
       T : Ada.Task_Identification.Task_Id :=
             Ada.Task_Identification.Current_Task);

   procedure Delay_Until_And_Set_Deadline
      (Delay_Until_Time : Ada.Real_Time.Time;
       Deadline_Offset  : Ada.Real_Time.Time_Span);

   function Get_Deadline
      (T : Ada.Task_Identification.Task_Id :=
             Ada.Task_Identification.Current_Task)
       return Deadline;
   --  with       {MaRTE_OS}
   --    SPARK_Mode,
   --    Volatile_Function,
   --    Global => Ada.Task_Identification.Tasking_State;

end Ada.Dispatching.EDF;
