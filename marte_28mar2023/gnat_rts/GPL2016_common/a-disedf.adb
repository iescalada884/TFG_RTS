------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  A D A . D I S P A T C H I N G . E D F                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2005-2008, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
--
--
--
--
--
--
--
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  MaRTE OS version of this package

with Interfaces.C;

with System.OS_Interface;

with System.Handlers_Support;
--  For To_OSI_Thread_Id

with Ada.Unchecked_Conversion;

package body Ada.Dispatching.EDF is

   package OSI renames System.OS_Interface;
   package HS renames System.Handlers_Support;

   use type Ada.Task_Identification.Task_Id, Ada.Real_Time.Time,
       Interfaces.C.int;

   function UC is new Ada.Unchecked_Conversion (Deadline, Duration);
   function UC is new Ada.Unchecked_Conversion (Duration, Deadline);

   ------------------
   -- Set_Deadline --
   ------------------

   procedure Set_Deadline
      (D : Deadline;
       T : Ada.Task_Identification.Task_Id :=
         Ada.Task_Identification.Current_Task) is
      Ret : Interfaces.C.int;
      TS : aliased OSI.timespec;
   begin
      --  ???  How to know a task has terminated? (RM D.2.6(29/2))

      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      --  Set new deadline

      TS := OSI.To_Timespec (UC (D));
      Ret := OSI.pthread_setdeadline (HS.To_OSI_Thread_Id (T),
                                      TS'Access,
                                      OSI.CLOCK_REALTIME,
                                      immediate => 1);

      if Ret = HS.NO_SUCH_PROCESS then
         raise Tasking_Error; --  Assume the cause is the task has terminated
      end if;

      pragma Assert (Ret = 0);
   end Set_Deadline;

   ----------------------------------
   -- Delay_Until_And_Set_Deadline --
   ----------------------------------

   procedure Delay_Until_And_Set_Deadline
      (Delay_Until_Time : Ada.Real_Time.Time;
       Deadline_Offset  : Ada.Real_Time.Time_Span) is
      Ret : Interfaces.C.int;
      TS : aliased OSI.timespec;
   begin
      --  ??? we plan to add a MaRTE function that permits set deadline and
      --  delay atomicaly

      --  Set new deadline

      TS := OSI.To_Timespec (UC (Delay_Until_Time + Deadline_Offset));
      Ret :=
        OSI.pthread_setdeadline
          (HS.To_OSI_Thread_Id (Ada.Task_Identification.Current_Task),
           TS'Access,
           OSI.CLOCK_REALTIME,
           immediate => 0);

      pragma Assert (Ret = 0);

      --  Delay

      delay until Delay_Until_Time;

   end Delay_Until_And_Set_Deadline;

   ------------------
   -- Get_Deadline --
   ------------------

   function Get_Deadline
      (T : Ada.Task_Identification.Task_Id :=
             Ada.Task_Identification.Current_Task) return Deadline is
      Ret : Interfaces.C.int;
      TS : aliased OSI.timespec;
   begin
      Ret := OSI.pthread_getdeadline (HS.To_OSI_Thread_Id (T),
                                      OSI.CLOCK_REALTIME,
                                      TS'Access);

      pragma Assert (Ret = 0);

      return UC (OSI.To_Duration (TS));
   end Get_Deadline;

end Ada.Dispatching.EDF;
