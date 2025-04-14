------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   A D A . E X E C U T I O N _ T I M E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is a MaRTE OS version of this package

with System.OS_Interface;
with Interfaces.C;
with Ada.Unchecked_Conversion;

with System.Handlers_Support; -- MaRTE OS

package body Ada.Execution_Time is

   package OSI renames System.OS_Interface;

   function To_CPU_Time is
      new Ada.Unchecked_Conversion (Duration, CPU_Time);
   --  Time is equal to Duration (although it is a private type) and
   --  CPU_Time is equal to Time.

   ---------
   -- "+" --
   ---------

   function "+"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) + Right);
   end "+";

   function "+"
     (Left  : Ada.Real_Time.Time_Span;
      Right : CPU_Time) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Left + Ada.Real_Time.Time (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Left  : CPU_Time;
      Right : Ada.Real_Time.Time_Span) return CPU_Time
   is
      use type Ada.Real_Time.Time;
   begin
      return CPU_Time (Ada.Real_Time.Time (Left) - Right);
   end "-";

   function "-"
     (Left  : CPU_Time;
      Right : CPU_Time) return Ada.Real_Time.Time_Span
   is
      use type Ada.Real_Time.Time;
   begin
      return (Ada.Real_Time.Time (Left) - Ada.Real_Time.Time (Right));
   end "-";

   -----------
   -- Clock --
   -----------

   function Clock
     (T : Ada.Task_Identification.Task_Id :=
        Ada.Task_Identification.Current_Task) return CPU_Time
   is
      Clock : aliased OSI.clockid_t;
      Ts : aliased OSI.timespec;
      Ret : Interfaces.C.int;

      use type Interfaces.C.int;
      use Real_Time;

   begin
      if T = Ada.Task_Identification.Null_Task_Id then
         raise Program_Error;
      end if;

      --  How to know if an Ada task has terminated but the underlying thread
      --  has not finished yet?

      if System.Handlers_Support.Pthread_Getcpuclockid -- MaRTE OS
        (System.Handlers_Support.To_OSI_Thread_Id (T),
         Clock'Access) /= 0
      then
         --  Assumes invalid task ID, probably because the task has terminated

         raise Tasking_Error;
      end if;

      --  Read thread clock

      Ret := OSI.clock_gettime (Clock, Ts'Access);
      pragma Assert (Ret = 0);

      return To_CPU_Time (OSI.To_Duration (Ts));
   end Clock;

   CLOCK_INTERRUPTS_CPUTIME : constant OSI.clockid_t;
   pragma Import (C, CLOCK_INTERRUPTS_CPUTIME,
                  "marte__kernel__timers__clock_interrupts_cputime");

   --------------------------
   -- Clock_For_Interrupts --
   --------------------------

   function Clock_For_Interrupts return CPU_Time is

      Ts : aliased OSI.timespec;
      Ret : Interfaces.C.int;

      use type Interfaces.C.int;
      use Real_Time;

   begin
      --  Read the interrupt clock

      Ret := OSI.clock_gettime (CLOCK_INTERRUPTS_CPUTIME, Ts'Access);
      pragma Assert (Ret = 0);

      return To_CPU_Time (OSI.To_Duration (Ts));
   end Clock_For_Interrupts;

   -----------
   -- Split --
   -----------

   procedure Split
     (T  : CPU_Time;
      SC : out Ada.Real_Time.Seconds_Count;
      TS : out Ada.Real_Time.Time_Span)
   is

   begin
      Ada.Real_Time.Split (Ada.Real_Time.Time (T), SC, TS);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (SC : Ada.Real_Time.Seconds_Count;
      TS : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero)
      return CPU_Time
   is
   begin
      return CPU_Time (Ada.Real_Time.Time_Of (SC, TS));
   end Time_Of;

end Ada.Execution_Time;
