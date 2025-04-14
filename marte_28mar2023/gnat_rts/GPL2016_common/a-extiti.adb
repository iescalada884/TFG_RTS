------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . E X E C U T I O N _ T I M E . T I M E R S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2005-2006, Free Software Foundation, Inc.        --
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

--  This is a MaRTE OS version of this package

with System.OS_Interface;
with Interfaces.C;

package body Ada.Execution_Time.Timers is

   package OSI renames System.OS_Interface;
   package HS renames System.Handlers_Support;

   use type Interfaces.C.int;

   type Timer_Ac is access all Timer;

   -----------------------
   --  Handler_Wrapper  --
   -----------------------

   procedure Handler_Wrapper (Event : HS.Base_Timing_Event_Ac;
                              TE    : access HS.MaRTE_Timed_Handler_T);

   procedure Handler_Wrapper (Event : HS.Base_Timing_Event_Ac;
                              TE    : access HS.MaRTE_Timed_Handler_T) is
      pragma Unreferenced (TE);
      Handler : Timer_Handler;
   begin
      pragma Assert (Timer_Ac (Event).Handler /= null);

      --  Store current handler and clear it in the event ("The initial action
      --  of the execution of the handler is to clear the event" RM
      --  D.14.1(17/2))

      Handler := Timer_Ac (Event).Handler;
      Timer_Ac (Event).Handler := null;

      --  Call user's handler passing the event itself as parameter

      Handler (Timer_Ac (Event).all);

   end Handler_Wrapper;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (TM : in out Timer) is
      Clock : aliased OSI.clockid_t;
      Ret : Interfaces.C.int;
   begin
      TM.Handler := null;

      if HS.Pthread_Getcpuclockid
        (HS.To_OSI_Thread_Id (TM.T.all),
         Clock'Access) /= 0
      then
         raise Storage_Error;
      end if;

      Ret := HS.MaRTE_Timed_Handler_Init (TM.Timed_Handler'Access,
                                          Clock,
                                          Handler_Wrapper'Access,
                                          TM'Address,
                                          System.Address'Size);
      if Ret /= 0 then
         raise Storage_Error;
      end if;
   end Initialize;

   CPU_Time_Zero : constant CPU_Time :=
     Time_Of (0, Ada.Real_Time.Time_Span_Zero);

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (TM      : in out Timer;
      At_Time : CPU_Time;
      Handler : Timer_Handler) is
   begin
      TM.Handler := Handler;

      HS.Set_Handler (TM,
                      Ada.Real_Time.To_Duration (At_Time - CPU_Time_Zero),
                      Relative => False,
                      Clear => Handler = null);
   end Set_Handler;

   procedure Set_Handler
     (TM      : in out Timer;
      In_Time : Ada.Real_Time.Time_Span;
      Handler : Timer_Handler) is
   begin
      TM.Handler := Handler;

      HS.Set_Handler (TM,
                      Ada.Real_Time.To_Duration (In_Time),
                      Relative => True,
                      Clear   => Handler = null);
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (TM : Timer) return Timer_Handler is
   begin
      return  TM.Handler;
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (TM        : in out Timer;
      Cancelled : out Boolean)
   is
      Ret : Interfaces.C.int;
   begin
      --  Disable MaRTE "timed event"

      Ret := HS.MaRTE_Timed_Handler_Disable (TM.Timed_Handler'Access);
      pragma Assert (Ret = 0);

      Cancelled := TM.Handler /= null;
      TM.Handler := null;
   end Cancel_Handler;

   --------------------
   -- Time_Remaining --
   --------------------

   function Time_Remaining (TM : Timer) return Ada.Real_Time.Time_Span is
      Ret : Interfaces.C.int;
      TS : aliased OSI.timespec;
   begin
      if TM.Handler = null then
         return Ada.Real_Time.Time_Span_Zero;
      else
         Ret :=
           HS.MaRTE_Timed_Handler_Time_To_Expiration (TM.Timed_Handler'Access,
                                                      TS'Access);
         pragma Assert (Ret = 0);

         return Ada.Real_Time.To_Time_Span (OSI.To_Duration (TS));
      end if;
   end Time_Remaining;

end Ada.Execution_Time.Timers;
