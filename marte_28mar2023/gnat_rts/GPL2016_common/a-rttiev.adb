------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . R E A L _ T I M E . T I M I N G _ E V E N T S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2005-2014, Free Software Foundation, Inc.        --
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

with Interfaces.C;
with System.OS_Interface;

package body Ada.Real_Time.Timing_Events is

   type Timing_Event_Ac is access all Timing_Event;

   package OSI renames System.OS_Interface;
   package HS renames System.Handlers_Support;

   use type Interfaces.C.int;

   -----------------------
   --  Handler_Wrapper  --
   -----------------------

   procedure Handler_Wrapper
     (Event : HS.Base_Timing_Event_Ac;
      TE    : access HS.MaRTE_Timed_Handler_T);

   procedure Handler_Wrapper
     (Event : HS.Base_Timing_Event_Ac;
      TE    : access HS.MaRTE_Timed_Handler_T) is
      pragma Unreferenced (TE);
      Handler : Timing_Event_Handler;
   begin
      pragma Assert (Timing_Event_Ac (Event).Handler /= null);

      --  Store current handler and clear it in the event ("The initial action
      --  of the execution of the handler is to clear the event" RM D.15(13/2))

      Handler := Timing_Event_Ac (Event).Handler;
      Timing_Event_Ac (Event).Handler := null;

      --  Call user's handler passing the event itself as parameter

      Handler (Timing_Event_Ac (Event).all);

   end Handler_Wrapper;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Event : in out Timing_Event) is
      Ret : Interfaces.C.int;
   begin
      Event.Handler := null;

      Ret := HS.MaRTE_Timed_Handler_Init (Event.Timed_Handler'Access,
                                          HS.CLOCK_REALTIME,
                                          Handler_Wrapper'Access,
                                          Event'Address,
                                          System.Address'Size);
      --  ??? Not using HS.CLOCK_MONOTONIC because the rest of the runtime uses
      --  gettimeofday to read clock (based on realtime clock). To use
      --  CLOCK_MONOTONIC here, it could be necessary to use it also in: timing
      --  events, STPO.Monotonic_Clock, condition variables (used for task
      --  suspension) and probably in s-osinte.adb' and 's-osprim.adb'

      if Ret /= 0 then
         raise Storage_Error;
      end if;
   end Initialize;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Event   : in out Timing_Event;
      At_Time : Time;
      Handler : Timing_Event_Handler)
   is
   begin
      Event.Handler := Handler;

      HS.Set_Handler
        (Event,
         Duration (At_Time),
         Relative => False,
         Clear => Handler = null);
   end Set_Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Event   : in out Timing_Event;
      In_Time : Time_Span;
      Handler : Timing_Event_Handler)
   is
   begin
      Event.Handler := Handler;

      HS.Set_Handler
        (Event,
         Duration (In_Time),
         Relative => True,
         Clear   => Handler = null);
   end Set_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Event : Timing_Event) return Timing_Event_Handler
   is
   begin
      return Event.Handler;
   end Current_Handler;

   --------------------
   -- Cancel_Handler --
   --------------------

   procedure Cancel_Handler
     (Event     : in out Timing_Event;
      Cancelled : out Boolean)
   is
      Ret : Interfaces.C.int;
   begin
      --  Disable MaRTE "timed event"

      Ret := HS.MaRTE_Timed_Handler_Disable (Event.Timed_Handler'Access);
      pragma Assert (Ret = 0);

      Cancelled := Event.Handler /= null;
      Event.Handler := null;
   end Cancel_Handler;

   -------------------
   -- Time_Of_Event --
   -------------------

   function Time_Of_Event (Event : Timing_Event) return Time is
      Ret : Interfaces.C.int;
      TS : aliased OSI.timespec;
   begin
      if Event.Handler = null then
         return Ada.Real_Time.Time_First;
      else
         Ret :=
           HS.MaRTE_Timed_Handler_Expiration_Time (Event.Timed_Handler'Access,
                                                   TS'Access);
         pragma Assert (Ret = 0);

         return Time (OSI.To_Duration (TS));
      end if;
   end Time_Of_Event;

end Ada.Real_Time.Timing_Events;
