------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . R E A L _ T I M E . T I M I N G _ E V E N T S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2005-2009, Free Software Foundation, Inc.        --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with System.Handlers_Support;

package Ada.Real_Time.Timing_Events is

   type Timing_Event is tagged limited private;

   type Timing_Event_Handler
     is access protected procedure (Event : in out Timing_Event);

   procedure Set_Handler
     (Event   : in out Timing_Event;
      At_Time : Time;
      Handler : Timing_Event_Handler);

   procedure Set_Handler
     (Event   : in out Timing_Event;
      In_Time : Time_Span;
      Handler : Timing_Event_Handler);

   function Current_Handler
     (Event : Timing_Event) return Timing_Event_Handler;

   procedure Cancel_Handler
     (Event     : in out Timing_Event;
      Cancelled : out Boolean);

   function Time_Of_Event (Event : Timing_Event) return Time;

private

   type Timing_Event is new System.Handlers_Support.Base_Timing_Event
   with record
      Handler : Timing_Event_Handler;
      --  An access value designating the protected procedure to be invoked
      --  at the Timeout time in the future.  When this value is null the event
      --  is said to be "cleared" and no timeout is processed.
   end record;

   procedure Initialize (Event : in out Timing_Event);

end Ada.Real_Time.Timing_Events;
