------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              S Y S T E M . H A N D L E R S _ S U P P O R T               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2005-2008, Free Software Foundation, Inc.        --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
--                                                                          --
------------------------------------------------------------------------------

--  This is a MaRTE OS specific package

with Ada.Finalization;
with Ada.Task_Identification;

with System.OS_Interface;

with Interfaces.C;
--  For int

package System.Handlers_Support is
   pragma Preelaborate;

   package OSI renames System.OS_Interface;

   --------------------------------
   -- Type MaRTE_Timed_Handler_T --
   --------------------------------

   type MaRTE_Timed_Handler_T is private;

   ----------------------------
   -- Type Base_Timing_Event --
   ----------------------------

   type Base_Timing_Event;
   --  Common base type for Timing Events, Execution Time Timers and Group
   --  Budgets

   type Base_Timing_Event is
     new Ada.Finalization.Limited_Controlled with record
      Timed_Handler : aliased MaRTE_Timed_Handler_T;

      Based_On_Group_Clock : Boolean := False;
   end record;

   type Base_Timing_Event_Ac is access all Base_Timing_Event'Class;

   -------------------------------------------
   -- Interface with MaRTE "timed handlers" --
   -------------------------------------------

   --  CLOCK_MONOTONIC : constant System.OS_Interface.clockid_t;
   --  pragma Import (C, CLOCK_MONOTONIC,
   --                 "marte__kernel__timers__clock_monotonic");
   --  ??? Not used right now, see comment in a-rttiev.Initialize

   CLOCK_REALTIME : constant System.OS_Interface.clockid_t;
   pragma Import (C, CLOCK_REALTIME,
                  "marte__kernel__timers__clock_realtime");

   --  MaRTE_Timed_Handler_Init

   function MaRTE_Timed_Handler_Init
     (TH      : access MaRTE_Timed_Handler_T;
      Clock   : OSI.clockid_t;
      Handler : access procedure (Event : Base_Timing_Event_Ac;
                                  TE    : access MaRTE_Timed_Handler_T);
      Area    : System.Address;
      Size    : System.OS_Interface.size_t) return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Init, "marte_timed_handler_init");

   --  MaRTE_Timed_Handler_Set

   function MaRTE_Timed_Handler_Set
     (TH      : access MaRTE_Timed_Handler_T;
      Options : Interfaces.C.int;
      Time    : access OSI.timespec) return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Set, "marte_timed_handler_set");

   --  MaRTE_Timed_Handler_Set_Interval

   function MaRTE_Timed_Handler_Set_Interval
     (TH       : access MaRTE_Timed_Handler_T;
      Interval : access OSI.timespec) return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Set_Interval,
                  "marte_timed_handler_set_interval");

   --  MaRTE_Timed_Handler_Disable

   function MaRTE_Timed_Handler_Disable (TH : access MaRTE_Timed_Handler_T)
                                         return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Disable,
                  "marte_timed_handler_disable");

   --  MaRTE_Timed_Handler_Expired

   function MaRTE_Timed_Handler_Expired
     (TH      : access constant MaRTE_Timed_Handler_T;
      Expired : access          Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Expired,
                  "marte_timed_handler_expired");

   --  MaRTE_Timed_Handler_Time_To_Expiration

   function MaRTE_Timed_Handler_Time_To_Expiration
     (TH : access constant MaRTE_Timed_Handler_T;
      TS : access          OSI.timespec)
      return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Time_To_Expiration,
                  "marte_timed_handler_time_to_expiration");

   --  MaRTE_Timed_Handler_Expiration_Time

   function MaRTE_Timed_Handler_Expiration_Time
     (TE_Ac : access constant MaRTE_Timed_Handler_T;
      TS    : access          OSI.timespec)
      return Interfaces.C.int;
   pragma Import (C, MaRTE_Timed_Handler_Expiration_Time,
                  "marte_timed_handler_expiration_time");

   ----------------------------------------
   -- Interface with MaRTE "thread sets" --
   ----------------------------------------

   type MaRTE_Thread_Set_T is private;
   Null_MaRTE_Thread_Set : constant MaRTE_Thread_Set_T;

   NO_SUCH_PROCESS : constant Interfaces.C.int;
   pragma Import (C, NO_SUCH_PROCESS, "marte__kernel__no_such_process");

   INVALID_ARGUMENT : constant Interfaces.C.int;
   pragma Import (C, INVALID_ARGUMENT, "marte__kernel__invalid_argument");

   OPERATION_NOT_SUPPORTED : constant Interfaces.C.int;
   pragma Import (C, OPERATION_NOT_SUPPORTED,
                  "marte__kernel__operation_not_supported");

   function Marte_Thread_Set_Create
     (Set : access MaRTE_Thread_Set_T)
     return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Create, "marte_threadset_create");

   function Marte_Thread_Set_Destroy
     (Set : MaRTE_Thread_Set_T)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Destroy, "marte_threadset_destroy");

   function Marte_Thread_Set_Empty
     (Set : MaRTE_Thread_Set_T)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Empty, "marte_threadset_empty");

   function Marte_Thread_Set_Add_Thread
     (Set : MaRTE_Thread_Set_T;
      T   : OSI.Thread_Id)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Add_Thread, "marte_threadset_add");

   function Marte_Thread_Set_Del_Thread
     (Set : MaRTE_Thread_Set_T;
      T   : OSI.Thread_Id)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Del_Thread, "marte_threadset_del");

   function Marte_Thread_Set_First
     (Set : MaRTE_Thread_Set_T;
      T   : access OSI.Thread_Id)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_First, "marte_threadset_first");

   function Marte_Thread_Set_Next
     (Set : MaRTE_Thread_Set_T;
      T   : access OSI.Thread_Id)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Next, "marte_threadset_next");

   function Marte_Thread_Set_Is_Member
     (Set       : MaRTE_Thread_Set_T;
      T         : OSI.Thread_Id;
      Is_Member : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Is_Member, "marte_threadset_ismember");

   function Marte_Thread_Set_Get_Set
     (T   : OSI.Thread_Id;
      Set : access MaRTE_Thread_Set_T)
      return Interfaces.C.int;
   pragma Import (C, Marte_Thread_Set_Get_Set,
                  "marte_threadset_getset");

   function Marte_Getgroupclockid
     (Set   : MaRTE_Thread_Set_T;
      Clock : access OSI.clockid_t)
      return Interfaces.C.int;
   pragma Import (C, Marte_Getgroupclockid, "marte_getgroupcpuclockid");

   ---------------------------
   -- Pthread_Getcpuclockid --
   ---------------------------

   function Pthread_Getcpuclockid
     (T     :        OSI.Thread_Id;
      Clock : access OSI.clockid_t) return Interfaces.C.int;
   pragma Import (C, Pthread_Getcpuclockid, "pthread_getcpuclockid");

   ----------------------
   -- To_OSI_Thread_Id --
   ----------------------

   function To_OSI_Thread_Id
     (Ada_Task_Id : Ada.Task_Identification.Task_Id)
      return OSI.Thread_Id;

   -------------------
   --  Set_Handler  --
   -------------------

   procedure Set_Handler
     (Event    : in out Base_Timing_Event'Class;
      T        : Duration; -- absolute or relative depending on "Realtive"
      Relative : Boolean;
      Clear    : Boolean);

private

   --  MaRTE_Timed_Handler_T
   TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES : constant := 100;

   type MaRTE_Timed_Handler_T is
     new String (1 .. TIMED_HANDLER_TIMED_EVENT_SIZE_IN_BYTES);
   pragma Convention (C, MaRTE_Timed_Handler_T);

   --  MaRTE_Thread_Set_T

   type MaRTE_Thread_Set_T is new Interfaces.C.int;
   pragma Convention (C, MaRTE_Thread_Set_T);
   Null_MaRTE_Thread_Set : constant MaRTE_Thread_Set_T := 0;

   ----------------
   --  Finalize  --
   ----------------

   overriding procedure Finalize (Event : in out Base_Timing_Event);
   --  Finalization procedure is required to satisfy (RM D.15 (19/2)), which
   --  says that the object must be cleared on finalization.

end System.Handlers_Support;
