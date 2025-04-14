------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--           'P O S I X _ A p p l i c a t i o n _ S c h e d u l i n g'
--
--                                  Body
--
--
--  File 'posix_application_scheduling.ads'                            By MAR.
--
--
--  Interface Ada for the Application-Defined Scheduling.
--
--  This package is not a part of the POSIX Ada bindings, it is only a
--  proposal we think could be interested for future revisions of the
--  POSIX Ada bindings.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;

with POSIX.Implementation;
with POSIX.Signals.Implementation;

with MaRTE.Kernel.Tasks_Operations;
with MaRTE.Kernel.Tasks_Operations.Internals;
with MaRTE.Kernel.Scheduler;
with MaRTE.Kernel.Signals;
with Pthread; pragma Elaborate_All (Pthread);
with MaRTE.HAL;
with MaRTE.Kernel.Timers;
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
use type MaRTE.Integer_Types.Int;

package body POSIX_Application_Scheduling is

   use APPSCHD_DATA;

   package TO   renames MaRTE.Kernel.Tasks_Operations;
   package TOI  renames MaRTE.Kernel.Tasks_Operations.Internals;
   package HWI  renames MaRTE.HAL;
   package TMRS renames MaRTE.Kernel.Timers;
   package SCHD renames MaRTE.Kernel.Scheduler;
   package PI   renames POSIX.Implementation;

   function SA (Base : in MaRTE.Kernel.Sched_App_Data_Base_Ac)
               return APPSCHD_DATA.Sched_App_Data_Ac
     renames APPSCHD_DATA.UC_To_Sched_App_Data_Ac;


   ----------------------------------------------------------------------------
   -- Scheduling Events -------------------------------------------------------
   ----------------------------------------------------------------------------

   --------------------
   -- Get_Event_Code --
   --------------------
   function Get_Event_Code (Event : in Scheduling_Event)
                            return Event_Code is
   begin
      return Event.Code;
   end Get_Event_Code;

   --------------
   -- Get_Task --
   --------------
   function Get_Task (Event : in Scheduling_Event)
                      return Ada.Task_Identification.Task_Id is
   begin
      PI.Check (Event.Code /= APPSCHED_TIMEOUT and
                Event.Code /= APPSCHED_SIGNAL, POSIX.Invalid_Argument);
      return Event.T;
   end Get_Task;

   ------------------------
   -- Get_Sched_Priority --
   ------------------------
   function Get_Sched_Priority (Event : in Scheduling_Event)
                                return System.Any_Priority is
   begin
      PI.Check (Event.Code = APPSCHED_PRIORITY_INHERIT or
                Event.Code = APPSCHED_PRIORITY_UNINHERIT,
                POSIX.Invalid_Argument);
      return Event.Sched_Priority;
   end Get_Sched_Priority;

   ---------------------
   -- Get_Signal_Info --
   ---------------------
   function Get_Signal_Info (Event : in Scheduling_Event)
                             return POSIX_Signals.Signal_Info is
   begin
      PI.Check (Event.Code = APPSCHED_SIGNAL, POSIX.Invalid_Argument);
      return Event.Siginfo;
   end Get_Signal_Info;

   -------------------------
   -- Get_User_Event_Code --
   -------------------------
   function Get_User_Event_Code (Event : in Scheduling_Event)
                                return Integer is
   begin
      PI.Check (Event.Code = APPSCHED_EXPLICIT_CALL,
                POSIX.Invalid_Argument);
      return Event.User_Event_Code;
   end Get_User_Event_Code;

   ---------------
   -- Get_Mutex --
   ---------------
   function Get_Mutex (Event : in Scheduling_Event)
                       return POSIX_Mutexes.Mutex_Descriptor is
   begin
      PI.Check (Event.Code = APPSCHED_INIT_MUTEX    or
                Event.Code = APPSCHED_DESTROY_MUTEX or
                Event.Code = APPSCHED_LOCK_MUTEX    or
                Event.Code = APPSCHED_UNLOCK_MUTEX  or
                Event.Code = APPSCHED_BLOCK_AT_MUTEX,
                POSIX.Invalid_Argument);
      return Event.M;
   end Get_Mutex;


   ----------------------------------------------------------------------------
   -- Scheduling actions ------------------------------------------------------
   ----------------------------------------------------------------------------

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (Sched_Actions : in out Scheduling_Actions) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Init
         (Sched_Actions.Actions'Access));
   end Initialize;

   -------------
   -- Destroy --
   -------------
   procedure Destroy (Sched_Actions : in out Scheduling_Actions) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Destroy
         (Sched_Actions.Actions'Access));
   end Destroy;

   ----------------
   -- Add_Accept --
   ----------------
   procedure Add_Accept
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Addaccept
         (Sched_Actions.Actions'Access,
          PI.To_Kernel_Task_Id (T)));
   end Add_Accept;

   ----------------
   -- Add_Reject --
   ----------------
   procedure Add_Reject
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Addreject
         (Sched_Actions.Actions'Access,
          PI.To_Kernel_Task_Id (T)));
   end Add_Reject;

   ------------------
   -- Add_Activate --
   ------------------
   procedure Add_Activate
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Addactivate
         (Sched_Actions.Actions'Access,
          PI.To_Kernel_Task_Id (T)));
   end Add_Activate;

   -----------------
   -- Add_Suspend --
   -----------------
   procedure Add_Suspend
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Addsuspend
         (Sched_Actions.Actions'Access,
          PI.To_Kernel_Task_Id (T)));
   end Add_Suspend;

   ----------------------
   -- Add_Accept_Mutex --
   ----------------------
   procedure Add_Accept_Mutex
     (Sched_Actions : in out Scheduling_Actions;
      M             : in     POSIX_Mutexes.Mutex_Descriptor) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Addaccept_Mutex
         (Sched_Actions.Actions'Access, M));
   end Add_Accept_Mutex;

   ----------------------
   -- Add_Reject_Mutex --
   ----------------------
   procedure Add_Reject_Mutex
     (Sched_Actions : in out Scheduling_Actions;
      M             : in     POSIX_Mutexes.Mutex_Descriptor) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Addreject_Mutex
         (Sched_Actions.Actions'Access, M));
   end Add_Reject_Mutex;

   --------------------
   -- Add_Lock_Mutex --
   --------------------
   procedure Add_Lock_Mutex
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id;
      M             : in     POSIX_Mutexes.Mutex_Descriptor) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appsched_Actions_Addlock_Mutex
         (Sched_Actions.Actions'Access,
          PI.To_Kernel_Task_Id (T),
          M));
   end Add_Lock_Mutex;


   ----------------------------------------------------------------------------
   -- Execute Scheduling Actions ----------------------------------------------
   ----------------------------------------------------------------------------

   function To_Kernel_Sched_Actions_Set is
     new Ada.Unchecked_Conversion (Scheduling_Actions,
                                   TO_APPSCHD.Sched_Actions_Set);

   function AS (Base : MaRTE.Kernel.AppScheduler_Data_Base_Ac)
                return APPSCHD_DATA.AppScheduler_Data_Ac
     renames APPSCHD_DATA.UC_To_AppScheduler_Data_Ac;

   ----------------
   -- Copy_Event --
   ----------------
   --  Internal procedure to copy information from the MaRTE event to
   --  the POSIX event.
   procedure Copy_Event (From : in  APPSCHD_DATA.AppSched_Event_Ac;
                         To   : out Scheduling_Event);
   procedure Copy_Event (From : in  APPSCHD_DATA.AppSched_Event_Ac;
                         To   : out Scheduling_Event) is
      function Siginfo_T_To_POSIX_Signal_Info is
        new Ada.Unchecked_Conversion (MaRTE.Kernel.Signals.Siginfo_T,
                                      POSIX_Signals.Signal_Info);
   begin
      To.Code := Event_Code (From.Event_Code);
      if (From.Event_Code /= APPSCHED_TIMEOUT and
          From.Event_Code /= APPSCHED_SIGNAL) then
         To.T := PI.To_Ada_Task_Id (From.T);
      else
         To.T := Ada.Task_Identification.Null_Task_Id;
      end if;
      case From.Event_Code is

         when APPSCHED_PRIORITY_INHERIT | APPSCHED_PRIORITY_UNINHERIT =>
            To.Sched_Priority := System.Any_Priority (From.Sched_Priority);

         when APPSCHED_SIGNAL =>
            To.Siginfo := Siginfo_T_To_POSIX_Signal_Info (From.Siginfo);

         when APPSCHED_INIT_MUTEX | APPSCHED_DESTROY_MUTEX |
           APPSCHED_LOCK_MUTEX | APPSCHED_TRY_LOCK_MUTEX |
           APPSCHED_UNLOCK_MUTEX | APPSCHED_BLOCK_AT_MUTEX |
           APPSCHED_CHANGE_SCHED_PARAM_MUTEX =>
            To.M := From.M;

         when APPSCHED_EXPLICIT_CALL =>
            To.User_Event_Code := From.User_Event_Code;

         when APPSCHED_EXPLICIT_CALL_WITH_DATA =>
            To.Msg      := From.Info;
            To.Msg_Size := From.Info_Size;

         when APPSCHED_NEW | APPSCHED_TERMINATE | APPSCHED_READY |
           APPSCHED_BLOCK | APPSCHED_YIELD | APPSCHED_CHANGE_SCHED_PARAM |
           APPSCHED_TIMEOUT | APPSCHED_TASK_NOTIFICATION =>
            null;
      end case;

      --  Release the event object
      TO_APPSCHD.Release_Event (From);
   end Copy_Event;
   pragma Inline (Copy_Event);

   ---------------------
   -- Execute_Actions --
   ---------------------
   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Event         : out Scheduling_Event) is
   begin
      Execute_Actions (Sched_Actions,
                       POSIX.Signals.Implementation.Null_Signal_Set,
                       Event);
   end Execute_Actions;

   ---------------------
   -- Execute_Actions --
   ---------------------
   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Set           : in  POSIX_Signals.Signal_Set;
                              Event         : out Scheduling_Event) is
      Flags : Integer;
      MaRTE_Event_Ac : aliased APPSCHD_DATA.AppSched_Event_Ac;
      Error : MaRTE.Kernel.Error_Code;
   begin
      TOI.Reset_POSIX_Error;

      MaRTE.Kernel.Enter_Critic_Section (Flags);

      --  Execute Scheduling Actions
      TO_APPSCHD.Execute_Actions (To_Kernel_Sched_Actions_Set (Sched_Actions),
                                  Error);
      PI.Check_NZ (Error, Flags);

      --  Wait for event
      if (POSIX.Signals."="
          (Set, POSIX.Signals.Implementation.Null_Signal_Set)) then
         TO_APPSCHD.Get_Event (MaRTE_Event_Ac, null);
      else
         TO_APPSCHD.Get_Event
           (MaRTE_Event_Ac,
            POSIX.Signals.Implementation.Get_MSignal_Set_Ref (Set));
      end if;

      MaRTE.Kernel.Leave_Critic_Section (Flags);

      Copy_Event (From => MaRTE_Event_Ac, To => Event);
   end Execute_Actions;
   pragma Inline (Execute_Actions);

   ---------------------
   -- Execute_Actions --
   ---------------------
   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Event         : out Scheduling_Event;
                              Current_Time  : out POSIX.Timespec) is
   begin
      Execute_Actions (Sched_Actions,
                       POSIX.Signals.Implementation.Null_Signal_Set,
                       Event,
                       Current_Time);
   end Execute_Actions;

   ---------------------
   -- Execute_Actions --
   ---------------------
   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Set           : in  POSIX_Signals.Signal_Set;
                              Event         : out Scheduling_Event;
                              Current_Time  : out POSIX.Timespec) is
      Flags : Integer;
      MaRTE_Event_Ac : aliased APPSCHD_DATA.AppSched_Event_Ac;
      Error : MaRTE.Kernel.Error_Code;
      use type HWI.HWTime;
      use TMRS;
   begin
      TOI.Reset_POSIX_Error;

      MaRTE.Kernel.Enter_Critic_Section (Flags);

      --  Execute Scheduling Actions
      TO_APPSCHD.Execute_Actions (To_Kernel_Sched_Actions_Set (Sched_Actions),
                                  Error);
      PI.Check_NZ (Error, Flags);

      --  Wait for event
      if (POSIX.Signals."="
          (Set, POSIX.Signals.Implementation.Null_Signal_Set)) then
         TO_APPSCHD.Get_Event (MaRTE_Event_Ac, null);
      else
         TO_APPSCHD.Get_Event
           (MaRTE_Event_Ac,
            POSIX.Signals.Implementation.Get_MSignal_Set_Ref (Set));
      end if;

      --  Get current time
      pragma Assert (AS (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_REALTIME or
                     AS (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_MONOTONIC);
      if AS (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_REALTIME then
         --  Realtime Clock
         Current_Time := HWI.HWTime_To_Duration (HWI.Get_HWTime +
                                                 TMRS.Realtime_Clock_Offset);
      else
         --  Monotonic Clock
         Current_Time := HWI.HWTime_To_Duration (HWI.Get_HWTime);
      end if;

      MaRTE.Kernel.Leave_Critic_Section (Flags);

      Copy_Event (From => MaRTE_Event_Ac, To => Event);
   end Execute_Actions;
   pragma Inline (Execute_Actions);

   ----------------------------------
   -- Execute_Actions_With_Timeout --
   ----------------------------------
   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec) is
   begin
      Execute_Actions_With_Timeout
        (Sched_Actions,
         POSIX.Signals.Implementation.Null_Signal_Set,
         Event,
         Timeout);
   end Execute_Actions_With_Timeout;

   ----------------------------------
   -- Execute_Actions_With_Timeout --
   ----------------------------------
   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Set           : in  POSIX_Signals.Signal_Set;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec) is
      Flags : Integer;
      MaRTE_Event_Ac : aliased APPSCHD_DATA.AppSched_Event_Ac;
      Error : MaRTE.Kernel.Error_Code;
   begin
      TOI.Reset_POSIX_Error;

      MaRTE.Kernel.Enter_Critic_Section (Flags);

      --  Execute Scheduling Actions
      TO_APPSCHD.Execute_Actions (To_Kernel_Sched_Actions_Set (Sched_Actions),
                                  Error);
      PI.Check_NZ (Error, Flags);

      --  Wait for event
      if (POSIX.Signals."="
          (Set, POSIX.Signals.Implementation.Null_Signal_Set)) then
         TO_APPSCHD.Get_Event (MaRTE_Event_Ac, null,
                               HWI.Duration_To_HWTime (Timeout));
      else
         TO_APPSCHD.Get_Event
           (MaRTE_Event_Ac,
            POSIX.Signals.Implementation.Get_MSignal_Set_Ref (Set),
            HWI.Duration_To_HWTime (Timeout));
      end if;

      MaRTE.Kernel.Leave_Critic_Section (Flags);

      Copy_Event (From => MaRTE_Event_Ac, To => Event);
   end Execute_Actions_With_Timeout;
   pragma Inline (Execute_Actions_With_Timeout);

   ----------------------------------
   -- Execute_Actions_With_Timeout --
   ----------------------------------
   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec;
      Current_Time  : out POSIX.Timespec) is
   begin
      Execute_Actions_With_Timeout
        (Sched_Actions,
         POSIX.Signals.Implementation.Null_Signal_Set,
         Event,
         Timeout,
         Current_Time);
   end Execute_Actions_With_Timeout;

   ----------------------------------
   -- Execute_Actions_With_Timeout --
   ----------------------------------
   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Set           : in  POSIX_Signals.Signal_Set;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec;
      Current_Time  : out POSIX.Timespec) is
      Flags : Integer;
      MaRTE_Event_Ac : aliased APPSCHD_DATA.AppSched_Event_Ac;
      Error : MaRTE.Kernel.Error_Code;
      use type HWI.HWTime;
      use TMRS;
   begin
      TOI.Reset_POSIX_Error;

      MaRTE.Kernel.Enter_Critic_Section (Flags);

      --  Execute Scheduling Actions
      TO_APPSCHD.Execute_Actions (To_Kernel_Sched_Actions_Set (Sched_Actions),
                                  Error);
      PI.Check_NZ (Error, Flags);

      --  Wait for event
      if (POSIX.Signals."="
          (Set, POSIX.Signals.Implementation.Null_Signal_Set)) then
         TO_APPSCHD.Get_Event (MaRTE_Event_Ac, null,
                               HWI.Duration_To_HWTime (Timeout));
      else
         TO_APPSCHD.Get_Event
           (MaRTE_Event_Ac,
            POSIX.Signals.Implementation.Get_MSignal_Set_Ref (Set),
            HWI.Duration_To_HWTime (Timeout));
      end if;

      --  Get current time
      pragma Assert (AS (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_REALTIME or
                     AS (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_MONOTONIC);
      if AS (SCHD.Self.AppScheduler).Clock = TMRS.CLOCK_REALTIME then
         --  Realtime Clock
         Current_Time := HWI.HWTime_To_Duration (HWI.Get_HWTime +
                                                 TMRS.Realtime_Clock_Offset);
      else
         --  Monotonic Clock
         Current_Time := HWI.HWTime_To_Duration (HWI.Get_HWTime);
      end if;

      MaRTE.Kernel.Leave_Critic_Section (Flags);

      Copy_Event (From => MaRTE_Event_Ac, To => Event);
   end Execute_Actions_With_Timeout;
   pragma Inline (Execute_Actions_With_Timeout);

   ----------------------------------------------------------------------------
   -- Become an application scheduler task ------------------------------------
   ----------------------------------------------------------------------------
   procedure Become_An_Application_Scheduler is
      Flags : Integer;
      AppSched_Data : APPSCHD_DATA.AppScheduler_Data_Ac;
      use type MaRTE.Kernel.AppScheduler_Data_Base_Ac;
   begin
      TOI.Reset_POSIX_Error;
      MaRTE.Kernel.Enter_Critic_Section (Flags);

      if (SCHD.Self.AppScheduler /= null or else
          SCHD.Self.Sched_Policy =  MaRTE.Kernel.SCHED_APP) then
         MaRTE.Kernel.Leave_Critic_Section (Flags);
         PI.Raise_POSIX_Error (POSIX.Operation_Not_Supported);
      end if;

      AppSched_Data := APPSCHD_DATA.Request_AppScheduler_Data;
      if APPSCHD_DATA.AppScheduler_Data_Lists."=" (AppSched_Data, null) then
         --  Impossible create more application scheduler tasks
         MaRTE.Kernel.Leave_Critic_Section (Flags);
         PI.Raise_POSIX_Error (POSIX.Resource_Temporarily_Unavailable);
      end if;
      SCHD.Self.AppScheduler :=
        MaRTE.Kernel.AppScheduler_Data_Base_Ac (AppSched_Data);

      MaRTE.Kernel.Leave_Critic_Section (Flags);
   end Become_An_Application_Scheduler;

   ----------------------------------------------------------------------------
   -- Scheduler Task Attributes -----------------------------------------------
   ----------------------------------------------------------------------------

   ---------------
   -- Set_Clock --
   ---------------
   procedure Set_Clock (Clock : in POSIX_Timers.Clock_Id) is
   begin
      TOI.Reset_POSIX_Error;
      if POSIX_Timers."/=" (Clock, POSIX_Timers.Clock_Realtime) then
         TOI.Set_POSIX_Error (MaRTE.Kernel.INVALID_ARGUMENT);
      end if;
   end Set_Clock;

   ---------------
   -- Get_Clock --
   ---------------
   function Get_Clock return POSIX_Timers.Clock_Id is
   begin
      TOI.Reset_POSIX_Error;
      return POSIX_Timers.Clock_Realtime;
   end Get_Clock;

   ---------------
   -- Set_Flags --
   ---------------
   procedure Set_Flags (Flags : in Scheduler_Flags) is
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appschedattr_SetFlags (POSIX.Option_Set (Flags)));
   end Set_Flags;

   ---------------
   -- Get_Flags --
   ---------------
   function Get_Flags return Scheduler_Flags is
      Flags : aliased POSIX.Option_Set;
   begin
      PI.Check_NZ (TO_APPSCHD.POSIX_Appschedattr_GetFlags (Flags'Access));
      return Scheduler_Flags (Flags);
   end Get_Flags;

   ----------
   -- Fill --
   ----------
   procedure Fill (Mask : in out Event_Mask) is
   begin
      PI.Check_NZ (TO_APPSCHD.POSIX_Appsched_Fillset (Mask.Mask'Access));
   end Fill;

   -----------
   -- Empty --
   -----------
   procedure Empty (Mask : in out Event_Mask) is
   begin
      PI.Check_NZ (TO_APPSCHD.POSIX_Appsched_Emptyset (Mask.Mask'Access));
   end Empty;

   ---------
   -- Add --
   ---------
   procedure Add (Mask : in out Event_Mask; Event : in Event_Code) is
   begin
      PI.Check_NZ (TO_APPSCHD.POSIX_Appsched_Addset (Mask.Mask'Access, Event));
   end Add;

   ---------
   -- Del --
   ---------
   procedure Del (Mask : in out Event_Mask; Event : in Event_Code) is
   begin
      PI.Check_NZ (TO_APPSCHD.POSIX_Appsched_Delset (Mask.Mask'Access, Event));
   end Del;

   --------------
   -- Ismember --
   --------------
   function Ismember (Mask : in Event_Mask; Event : in Event_Code)
                      return Boolean is
   begin
      return TO_APPSCHD.POSIX_Appsched_Ismember (Mask.Mask, Event) = 0;
   end Ismember;

   --------------------
   -- Set_Event_Mask --
   --------------------
   procedure Set_Event_Mask (Mask : in Event_Mask) is
   begin
      PI.Check_NZ (TO_APPSCHD.POSIX_Appschedattr_Seteventmask (Mask.Mask));
   end Set_Event_Mask;

   --------------------
   -- Get_Event_Mask --
   --------------------
   function Get_Event_Mask return Event_Mask is
      Mask : aliased Event_Mask;
   begin
      PI.Check_NZ
        (TO_APPSCHD.POSIX_Appschedattr_Geteventmask (Mask.Mask'Access));
      return Mask;
   end Get_Event_Mask;

   ----------------------------------------------------------------------------
   -- Change Task Policy ------------------------------------------------------
   ----------------------------------------------------------------------------

   --------------------------------------------------
   -- Change_Task_Policy_To_FIFO_Within_Priorities --
   --------------------------------------------------
   procedure Change_Task_Policy_To_FIFO_Within_Priorities is
      Sched_Param : aliased TO.Sched_Param;
      Policy      : aliased TO.Scheduling_Policies;
      Flags : Integer;
   begin
      TOI.Reset_POSIX_Error;
      MaRTE.Kernel.Enter_Critic_Section (Flags);

      --  Get current scheduling parameters
      PI.Check_NZ (TO.Pthread_Getschedparam (SCHD.Self,
                                             Policy'Access,
                                             Sched_Param'Access),
                   Flags);
      --  Change policy in the scheduling parameters
      PI.Check_NZ (TO.Pthread_Setschedparam
                            (SCHD.Self, MaRTE.Kernel.SCHED_FIFO, Sched_Param'Access),
                   Flags);

      MaRTE.Kernel.Leave_Critic_Section (Flags);
   end Change_Task_Policy_To_FIFO_Within_Priorities;

   -------------------------------------
   -- Change_Task_Policy_To_App_Sched --
   -------------------------------------
   procedure Change_Task_Policy_To_App_Sched
     (Scheduler : in Ada.Task_Identification.Task_Id) is
      Sched_Param : aliased TO.Sched_Param;
      Policy      : aliased TO.Scheduling_Policies;
      Flags : Integer;
   begin
      TOI.Reset_POSIX_Error;
      MaRTE.Kernel.Enter_Critic_Section (Flags);
      --  Set appscheduler attribute
      PI.Check_NZ (TO_APPSCHD.Pthread_Setappscheduler
                                 (SCHD.Self, PI.To_Kernel_Task_Id (Scheduler)),
                   Flags);
      --  Set null specific parameters
      PI.Check_NZ (TO_APPSCHD.Pthread_Setappschedparam (SCHD.Self, null, 0),
                   Flags);
      --  Get current scheduling parameters
      PI.Check_NZ (TO.Pthread_Getschedparam (SCHD.Self,
                                             Policy'Access,
                                             Sched_Param'Access),
                   Flags);
      --  Set new policy
      PI.Check_NZ (TO.Pthread_Setschedparam (SCHD.Self,
                                             MaRTE.Kernel.SCHED_APP,
                                             Sched_Param'Access),
                   Flags);
      MaRTE.Kernel.Leave_Critic_Section (Flags);
   end Change_Task_Policy_To_App_Sched;


   --------------------------------
   -- Application_Defined_Policy --
   --------------------------------
   package body Application_Defined_Policy is

      function To_Kernel_AppSched_Param_T_Ac is
        new Ada.Unchecked_Conversion (System.Address,
                                      MaRTE.Kernel.AppSched_Param_T_Ac);

      ------------------------
      -- Change_Task_Policy --
      ------------------------
      procedure Change_Task_Policy
        (Scheduler : in Ada.Task_Identification.Task_Id;
         Param     : in Parameters) is
         Sched_Param : aliased TO.Sched_Param;
         Policy      : aliased TO.Scheduling_Policies;
         Flags : Integer;
      begin
         TOI.Reset_POSIX_Error;
         MaRTE.Kernel.Enter_Critic_Section (Flags);
         --  Set appscheduler attribute
         PI.Check_NZ (TO_APPSCHD.Pthread_Setappscheduler
                                 (SCHD.Self, PI.To_Kernel_Task_Id (Scheduler)),
                      Flags);
         --  Set specific parameters
         PI.Check_NZ (TO_APPSCHD.Pthread_Setappschedparam
                               (SCHD.Self,
                                To_Kernel_AppSched_Param_T_Ac (Param'Address),
                                MaRTE.Kernel.AppSched_Param_Size_T (Param'Size / 8)),
                      Flags);
         --  Get current scheduling parameters
         PI.Check_NZ (TO.Pthread_Getschedparam (SCHD.Self,
                                                Policy'Access,
                                                Sched_Param'Access),
                      Flags);
         --  Set new policy
         PI.Check_NZ (TO.Pthread_Setschedparam (SCHD.Self,
                                                MaRTE.Kernel.SCHED_APP,
                                                Sched_Param'Access),
                      Flags);
         MaRTE.Kernel.Leave_Critic_Section (Flags);
      end Change_Task_Policy;

      --------------------
      -- Get_Parameters --
      --------------------
      procedure Get_Parameters
        (T     : in  Ada.Task_Identification.Task_Id;
         Param : out Parameters) is

         use MaRTE.Kernel;
         Param_Ac : MaRTE.Kernel.AppSched_Param_T_Ac :=
           To_Kernel_AppSched_Param_T_Ac (Param'Address);
         Flags : Integer;
      begin
         MaRTE.Kernel.Enter_Critic_Section (Flags);
         if (Integer (SA (PI.To_Kernel_Task_Id (T).Sched_App).Param_Size) /=
             Param'Size / 8) then
            MaRTE.Kernel.Leave_Critic_Section (Flags);
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
         end if;
         TOI.Reset_POSIX_Error;

         --  Copy the TCB's field 'AppSched_Param'
         Param_Ac.all
           (1 .. MaRTE.Kernel.AppSched_Param_Size_T (Param'Size / 8)) :=
           SA (PI.To_Kernel_Task_Id (T).Sched_App).Param
           (1 .. MaRTE.Kernel.AppSched_Param_Size_T (Param'Size / 8));
         MaRTE.Kernel.Leave_Critic_Section (Flags);
      end Get_Parameters;

   end Application_Defined_Policy;


   ----------------------------------------------------------------------------
   -- Explicit Scheduler Invocation -------------------------------------------
   ----------------------------------------------------------------------------

   ------------
   -- Invoke --
   ------------
   procedure Invoke_Scheduler (User_Event_Code : in Integer) is
   begin
      PI.Check_NZ (TO_APPSCHD.Invoke_Scheduler (User_Event_Code));
   end Invoke_Scheduler;

   package body Explicit_Scheduler_Invocation is

      ------------
      -- Invoke --
      ------------
      procedure Invoke (Msg : in Message) is
      begin
         PI.Check_NZ
           (TO_APPSCHD.Invoke_Scheduler_With_Data (Msg'Address, Msg'Size / 8,
                                                   Reply => null,
                                                   Reply_Size => null));
      end Invoke;

      -------------------------
      -- Invoke (with reply) --
      -------------------------
      procedure Invoke (Msg   : in  Message;
                        Reply : out Scheduler_Reply) is
         function To_Marte_Reply_Ac is
            new Ada.Unchecked_Conversion (System.Address,
                                          APPSCHD_DATA.Scheduler_Reply_Ac);
         Marte_Reply      : aliased APPSCHD_DATA.Scheduler_Reply_T;
         Marte_Reply_Size : aliased APPSCHD_DATA.Scheduler_Reply_size_T;
      begin
         PI.Check_NZ (TO_APPSCHD.Invoke_Scheduler_With_Data
                                          (Msg'Address, Msg'Size / 8,
                                           Marte_Reply'Unchecked_Access,
                                           Marte_Reply_Size'Unchecked_Access));
         if Integer (Marte_Reply_Size) /= Scheduler_Reply'Size / 8 then
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
         end if;

         To_Marte_Reply_Ac (Reply'Address) (1 .. Marte_Reply_Size) :=
           Marte_Reply (1 .. Marte_Reply_Size);
      end Invoke;

      -----------------
      -- Get_Message --
      -----------------
      function Get_Message (Event : in Scheduling_Event)
                            return Message is
         type Message_Ac is access all Message;
         function To_Message_Ac is
           new Ada.Unchecked_Conversion (System.Address, Message_Ac);
      begin
         PI.Check (Event.Code = APPSCHED_EXPLICIT_CALL_WITH_DATA and then
                   Integer (Event.Msg_Size) = Message'Size / 8,
                   POSIX.Invalid_Argument);
         TOI.Reset_POSIX_Error;
         return To_Message_Ac (Event.Msg).all;
      end Get_Message;

      ---------------
      -- Set_Reply --
      ---------------
      procedure Set_Reply (Reply : in Scheduler_Reply) is
         function To_Marte_Reply_Size_T is
            new Ada.Unchecked_Conversion (Integer,
                                          APPSCHD_DATA.Scheduler_Reply_Size_T);
         type Reply_Ac is access all Scheduler_Reply;
         function To_Marte_Reply_Ac is
            new Ada.Unchecked_Conversion (Reply_Ac,
                                          APPSCHD_DATA.Scheduler_Reply_Ac);
      begin
         PI.Check (TO_APPSCHD.POSIX_Appschedattr_Setreplyinfo
                   (To_Marte_Reply_Ac (Reply'Unrestricted_Access),
                    To_Marte_Reply_Size_T (Scheduler_Reply'Size / 8)));
      end Set_Reply;

   end Explicit_Scheduler_Invocation;

end POSIX_Application_Scheduling;
