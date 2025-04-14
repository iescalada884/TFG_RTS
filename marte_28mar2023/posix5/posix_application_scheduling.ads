------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--           'P O S I X _ A p p l i c a t i o n _ S c h e d u l i n g'
--
--                                  Spec
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
with System;
with Ada.Task_Identification;

with POSIX;
with POSIX_Mutexes;
with POSIX_Timers;
with POSIX_Signals;

with MaRTE.Kernel.Application_Scheduling_Data;
with MaRTE.Kernel.Tasks_Operations.Application_Scheduler;
with MaRTE.Integer_Types;

package POSIX_Application_Scheduling is

   package APPSCHD_DATA renames MaRTE.Kernel.Application_Scheduling_Data;

   ------------------
   -- Events Codes --
   ------------------
   subtype Event_Code is APPSCHD_DATA.Event_Code_T;
   NEW_TASK                 : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_NEW;
   TERMINATE_TASK           : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_TERMINATE;
   READY                    : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_READY;
   BLOCK                    : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_BLOCK;
   YIELD                    : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_YIELD;
   SIGNAL                   : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_SIGNAL;
   CHANGE_SCHED_PARAM       : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_CHANGE_SCHED_PARAM;
   EXPLICIT_CALL            : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_EXPLICIT_CALL;
   EXPLICIT_CALL_WITH_DATA  : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_EXPLICIT_CALL_WITH_DATA;
   TIMEOUT                  : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_TIMEOUT;
   PRIORITY_INHERIT         : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_PRIORITY_INHERIT;
   PRIORITY_UNINHERIT       : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_PRIORITY_UNINHERIT;
   INIT_MUTEX               : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_INIT_MUTEX;
   DESTROY_MUTEX            : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_DESTROY_MUTEX;
   LOCK_MUTEX               : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_LOCK_MUTEX;
   TRY_LOCK_MUTEX           : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_TRY_LOCK_MUTEX;
   UNLOCK_MUTEX             : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_UNLOCK_MUTEX;
   BLOCK_AT_MUTEX           : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_BLOCK_AT_MUTEX;
   CHANGE_SCHED_PARAM_MUTEX : constant Event_Code :=
     APPSCHD_DATA.APPSCHED_CHANGE_SCHED_PARAM_MUTEX;

   -----------------------
   -- Scheduling Events --
   -----------------------
   type Scheduling_Event is private;

   function Get_Event_Code (Event : in Scheduling_Event)
                            return Event_Code;
   function Get_Task (Event : in Scheduling_Event)
                      return Ada.Task_Identification.Task_Id;
   function Get_Sched_Priority (Event : in Scheduling_Event)
                                return System.Any_Priority;
   function Get_Signal_Info (Event : in Scheduling_Event)
                            return POSIX_Signals.Signal_Info;
   function Get_User_Event_Code (Event : in Scheduling_Event)
                                return Integer;
   function Get_Mutex (Event : in Scheduling_Event)
                       return POSIX_Mutexes.Mutex_Descriptor;

   ------------------------
   -- Scheduling actions --
   ------------------------
   type Scheduling_Actions is private;

   procedure Initialize (Sched_Actions : in out Scheduling_Actions);
   procedure Destroy (Sched_Actions : in out Scheduling_Actions);
   procedure Add_Accept
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id);
   procedure Add_Reject
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id);
   procedure Add_Activate
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id);
   procedure Add_Suspend
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id);
   procedure Add_Accept_Mutex
     (Sched_Actions : in out Scheduling_Actions;
      M             : in     POSIX_Mutexes.Mutex_Descriptor);
   procedure Add_Reject_Mutex
     (Sched_Actions : in out Scheduling_Actions;
      M             : in     POSIX_Mutexes.Mutex_Descriptor);
   procedure Add_Lock_Mutex
     (Sched_Actions : in out Scheduling_Actions;
      T             : in     Ada.Task_Identification.Task_Id;
      M             : in     POSIX_Mutexes.Mutex_Descriptor);

   --------------------------------
   -- Execute Scheduling Actions --
   --------------------------------
   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Event         : out Scheduling_Event);

   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Set           : in  POSIX_Signals.Signal_Set;
                              Event         : out Scheduling_Event);

   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Event         : out Scheduling_Event;
                              Current_Time  : out POSIX.Timespec);

   procedure Execute_Actions (Sched_Actions : in  Scheduling_Actions;
                              Set           : in  POSIX_Signals.Signal_Set;
                              Event         : out Scheduling_Event;
                              Current_Time  : out POSIX.Timespec);

   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec);

   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Set           : in  POSIX_Signals.Signal_Set;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec);

   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec;
      Current_Time  : out POSIX.Timespec);

   procedure Execute_Actions_With_Timeout
     (Sched_Actions : in  Scheduling_Actions;
      Set           : in  POSIX_Signals.Signal_Set;
      Event         : out Scheduling_Event;
      Timeout       : in  POSIX.Timespec;
      Current_Time  : out POSIX.Timespec);

   ------------------------------------------
   -- Become an application scheduler task --
   ------------------------------------------
   procedure Become_An_Application_Scheduler;

   -------------------------------
   -- Scheduler Task Attributes --
   -------------------------------
   procedure Set_Clock (Clock : in POSIX_Timers.Clock_Id);
   function Get_Clock return POSIX_Timers.Clock_Id;

   type Scheduler_Flags is new POSIX.Option_Set;
   ABSOLUTE_TIMEOUT : constant Scheduler_Flags;
   procedure Set_Flags (Flags : in Scheduler_Flags);
   function Get_Flags return Scheduler_Flags;

   type Event_Mask is private;
   procedure Fill (Mask : in out Event_Mask);
   procedure Empty (Mask : in out Event_Mask);
   procedure Add (Mask : in out Event_Mask; Event : in Event_Code);
   procedure Del (Mask : in out Event_Mask; Event : in Event_Code);
   function Ismember (Mask : in Event_Mask; Event : in Event_Code)
                      return Boolean;
   procedure Set_Event_Mask (Mask : in Event_Mask);
   function Get_Event_Mask return Event_Mask;

   -----------------------------------
   -- Change Task Scheduling Policy --
   -----------------------------------
   procedure Change_Task_Policy_To_FIFO_Within_Priorities;

   procedure Change_Task_Policy_To_App_Sched
     (Scheduler : in Ada.Task_Identification.Task_Id);

   generic
      type Parameters is private;
   package Application_Defined_Policy is

      procedure Change_Task_Policy
        (Scheduler : in Ada.Task_Identification.Task_Id;
         Param     : in Parameters);
      procedure Get_Parameters
        (T     : in  Ada.Task_Identification.Task_Id;
         Param : out Parameters);

   end Application_Defined_Policy;

   -----------------------------------
   -- Explicit Scheduler Invocation --
   -----------------------------------
   procedure Invoke_Scheduler (User_Event_Code : in Integer);

   generic
      type Message is private;
      type Scheduler_Reply is private;
   package Explicit_Scheduler_Invocation is

      procedure Invoke (Msg : in Message);
      procedure Invoke (Msg   : in  Message;
                        Reply : out Scheduler_Reply);

      function Get_Message (Event : in Scheduling_Event) return Message;

      procedure Set_Reply (Reply : in Scheduler_Reply);

   end Explicit_Scheduler_Invocation;

private
   package TO_APPSCHD renames MaRTE.Kernel.Tasks_Operations.Application_Scheduler;

   ---------------------------
   -- type Scheduling_Event --
   ---------------------------
   type Scheduling_Event is record
      Code            : Event_Code;
      T               : Ada.Task_Identification.Task_Id;
      Sched_Priority  : System.Any_Priority;
      Siginfo         : POSIX_Signals.Signal_Info;
      M               : POSIX_Mutexes.Mutex_Descriptor;
      Msg             : System.Address;
      Msg_Size        : MaRTE.Integer_Types.Int;
      User_Event_Code : MaRTE.Integer_Types.Int;
   end record;

   -------------------------------
   -- Scheduler Flags Constants --
   -------------------------------
   ABSOLUTE_TIMEOUT : constant Scheduler_Flags
     := Scheduler_Flags (APPSCHD_DATA.APPSCHED_ABSTIMEOUT);

   -----------------------------
   -- type Scheduling_Actions --
   -----------------------------
   type Scheduling_Actions is record
      Actions : aliased TO_APPSCHD.Sched_Actions_Set;
   end record;

   ---------------------
   -- type Event_Mask --
   ---------------------
   type Event_Mask is record
      Mask : aliased POSIX.Option_Set;
   end record;

end POSIX_Application_Scheduling;
