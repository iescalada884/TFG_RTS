------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                 'R o u n d _ R o b i n _ S c h e d u l i n g'
--
--                                   Body
--
--
--  File 'round_robin_scheduling.adb'                                  By MAR.
--
--
--  This package instantiates a Round Robin scheduler task using the
--  Application-Defined Scheduling Interface implemented in MaRTE OS.
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
with POSIX_Application_Scheduling; use POSIX_Application_Scheduling;
with POSIX;
with POSIX_Timers;
with POSIX_Signals;
with Ada.Task_Identification;
with Ada.Task_Attributes;
with Ada.Unchecked_Conversion;
with Ada.Calendar; use Ada.Calendar;

with Ada.Exceptions;
with Text_IO; use Text_IO;
package body Round_Robin_Scheduling is

   package Duration_IO is new Text_IO.Fixed_IO (Duration); use Duration_IO;

   ---------------------------------------------
   -- RR tasks application-defined attributes --
   ---------------------------------------------
   type RR_Attr is record
      Quantum : Duration;
   end record;
   package RR_Policy is
     new POSIX_Application_Scheduling.Application_Defined_Policy (RR_Attr);

   -------------------------------------
   -- Round-Robin Tasks Specific Data --
   -------------------------------------
   type RR_Data is tagged record
      Quantum : POSIX.Timespec;
      System_Resources_Held_count : Integer;
      Quantum_Spired : Boolean;
      Cputime_Timer : POSIX_Timers.Timer_Id;
   end record;
   type RR_Data_Ac is access all RR_Data;
   package RR_Specific is new Ada.Task_Attributes (RR_Data_Ac, null);

   -----------------------------------------------------
   -- Conversions between 'Signal_Data' and 'Task_Id' --
   -----------------------------------------------------
   function To_Signal_Data is
     new Ada.Unchecked_Conversion (Ada.Task_Identification.Task_Id,
                                   POSIX_Signals.Signal_Data);
   function To_Task_Id is
     new Ada.Unchecked_Conversion (POSIX_Signals.Signal_Data,
                                   Ada.Task_Identification.Task_Id);

   -----------------------
   -- RR_Scheduler task --
   -----------------------
   RR_Scheduler_Id : Ada.Task_Identification.Task_Id;

   task RR_Scheduler is
      pragma Priority (Max_Round_Robin_Priority);
   end RR_Scheduler;
   task body RR_Scheduler is
      Actions : POSIX_Application_Scheduling.Scheduling_Actions;
      Event : POSIX_Application_Scheduling.Scheduling_Event;
      RRdata_Ac : RR_Data_Ac;
      RRTask_Id : Ada.Task_Identification.Task_Id;
      RRattr : RR_Attr;
      Signals_Waited : POSIX_Signals.Signal_Set;
      Sigevent : POSIX_Signals.Signal_Event;
      Timerstate : POSIX_Timers.Timer_State;
      Relative_Timer_Options : POSIX_Timers.Timer_Options;
      Unnecessary_Events : POSIX_Application_Scheduling.Event_Mask;
      Associated_Tasks : Integer := 0;
      Start_Time : Time;
   begin
      RR_Scheduler_Id := Ada.Task_Identification.Current_Task;

      --  Initialize 'Signals_Waited' and 'Sigevent'.
      POSIX_Signals.Add_Signal (Signals_Waited, POSIX_Signals.Signal_User_1);
      POSIX_Signals.Set_Signal (Sigevent, POSIX_Signals.Signal_User_1);
      POSIX_Signals.Set_Notification (Sigevent,
                                      POSIX_Signals.Signal_Notification);

      --  Becomes an application scheduler task
      POSIX_Application_Scheduling.Become_An_Application_Scheduler;

      --  Discard unnecessary events
      POSIX_Application_Scheduling.Fill (Unnecessary_Events);
      POSIX_Application_Scheduling.Del (Unnecessary_Events, NEW_TASK);
      POSIX_Application_Scheduling.Del (Unnecessary_Events, SIGNAL);
      POSIX_Application_Scheduling.Del (Unnecessary_Events, PRIORITY_INHERIT);
      POSIX_Application_Scheduling.Del (Unnecessary_Events,
                                        PRIORITY_UNINHERIT);
      POSIX_Application_Scheduling.Del (Unnecessary_Events, EXPLICIT_CALL);
      POSIX_Application_Scheduling.Del (Unnecessary_Events, TERMINATE_TASK);
      POSIX_Application_Scheduling.Set_Event_Mask (Unnecessary_Events);


      --  Initialize the actions object
      POSIX_Application_Scheduling.Initialize (Actions);

      --  Get starting time
      Start_Time := Clock;

      --  Events loop
      loop

         --  Execute scheduling actions and wait for next event
         POSIX_Application_Scheduling.Execute_Actions (Actions,
                                                       Signals_Waited,
                                                       Event);
         --  Reset the actions object
         POSIX_Application_Scheduling.Destroy (Actions);
         POSIX_Application_Scheduling.Initialize (Actions);

         case POSIX_Application_Scheduling.Get_Event_Code (Event) is

            when NEW_TASK =>
               New_Line; Put ("New task at ");
               Put (Clock - Start_Time, 4, 2); New_Line;

               Associated_Tasks := Associated_Tasks + 1;
               --  Get the task attributes
               RR_Policy.Get_Parameters (Get_Task (Event), RRattr);
               --  Create the task specific data
               RRdata_Ac := new RR_Data;

               --  Initialize Quantum
               RRdata_Ac.Quantum := RRattr.Quantum;
               RRdata_Ac.Quantum_Spired := False;

               --  Initialize System_Resources_Held_count
               RRdata_Ac.System_Resources_Held_count := 0;

               --  Inititalize Cputime_Timer (The data associated with
               --  the timer signal is the task Id)
               POSIX_Signals.Set_Data
                 (Sigevent, To_Signal_Data (Get_Task (Event)));
               RRdata_Ac.Cputime_Timer :=
                 POSIX_Timers.Create_Timer
                 (POSIX_Timers.Get_Cpuclock_Id (Get_Task (Event)), Sigevent);

               --  Set the specific data
               RR_Specific.Set_Value (RRdata_Ac, Get_Task (Event));

               --  Accept task
               POSIX_Application_Scheduling.Add_Accept (Actions,
                                                        Get_Task (Event));

               --  Reactivate task and arm its CPU timer
               POSIX_Application_Scheduling.Add_Activate (Actions,
                                                          Get_Task (Event));
               POSIX_Timers.Set_Initial (Timerstate, RRdata_Ac.Quantum);
               POSIX_Timers.Set_Interval (Timerstate, RRdata_Ac.Quantum);
               POSIX_Timers.Arm_Timer (RRdata_Ac.Cputime_Timer,
                                       Relative_Timer_Options,
                                       Timerstate);

            when SIGNAL => --  RR slice finished
               New_Line; Put ("Task finishes quantum at ");
               Put (Clock - Start_Time, 4, 2); New_Line;
               --  Get the task id
               RRTask_id := To_Task_Id
                 (POSIX_Signals.Get_Data (Get_Signal_Info (Event)));
               --  Get the task specific data
               RRdata_Ac := RR_Specific.Value (RRTask_id);
               --  Is the task holding systems resources ?
               if RRdata_Ac.System_Resources_Held_Count = 0 then
                  --  No system resources held: the tast can be put at the
                  --  tail of its priority queue
                  POSIX_Application_Scheduling.Add_Suspend (Actions,
                                                            RRTask_id);
                  POSIX_Application_Scheduling.Add_Activate (Actions,
                                                             RRTask_id);
               else
                  --  The task will be put at the tail of its priority queue
                  --  as soon as it releases all the system resources.
                  RRdata_Ac.Quantum_Spired := True;
               end if;

            when PRIORITY_INHERIT =>
               --  Get the task specific data
               RRdata_Ac := RR_Specific.Value (Get_Task (Event));
               --  Increment the resources held count
               RRdata_Ac.System_Resources_Held_Count :=
                 RRdata_Ac.System_Resources_Held_Count + 1;

               --  Reactivate task
               POSIX_Application_Scheduling.Add_Activate (Actions,
                                                          Get_Task (Event));

            when PRIORITY_UNINHERIT =>
               begin
               --  Get the task specific data
               RRdata_Ac := RR_Specific.Value (Get_Task (Event));
               --  Decrement the resources held count
               RRdata_Ac.System_Resources_Held_Count :=
                 RRdata_Ac.System_Resources_Held_Count - 1;
               if (RRdata_Ac.System_Resources_Held_Count = 0 and then
                   RRdata_Ac.Quantum_Spired) then
                  --  The task quantum had spired while it was holding some
                  --  system resource.
                  POSIX_Application_Scheduling.Add_Suspend (Actions,
                                                            Get_Task (Event));
                  POSIX_Application_Scheduling.Add_Activate (Actions,
                                                             Get_Task (Event));
                  RRdata_Ac.Quantum_Spired := False;
               end if;
               exception
                  when Program_Error =>
                     --  Raised in 'RR_Specific.Value': a task is
                     --  marked as "terminated" before having released
                     --  all the mutexes used internally by GNARL.
                     null;
               end;

               --  Reactivate task
               POSIX_Application_Scheduling.Add_Activate (Actions,
                                                          Get_Task (Event));
            when EXPLICIT_CALL => --  Task finishes its execution
               New_Line; Put ("Task finishes execution at ");
               Put (Clock - Start_Time, 4, 2); New_Line;
               --  Get the task specific data
               RRdata_Ac := RR_Specific.Value (Get_Task (Event));
               --  Delete its timer.
               POSIX_Timers.Delete_Timer (RRdata_Ac.CPUtime_Timer);
               Associated_Tasks := Associated_Tasks - 1;

               --  Reactivate task
               POSIX_Application_Scheduling.Add_Activate (Actions,
                                                          Get_Task (Event));

            when TERMINATE_TASK =>
               exit when Associated_Tasks = 0;

            when others =>
               null;
         end case;
      end loop;

   exception
      when Excep_Event:others =>
         Put ("Excep. in RR_Scheduler:");
         Put (Ada.Exceptions.Exception_Name (Excep_Event));
         Put (" " & Ada.Exceptions.Exception_Message (Excep_Event));
   end RR_Scheduler;


   -----------------------------
   -- Become_Round_Robin_Task --
   -----------------------------
   procedure Become_Round_Robin_Task
     (Quantum : in Duration := Default_Quantum) is
   begin
      RR_Policy.Change_Task_Policy (RR_Scheduler_Id, (Quantum => Quantum));
   end Become_Round_Robin_Task;

   -------------------------
   -- Terminate_Execution --
   -------------------------
   procedure Terminate_Execution is
   begin
      POSIX_Application_Scheduling.Invoke_Scheduler (0);
   end Terminate_Execution;


end Round_Robin_Scheduling;
