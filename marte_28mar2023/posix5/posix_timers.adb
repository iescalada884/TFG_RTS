------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                        'P O S I X - T i m e r s'
--
--                                  Body
--
--
--  File 'posix_timers.adb'                                           By MAR.
--
--
--  Package 'POSIX_Timers' as defined in IEEE Std 1003.5b-1996.
--
--  MaRTE OS follows the Minimal Realtime System Profile (PSE51) as
--  defined in IEEE Std 1003.13-98.
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

with MaRTE.Kernel.Signals;
with Option_Sets;
with MaRTE.HAL;
with MaRTE.Integer_Types;

package body POSIX_Timers is

   package MTimers  renames MaRTE.Kernel.Timers;
   package MSignals renames MaRTE.Kernel.Signals;
   package HWI      renames MaRTE.HAL;
   package PI       renames POSIX.Implementation;

   use type MTimers.Timer_Options;

   ---------------------------
   -- Unchecked Conversions --
   ---------------------------
   function To_MClock_Id is
     new Ada.Unchecked_Conversion (Clock_Id,
                                   MTimers.Clock_Id);
   function To_Clock_Id is
     new Ada.Unchecked_Conversion (MTimers.Clock_Id,
                                   Clock_Id);
   function To_MTimer_Id is
     new Ada.Unchecked_Conversion (Timer_Id,
                                   MTimers.Timer_Id);
   function To_Timer_Id is
     new Ada.Unchecked_Conversion (MTimers.Timer_Id,
                                   Timer_Id);
   function To_MOptions is
     new Ada.Unchecked_Conversion (Timer_Options,
                                   Option_Sets.Option_Set);
   function To_MSignals_Signal_Event is
     new Ada.Unchecked_Conversion (POSIX.Signals.Signal_Event,
                                   MSignals.Signal_Event);

   -----------------
   -- Set_Initial --
   -----------------
   procedure Set_Initial (State   : in out Timer_State;
                          Initial : in POSIX.Timespec) is
   begin
      State.Initial := Initial;
   end Set_Initial;

   -----------------
   -- Get_Initial --
   -----------------
   function Get_Initial (State : Timer_State) return POSIX.Timespec is
   begin
      return State.Initial;
   end Get_Initial;

   ------------------
   -- Set_Interval --
   ------------------
   procedure Set_Interval (State    : in out Timer_State;
                           Interval : in POSIX.Timespec) is
   begin
      State.Interval := Interval;
   end Set_Interval;

   -----------------
   -- Get_Interval --
   -----------------
   function Get_Interval (State : Timer_State) return POSIX.Timespec is
   begin
      return State.Interval;
   end Get_Interval;

   -----------------
   --  Set_Time   --
   -----------------
   procedure Set_Time (Clock : in Clock_Id;
                       Value : in POSIX.Timespec) is
   begin
      MTimers.Set_Time (To_MClock_Id (Clock),
                        HWI.Duration_To_HWTime (Value));
      PI.Raise_POSIX_Error_On_Error;
   end Set_Time;

   ----------------
   --  Get_Time  --
   ----------------
   function Get_Time (Clock : Clock_Id) return POSIX.Timespec is
      TS : POSIX.Timespec;
   begin
      TS := HWI.HWTime_To_Duration (MTimers.Get_Time (To_MClock_Id (Clock)));
      PI.Raise_POSIX_Error_On_Error;
      return TS;
   end Get_Time;

   ----------------------
   --  Get_Resolution  --
   ----------------------
   function Get_Resolution (Clock : Clock_Id) return POSIX.Timespec is
      TS : POSIX.Timespec;
   begin
      TS := POSIX.To_Timespec (MTimers.Get_Resolution (To_MClock_Id (Clock)));
      PI.Raise_POSIX_Error_On_Error;
      return TS;
   end Get_Resolution;

   -----------------------
   --  Get_Cpuclock_Id  --
   -----------------------
   function Get_Cpuclock_Id (T : Ada.Task_Identification.Task_Id)
                             return Clock_Id is
      Clock : aliased MTimers.Clock_Id;
   begin
      PI.Check_NZ
        (MTimers.Pthread_Getcpuclockid (PI.To_Kernel_Task_Id (T),
                                        Clock'Access));
      return To_Clock_Id (Clock);
   end Get_Cpuclock_Id;

   --------------------
   --  Create_Timer  --
   --------------------
   function Create_Timer (Clock : Clock_Id;
                          Event : POSIX.Signals.Signal_Event)
                          return Timer_Id is
      Timer : MTimers.Timer_Id;
   begin
      Timer := MTimers.Create_Timer (To_MClock_Id (Clock),
                                     To_MSignals_Signal_Event (Event));

      PI.Raise_POSIX_Error_On_Error;
      return To_Timer_Id (Timer);
   end Create_Timer;

   --------------------
   --  Delete_Timer  --
   --------------------
   procedure Delete_Timer (Timer : in out Timer_Id) is
   begin
      PI.Check_NZ
        (MTimers.Timer_Delete (To_MTimer_Id (Timer)));
   end Delete_Timer;

   ---------------------------------------
   -- Conversions between 'Timer_State' --
   -- y 'MTimers.Timer_State'           --
   ---------------------------------------
   function To_MTimer_State (TS : Timer_State) return MTimers.Timer_State;
   function To_MTimer_State (TS : Timer_State) return MTimers.Timer_State is
   begin
      return MTimers.Timer_State'
        (Initial  => HWI.Duration_To_HWTime (POSIX.To_Duration (TS.Initial)),
         Interval => HWI.Duration_To_HWTime (POSIX.To_Duration (TS.Interval)));
   end To_MTimer_State;
   pragma Inline (To_MTimer_State);

   function To_Timer_State (MTS : MTimers.Timer_State) return Timer_State;
   function To_Timer_State (MTS : MTimers.Timer_State) return Timer_State is
   begin
      return Timer_State'
        (Initial  => POSIX.To_Timespec (HWI.HWTime_To_Duration (MTS.Initial)),
         Interval =>
           POSIX.To_Timespec (HWI.HWTime_To_Duration (MTS.Interval)));
   end To_Timer_State;
   pragma Inline (To_Timer_State);

   -----------------
   --  Arm_Timer  --
   -----------------
   procedure Arm_Timer (Timer     : in  Timer_Id;
                        Options   : in  Timer_Options;
                        New_State : in  Timer_State;
                        Old_State : out Timer_State) is
      KOld_State : MTimers.Timer_State;
   begin
      if Absolute_Timer <= Options then
         if New_State.Initial = 0.0 then
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
            return;
         end if;
      else
         if New_State.Initial <= 0.0 then
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
            return;
         end if;
      end if;
      MTimers.Arm_Timer (To_MTimer_Id (Timer),
                         Options,
                         To_MTimer_State (New_State),
                         KOld_State);

      PI.Raise_POSIX_Error_On_Error;
      Old_State := To_Timer_State (KOld_State);
   end Arm_Timer;

   -----------------
   --  Arm_Timer  --
   -----------------
   procedure Arm_Timer (Timer     : in Timer_Id;
                        Options   : in Timer_Options;
                        New_State : in Timer_State) is
   begin
      if Absolute_Timer <= Options then
         if New_State.Initial = 0.0 then
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
            return;
         end if;
      else
         if New_State.Initial <= 0.0 then
            PI.Raise_POSIX_Error (POSIX.Invalid_Argument);
            return;
         end if;
      end if;
      MTimers.Arm_Timer (To_MTimer_Id (Timer),
                         MTimers.Timer_Options (Options),
                         To_MTimer_State (New_State));

      PI.Raise_POSIX_Error_On_Error;
   end Arm_Timer;

   -----------------------
   --  Get_Timer_State  --
   -----------------------
   function Get_Timer_State (Timer : Timer_Id) return Timer_State is
      TS : Timer_State;
   begin
      TS := To_Timer_State (MTimers.Get_Timer_State (To_MTimer_Id (Timer)));

      PI.Raise_POSIX_Error_On_Error;
      return TS;
   end Get_Timer_State;

   --------------------
   --  Disarm_Timer  --
   --------------------
   procedure Disarm_Timer (Timer : in Timer_Id) is
   begin
      MTimers.Disarm_Timer (To_MTimer_Id (Timer));

      PI.Raise_POSIX_Error_On_Error;
   end Disarm_Timer;

   --------------------------
   --  Get_Timer_Overruns  --
   --------------------------
   function Get_Timer_Overruns (Timer : Timer_Id) return Natural is
      Overruns : MaRTE.Integer_Types.Int;
   begin
      Overruns := MTimers.Timer_Getoverrun (To_MTimer_Id (Timer));
      PI.Check_NNeg (Overruns);
      return Natural (Overruns);
   end Get_Timer_Overruns;


end POSIX_Timers;
