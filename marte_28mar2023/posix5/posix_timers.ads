------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                        'P O S I X - T i m e r s'
--
--                                  Spec
--
--
--  File 'posix_timers.ads'                                           By MAR.
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
with Ada.Task_Identification;
with POSIX;
with POSIX.Signals;

with MaRTE.Kernel.Timers;

package POSIX_Timers is

   type Clock_Id is private;
   type Timer_Id is private;

   Clock_Realtime        : constant Clock_Id;
   Clock_Task_Cputime_Id : constant Clock_Id;

   type Timer_State is private;

   subtype Timer_Options is MaRTE.Kernel.Timers.Timer_Options;
   ABSOLUTE_TIMER : constant Timer_Options;

   procedure Set_Initial (State   : in out Timer_State;
                          Initial : in POSIX.Timespec);

   function Get_Initial (State : Timer_State) return POSIX.Timespec;

   procedure Set_Interval (State    : in out Timer_State;
                           Interval : in POSIX.Timespec);

   function Get_Interval (State : Timer_State) return POSIX.Timespec;

   procedure Set_Time (Clock : in Clock_Id;
                       Value : in POSIX.Timespec);

   function Get_Time (Clock : Clock_Id) return POSIX.Timespec;

   function Get_Resolution (Clock : Clock_Id) return POSIX.Timespec;

   function Get_Cpuclock_Id (T : Ada.Task_Identification.Task_Id)
                             return Clock_Id;

   function Create_Timer (Clock : Clock_Id;
                          Event : POSIX.Signals.Signal_Event) return Timer_Id;

   procedure Delete_Timer (Timer : in out Timer_Id);

   procedure Arm_Timer (Timer     : in  Timer_Id;
                        Options   : in  Timer_Options;
                        New_State : in  Timer_State;
                        Old_State : out Timer_State);

   procedure Arm_Timer (Timer     : in Timer_Id;
                        Options   : in Timer_Options;
                        New_State : in Timer_State);

   function Get_Timer_State (Timer : Timer_Id) return Timer_State;

   procedure Disarm_Timer (Timer : in Timer_Id);

   function Get_Timer_Overruns (Timer : Timer_Id) return Natural;

private
   type Clock_Id is new MaRTE.Kernel.Timers.Clock_Id;

   Clock_Realtime : constant Clock_Id :=
     Clock_Id (MaRTE.Kernel.Timers.CLOCK_REALTIME);
   Clock_Task_Cputime_Id : constant Clock_Id :=
     Clock_Id (MaRTE.Kernel.Timers.CLOCK_THREAD_CPUTIME_ID);

   type Timer_Id is new MaRTE.Kernel.Timers.Timer_Id;

   type Timer_State is record
      Initial  : POSIX.Timespec;
      Interval : POSIX.Timespec;
   end record;

   Absolute_Timer : constant Timer_Options :=
     Timer_Options (MaRTE.Kernel.Timers.ABSOLUTE_TIMER);
end POSIX_Timers;
