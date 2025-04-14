with POSIX;
with POSIX.Signals;
-- with MaRTE.Kernel.Timers;
with MaRTE.Integer_Types;
with System;
with MaRTE.POSIX_Constants;

package POSIX_Timers is

--    subtype Clock_Id is MaRTE.Kernel.Timers.Clock_Id;
   type Clock_Id is new MaRTE.Integer_Types.Unsigned_32;

--    subtype Timer_Id is MaRTE.Kernel.Timers.Timer_Id;
   type Timer_Id is new System.Address;

   Clock_Realtime : constant Clock_Id := MaRTE.POSIX_Constants.CLOCK_REALTIME;
   Clock_Task_Cputime_Id : constant Clock_Id := MaRTE.POSIX_Constants.CLOCK_THREAD_CPUTIME_ID;
   type Timer_State is private;
   type Timer_Options is new POSIX.Option_Set;

   function Get_Time (Clock : Clock_Id) return POSIX.Timespec;

   procedure Set_Initial (State   : in out Timer_State;
                          Initial : in POSIX.Timespec);

   procedure Set_Interval (State    : in out Timer_State;
                           Interval : in POSIX.Timespec);

   function Create_Timer (Clock : Clock_Id;
                          Event : POSIX.Signals.Signal_Event) return Timer_Id;

   procedure Arm_Timer (Timer     : in Timer_Id;
                        Options   : in Timer_Options;
                        New_State : in Timer_State);

   procedure Disarm_Timer (Timer : in Timer_Id);

private

   type Itimerspec is record
      It_Interval : POSIX.Timespec;
      It_Value    : POSIX.Timespec;
   end record;
   pragma Convention (C, Itimerspec);
   type Itimerspec_Ac is access all Itimerspec;
   type Itimerspec_Ac_Const is access constant Itimerspec;

   type Timer_State is new Itimerspec;

end POSIX_Timers;
