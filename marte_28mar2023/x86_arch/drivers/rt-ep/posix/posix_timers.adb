with MaRTE.Integer_Types; use MaRTE.Integer_Types; -- for Int
package body POSIX_Timers is

   procedure CHECK_ERROR (Value : Int);
   pragma Inline (CHECK_ERROR);

   procedure CHECK_ERROR (Value : Int) is
   begin
      null; --  Just ignore it for the moment
   end CHECK_ERROR;

   ----------------
   --  Get_Time  --
   ----------------
   function Clock_Gettime (Clock    : Clock_Id;
                           Tp       : access POSIX.Timespec) return Int;
   pragma Import (C, Clock_Gettime, "clock_gettime");

   function Get_Time (Clock : Clock_Id) return POSIX.Timespec is
      TS : aliased POSIX.Timespec;
   begin
      CHECK_ERROR (Clock_Gettime (Clock, TS'Access));
      return TS;
   end Get_Time;

   -----------------
   -- Set_Initial --
   -----------------
   procedure Set_Initial (State   : in out Timer_State;
                          Initial : in POSIX.Timespec) is
   begin
      State.It_Value := Initial;
   end Set_Initial;

   ------------------
   -- Set_Interval --
   ------------------
   procedure Set_Interval (State    : in out Timer_State;
                           Interval : in POSIX.Timespec) is
   begin
      State.It_Interval := Interval;
   end Set_Interval;

   --------------------
   --  Create_Timer  --
   --------------------

   function Timer_Create (Clock      : Clock_Id;
                          Evp        : POSIX.Signals.Signal_Event_Ac_Const;
                          Timerid    : access Timer_Id) return Int;
   pragma Import (C, Timer_Create, "timer_create");


   function Create_Timer (Clock : Clock_Id;
                          Event : POSIX.Signals.Signal_Event)
                          return Timer_Id is
      Timer : aliased Timer_Id;
      Tmp_Event : aliased POSIX.Signals.Signal_Event := Event;
   begin
      CHECK_ERROR (Timer_Create (Clock, Tmp_Event'Unchecked_Access, Timer'Access));
      return Timer;
   end Create_Timer;

   -----------------
   --  Arm_Timer  --
   -----------------

   function Timer_Settime (Timerid : Timer_Id;
                           Flags   : Unsigned_32;
                           Value   : Itimerspec_Ac_Const;
                           Ovalue  : Itimerspec_Ac) return Int;
   pragma Import (C, Timer_Settime, "timer_settime");


   procedure Arm_Timer (Timer     : in Timer_Id;
                        Options   : in Timer_Options;
                        New_State : in Timer_State) is
      Value : aliased Itimerspec;
   begin
      Value := Itimerspec (New_State);
      CHECK_ERROR (Timer_Settime (Timer, 0, Value'Unchecked_Access, Null));
   end Arm_Timer;

   --------------------
   --  Disarm_Timer  --
   --------------------

   procedure Disarm_Timer (Timer : in Timer_Id) is
      Value : aliased Itimerspec;
   begin
      Value.It_Value := POSIX.To_Timespec (0, 0);
      CHECK_ERROR (Timer_Settime (Timer, 0, Value'Unchecked_Access, Null));
   end Disarm_Timer;

end POSIX_Timers;
