--  Test for all architectures

with Text_IO; use Text_IO;
with POSIX;
with POSIX.Signals;
with POSIX_Timers;
with Ada.Exceptions;

with Reports;

procedure Test_Timer_Signal is
   The_Signal : constant POSIX.Signals.Signal := POSIX.Signals.SIGUSR1;
   Info       : POSIX.Signals.Signal_Info;
   Waited_Set : POSIX.Signals.Signal_Set;
   Old_Set    : POSIX.Signals.Signal_Set;
   Event      : POSIX.Signals.Signal_Event;
   Timerid    : POSIX_Timers.Timer_Id;
   Timerdata  : POSIX_Timers.Timer_State;
   Options    : POSIX_Timers.Timer_Options;
   Start_Time : Duration :=
      POSIX.To_Duration (POSIX_Timers.Get_Time (POSIX_Timers.Clock_Realtime));

   Signal_Count_Total : constant Integer := 4;
   Signal_Counter     : Integer := 0;
begin

   Reports.Init;

   --  Block signal
   POSIX.Signals.Delete_All_Signals (Waited_Set);
   POSIX.Signals.Add_Signal (Waited_Set, The_Signal);
   POSIX.Signals.Block_Signals (Waited_Set, Old_Set);

   Put_Line ("Prepare Event");
   POSIX.Signals.Set_Signal (Event, The_Signal);
   POSIX.Signals.Set_Notification (Event, POSIX.Signals.Signal_Notification);

   Put_Line ("Create timer");
   Timerid := POSIX_Timers.Create_Timer (POSIX_Timers.Clock_Realtime, Event);
   POSIX_Timers.Set_Initial (Timerdata, POSIX.To_Timespec (1, 0));
   POSIX_Timers.Set_Interval (Timerdata, POSIX.To_Timespec (1, 0));
--    Options := POSIX_Timers.Timer_Options (POSIX.Empty_Set);
   POSIX_Timers.Arm_Timer (Timerid, Options, Timerdata);

   Put_Line ("Wait for signal");
   for I in 1 .. Signal_Count_Total loop
      Info := POSIX.Signals.Await_Signal (Waited_Set);
      Put ("Received Signal at: ");
      Put (Duration'Image (POSIX.To_Duration (
                              POSIX_Timers.Get_Time (
                                 POSIX_Timers.Clock_Realtime)) - Start_Time));
      New_Line;
      Signal_Counter := I;
   end loop;
   Put_Line ("Main task finishes.");
   Reports.Assert (Signal_Counter = Signal_Count_Total);
   Reports.Test_OK;

exception
   when The_Error : others =>
      Text_IO.Put_Line("Unknown error:");
      Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_Timer_Signal;
