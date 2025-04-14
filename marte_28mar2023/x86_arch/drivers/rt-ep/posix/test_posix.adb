with Text_IO; use Text_IO;
with POSIX;
with POSIX.Signals;
with POSIX_Timers;
with Ada.Exceptions;

procedure Test_Posix is
   pragma Priority (8);

   The_Signal : constant POSIX.Signals.Signal := 34;
   Info : POSIX.Signals.Signal_Info;
   Waited_Set : POSIX.Signals.Signal_Set;
   Event : POSIX.Signals.Signal_Event;
   Timerid : POSIX_Timers.Timer_Id;
   Timerdata : POSIX_Timers.Timer_State;
   Options : POSIX_Timers.Timer_Options;
   Start_Time : Duration :=
      POSIX.To_Duration (POSIX_Timers.Get_Time (POSIX_Timers.Clock_Realtime));
begin
   Put_Line ("Prepare Event");
   POSIX.Signals.Set_Signal (Event, The_Signal);
   POSIX.Signals.Set_Notification (Event, POSIX.Signals.Signal_Notification);

   Put_Line ("Create timer");
   Timerid := POSIX_Timers.Create_Timer (POSIX_Timers.Clock_Realtime, Event);
   POSIX_Timers.Set_Initial (Timerdata, POSIX.To_Timespec (5, 0));
   POSIX_Timers.Set_Interval (Timerdata, POSIX.To_Timespec (5, 0));
   POSIX_Timers.Arm_Timer (Timerid, Options, Timerdata);

   Put_Line ("Wait for signal");
   POSIX.Signals.Delete_All_Signals (Waited_Set);
   POSIX.Signals.Add_Signal (Waited_Set, The_Signal);
   for I in 1 .. 6 loop
      Info := POSIX.Signals.Await_Signal (Waited_Set);
      Put ("Received Signal at: ");
      Put (Duration'Image (POSIX.To_Duration (
                              POSIX_Timers.Get_Time (
                                 POSIX_Timers.Clock_Realtime)) - Start_Time));
      New_Line;
   end loop;
   Put_Line ("Main task finishes.");
exception
   when The_Error : others =>
      Text_IO.Put_Line("Unknown error:");
      Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_Posix;
