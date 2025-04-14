--  Test for all architectures

--  Fails because Block_Signals does not call the MaRTE function to
--  block signals

with POSIX_Signals;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Text_IO; use Text_IO;

with Reports;

procedure Test_Uses_Signals is
   pragma Priority (8); --  Just one signal enqueued
   --  pragma Priority (6); --  All the signals enqueued before received

   The_Signal : constant POSIX_Signals.Signal :=
     POSIX_Signals.Realtime_Signal'First;

   --  Conversions between 'Integer' and 'Signal_Data'
   function To_Integer is
     new Ada.Unchecked_Conversion (POSIX_Signals.Signal_Data, Integer);
   function To_Signal_Data is
     new Ada.Unchecked_Conversion (Integer, POSIX_Signals.Signal_Data);

   ---------------------------
   --  Task 'Signal_Sender' --
   ---------------------------
   task Signal_Sender is
      pragma Priority (7);
   end Signal_Sender;

   task body Signal_Sender is
   begin
      Reports.Init;
      for I in 1 .. 6 loop
         Put_Line ("Sending Signal with info " & I'Img);
         POSIX_Signals.Queue_Signal
                (The_Signal,
                 To_Signal_Data (I));
      end loop;
      Put_Line ("Signal_Sender task finishes.");
   exception
      when Event : others =>
         Put ("Excep. in Signal_Sender task:");
         Put (Ada.Exceptions.Exception_Name (Event));
         Put_Line (" " & Ada.Exceptions.Exception_Message (Event));
   end Signal_Sender;

   Info : POSIX_Signals.Signal_Info;
   Waited_Set, Old_Set : POSIX_Signals.Signal_Set;
   Received_Data : Integer;

begin

   Reports.Init;

   --  Prepare set of signals and block
   POSIX_Signals.Delete_All_Signals (Waited_Set);
   POSIX_Signals.Add_Signal (Waited_Set, The_Signal);
   POSIX_Signals.Block_Signals (Waited_Set, Old_Set);

   --  Wait for signal
   for I in 1 .. 6 loop
      Info := POSIX_Signals.Await_Signal (Waited_Set);
      Received_Data := To_Integer (POSIX_Signals.Get_Data (Info));
      Put_Line ("  Received Signal with info "
                & Received_Data'Img);
      Reports.Assert (Received_Data = I);
   end loop;
   Put_Line ("Main task finishes.");

    Reports.Test_OK;

exception
   when Event : others =>
      Put ("Excep. in main task:");
      Put (Ada.Exceptions.Exception_Name (Event));
      Put_Line (" " & Ada.Exceptions.Exception_Message (Event));
end Test_Uses_Signals;
