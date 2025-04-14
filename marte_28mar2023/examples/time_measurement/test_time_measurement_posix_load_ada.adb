with Execution_Load;
with Time_Measurement_POSIX_Ada;
with POSIX_Timers;
with Text_IO;
with Ada.Exceptions;

procedure Test_Time_Measurement_POSIX_Load_Ada is
   package TM renames Time_Measurement_POSIX_Ada;

   Measure_ID : TM.Time_Measure_Id;
   Name       : TM.Time_Measure_Name := "measure";
   Msg        : TM.Time_Measure_Msg := "msg";

   Fd : Text_IO.File_Type;
   Buff : String(1..200);
   Last : Natural;
begin
   TM.Time_Measure_POSIX_Create
                (Name, POSIX_Timers.Clock_Realtime, Measure_ID);

   for I in 1 .. TM.MX_TIME_MEASURES loop
      TM.Time_Measure_POSIX_Begin (Measure_ID);
      Execution_Load.Eat (0.003);
      TM.Time_Measure_POSIX_End (Measure_ID, Msg);
   end loop;

   Text_IO.Open (Fd, Text_IO.In_File, "/dev/mem");
   while not Text_IO.End_Of_File (Fd) loop
      Text_IO.Get_Line (Fd, Buff, Last);
      Text_IO.Put_Line (Buff (1 .. Last));
   end loop;
   Text_IO.Close (Fd);

exception
   when The_Error : others =>
      Text_IO.Put_Line(Ada.Exceptions.Exception_Name(The_Error));
      Text_IO.Put_Line(Ada.Exceptions.Exception_Message(The_Error));
      raise;
end Test_Time_Measurement_POSIX_Load_Ada;
