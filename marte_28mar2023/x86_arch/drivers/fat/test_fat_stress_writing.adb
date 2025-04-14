with Text_IO;
with Ada.Exceptions;

procedure Test_FAT_Stress_Writing is

   procedure Pause is
      H : Character;
   begin
      Text_IO.Put(" ...");
      Text_IO.Get_Immediate(H);
   end Pause;

   TOTAL_LINES : constant := 2000;

   Fd : Text_IO.File_Type;
   Buff : String(1..200);
   Last : Natural;
begin

   Text_IO.Create (Fd, Text_IO.Out_File, "/fat/stress");

   for I in 1 .. TOTAL_LINES loop
      --  Text_IO.Put_Line ("line "&Integer'Image (I));
      Text_IO.Put_Line (Fd, "line "&Integer'Image (I));
   end loop;

   Text_IO.Close (Fd);

   Text_IO.Put ("### Check the File ###");
   Pause;

   Text_IO.Open (Fd, Text_IO.In_File, "/fat/stress");
   while not Text_IO.End_Of_File (Fd) loop
      Text_IO.Get_Line (Fd, Buff, Last);
      Text_IO.Put_Line (Buff (1 .. Last));
   end loop;
   Text_IO.Close (Fd);

end Test_FAT_Stress_Writing;
