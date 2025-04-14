with MaRTE_OS;
with Text_IO;
with Ada.Exceptions;

procedure tests_fat_automatic is
   Fd : Text_IO.File_Type;
   Buff : String (1..60);
   Last : Natural;
   Text : String := "Hello World!!";
begin
   Text_IO.Put_Line ("BEGIN OF FAT AUTOMATIC TESTS");
   -----------------------------------------------
   -- TEST 1: Create a file and write some text --
   -----------------------------------------------
   Text_IO.Put ("TEST 1: Create a file and write some text...   ");
   Text_IO.Create (Fd, Text_IO.Out_File, "/fat/file1");
   Text_IO.Put_Line (Fd, Text);
   Text_IO.Close (Fd);
   Text_IO.Put_Line ("Ok");
   ------------------------------
   -- TEST 2: Read a text file --
   ------------------------------
   Text_IO.Put ("TEST 2: Read a text file...  ");
   Text_IO.Open (Fd, Text_IO.In_File, "/fat/file1");
   while not Text_IO.End_Of_File (Fd) loop
      Text_IO.Get_Line (Fd, Buff, Last);
   end loop;
   Text_IO.Close (Fd);
   if Buff (1 .. Last) = Text then
      Text_IO.Put_Line ("Ok");
   else
      Text_IO.Put_Line ("Fail");
   end if;
   --------------------------
   -- TEST 3: Seek a file  --
   --------------------------
   Text_IO.Put ("TEST 3: Seek a file...  ");
   Text_IO.Open (Fd, Text_IO.In_File, "/fat/file1");
   Text_IO.Get_Line (Fd, Buff (1 .. 6), Last);
   Text_IO.Reset (Fd);
   Text_IO.Get_Line (Fd, Buff (1 .. 6), Last);
   Text_IO.Close (Fd);
   if Buff (1 .. 6) = Text (1 .. 6) then
      Text_IO.Put_Line ("Ok");
   else
      Text_IO.Put_Line ("Fail");
   end if;
   ----------------------------
   -- TEST 4: Delete a file  --
   ----------------------------
   Text_IO.Put ("TEST 4: Delete a file...  ");
   Text_IO.Open (Fd, Text_IO.In_File, "/fat/file1");
   Text_IO.Delete (Fd);
   Text_IO.Put_Line ("Ok");
   ------------------------------------------------------
   Text_IO.Put_Line ("END OF FAT AUTOMATIC TESTS");
exception
      when Excep_Event:others =>
         Text_IO.Put_Line ("Fail");
         Text_IO.Put ("Exception:");
         Text_IO.Put (Ada.Exceptions.Exception_Name (Excep_Event));
         Text_IO.Put ("  " & Ada.Exceptions.Exception_Message (Excep_Event));
end tests_fat_automatic;