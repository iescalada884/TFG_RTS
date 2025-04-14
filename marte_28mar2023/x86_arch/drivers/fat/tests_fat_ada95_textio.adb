with MaRTE_OS;
with Text_IO;
with Ada.Exceptions;

procedure tests_fat_ada95_textio is

   procedure Pause is
      H : Character;
   begin
      Text_IO.Put(" ...");
      Text_IO.Get_Immediate(H);
   end Pause;

   Fd, Fd2 : Text_IO.File_Type;
   Buff : String(1..200);
   Last : Natural;
begin
   Text_IO.Put ("BEGIN OF FAT Ada95-Text_IO INTERFACE TESTS"); Pause;
   -------------------------------------------------------
   -- TEST 1: Read a text file and display its contents --
   -------------------------------------------------------
   -- Text_IO.Put ("TEST 1: Read a text file and display its contents"); Pause;
   -- Text_IO.Open (Fd, Text_IO.In_File, "/fat/file1");
   -- while not Text_IO.End_Of_File (Fd) loop
   --    Text_IO.Get_Line (Fd, Buff, Last);
   --    Text_IO.Put_Line (Buff (1 .. Last));
   -- end loop;
   -- Text_IO.Close (Fd);
   -------------------------------------------------------
   -- TEST 2: Copy a file /file1 to another file /file2 --
   -------------------------------------------------------
   -- Text_IO.Put ("TEST 2: Copy a file /file1 to another file /file2"); Pause;
   -- Text_IO.Open (Fd, Text_IO.In_File, "/fat/file1");
   -- Text_IO.Create (Fd2, Text_IO.Out_File, "/fat/file2");
   -- while not Text_IO.End_Of_File (Fd) loop
   --    Text_IO.Get_Line (Fd, Buff, Last);
   --    Text_IO.Put_Line (Fd2, Buff (1 .. Last));
   -- end loop;
   -- Text_IO.Close (Fd);
   -- Text_IO.Close (Fd2);
   ------------------------------------------------------
   -- TEST 3: Create a file /file3 and write some text --
   ------------------------------------------------------
   -- Text_IO.Put ("TEST 3: Create a file /file3 and write some text"); Pause;
   -- Text_IO.Create (Fd, Text_IO.Out_File, "/fat/file3");
   -- Text_IO.Put_Line (Fd, "Hello World!!");
   -- Text_IO.Close (Fd);
   -----------------------------------
   -- TEST 4: Delete a file /file3  --
   -----------------------------------
   -- Text_IO.Put ("TEST 4: Delete a file /file3"); Pause;
   -- Text_IO.Open (Fd, Text_IO.In_File, "/fat/file3");
   -- Text_IO.Delete (Fd);
   --------------------------------------------------
   -- TEST 5: Seek 5 chars and read a file /file1  --
   --------------------------------------------------
   -- Text_IO.Put ("TEST 5: Seek related Text_IO functions"); Pause;
   -- Text_IO.Open (Fd, Text_IO.In_File, "/fat/file1");
   --
   -- Text_IO.Put ("a) Read, reset and read again"); Pause;
   -- Text_IO.Get_Line (Fd, Buff (1 .. 6), Last);
   -- Text_IO.Put_Line (Buff (1 .. Last));
   -- Text_IO.Reset (Fd);
   -- Text_IO.Get_Line (Fd, Buff (1 .. 6), Last);
   -- Text_IO.Put_Line (Buff (1 .. Last));
   -- Text_IO.Close (Fd);
   ------------------------------------------------------
   Text_IO.Put ("END OF FAT Ada95-Text_IO INTERFACE TESTS"); Pause;
exception
      when Excep_Event:others =>
         Text_IO.Put ("Exception:");
         Text_IO.Put (Ada.Exceptions.Exception_Name (Excep_Event));
         Text_IO.Put ("  " & Ada.Exceptions.Exception_Message (Excep_Event));
end tests_fat_ada95_textio;
