------------------------------------------------------------------------------
--                            TESTS - FAT POSIX-Ada                         --
------------------------------------------------------------------------------
--  Test for the FAT through a POSIX-Ada interface. You have to install the --
--  fat_driver_functions in MaRTE OS (see marteos users guide)              --
--  Uncomment/comment the tests as you want                                 --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
with MaRTE_OS;
--  for using the POSIX_IO interface
with POSIX_IO;  use POSIX_IO;
with POSIX_Files;  -- for unlink
with Ada.Streams;
with Ada.Unchecked_Conversion;
--  for checking unexpected exceptions
with Ada.Exceptions;
--  For output messages
with Ada.Text_IO; use Ada.Text_IO;

procedure Tests_Fat_Posix is

   use type Ada.Streams.Stream_Element_Offset;

   ---------------------
   --  String_To_SEA  --
   ---------------------
   procedure String_To_SEA (Str : in String;
                            SEA : out Ada.Streams.Stream_Element_Array)
   is
      type String_T    is new String (1 .. Str'Length);
      type SEA_T       is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (SEA'Length));
      type String_T_Ac is access all String_T;
      type SEA_T_Ac    is access all SEA_T;
      function String_T_Ac_To_SEA_T_Ac is new
        Ada.Unchecked_Conversion (String_T_Ac, SEA_T_Ac);

      Str_Tmp : aliased String_T := String_T (Str);
      SEA_Tmp_Ac : SEA_T_Ac;
   begin
      SEA_Tmp_Ac := String_T_Ac_To_SEA_T_Ac (Str_Tmp'Access);
      SEA := Ada.Streams.Stream_Element_Array (SEA_Tmp_Ac.all);
   end String_To_SEA;

   ---------------------
   --  SEA_To_String  --
   ---------------------
   procedure SEA_To_String (SEA : in Ada.Streams.Stream_Element_Array;
                            Str : out String)
   is
      type String_T    is new String (1 .. Str'Length);
      type SEA_T       is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (SEA'Length));
      type String_T_Ac is access all String_T;
      type SEA_T_Ac    is access all SEA_T;
      function SEA_T_Ac_To_String_T_Ac is new
        Ada.Unchecked_Conversion (SEA_T_Ac, String_T_Ac);
      SEA_Tmp : aliased SEA_T := SEA_T (SEA);
      Str_Tmp_Ac : String_T_Ac;
   begin
      Str_Tmp_Ac := SEA_T_Ac_To_String_T_Ac (SEA_Tmp'Access);
      Str := String (Str_Tmp_Ac.all);
   end SEA_To_String;

   ---------------
   --  Put_SEA  --
   ---------------
   procedure Put_SEA (SEA : in Ada.Streams.Stream_Element_Array) is
      Tmp_Str : String (1 .. Integer (SEA'Length));
   begin
      SEA_To_String (SEA, Tmp_Str);
      Put (Tmp_Str);
   end Put_SEA;

   -------------
   --  Pause  --
   -------------
   procedure Pause is
      H : Character;
   begin
      Put(" ...");
      Get_Immediate(H);
   end Pause;

   Fd, Fd2 : POSIX_IO.File_Descriptor;
   Last_R, Last_W   : Ada.Streams.Stream_Element_Offset;
   Buffer_Read  : Ada.Streams.Stream_Element_Array (1 .. 10);
   Buffer_Write : String := "Hello World!!";
   SEA_Write    : Ada.Streams.Stream_Element_Array (1 .. Buffer_Write'Length);
   Ret : Integer;
   Whence : POSIX_IO.Position;
   Offset, Ret_Offset : POSIX_IO.IO_Offset;
begin
   Put ("BEGIN OF FAT POSIX-Ada95 INTERFACE TESTS"); Pause;
   -------------------------------------------------------
   -- TEST 1: Read a text file and display its contents --
   -------------------------------------------------------
   --    Put ("TEST 1: Read a text file and display its contents"); Pause;
   --    --  Open device file and get file descriptor
   --    Fd := POSIX_IO.Open ("/fat/file1", POSIX_IO.Read_Only);
   --    --  Read from device file
   --    loop
   --       Read (Fd, Buffer_Read, Last_R);
   --       Put_SEA (Buffer_Read (1 .. Last_R));
   --       exit when Last_R = (Buffer_Read'First - 1);
   --    end loop;
   --    --  Close the file
   --    POSIX_IO.Close (Fd);
   -------------------------------------------------------
   -- TEST 2: Copy a file /file1 to another file /file2 --
   -------------------------------------------------------
   --    Put ("TEST 2: Copy a file /file1 to another file /file2"); Pause;
   --    Fd := POSIX_IO.Open ("/fat/file1", POSIX_IO.Read_Only);
   --    Fd2 := POSIX_IO.Open ("/fat/file2", POSIX_IO.Write_Only);
   --    loop
   --       Read (Fd, Buffer_Read, Last_R);
   --       exit when Last_R = (Buffer_Read'First - 1);
   --       Write (Fd2, Buffer_Read (1 .. Last_R), Last_W);
   --    end loop;
   --    POSIX_IO.Close (Fd);
   --    POSIX_IO.Close (Fd2);
   ------------------------------------------------------
   -- TEST 3: Create a file /file3 and write some text --
   ------------------------------------------------------
   --    Put ("TEST 3: Create a file /file3 and write some text"); Pause;
   --    Fd := POSIX_IO.Open_Or_Create ("/fat/file3", POSIX_IO.Write_Only);
   --    String_To_SEA (Buffer_Write, SEA_Write);
   --    POSIX_IO.Write (Fd, SEA_Write, Last_W);
   --    POSIX_IO.Close (Fd);
   -----------------------------------
   -- TEST 4: Delete a file /file3  --
   -----------------------------------
   --    Put ("TEST 4: Delete a file /file3"); Pause;
   --    Fd := POSIX_IO.Open ("/fat/file3", POSIX_IO.Read_Only);
   --    Read (Fd, Buffer_Read, Last_R);
   --    Put_SEA (Buffer_Read (1 .. Last_R));
   --    POSIX_Files.Unlink ("/fat/file3");
   --    --  The File must be deleted when I close the file.
   --    POSIX_IO.Close (Fd);
   --------------------------------------------------
   -- TEST 5: Seek 5 chars and read a file /file1  --
   --------------------------------------------------
   --    Put ("TEST 5: Seek 5 chars and read a file /file1"); Pause;
   --    Fd := POSIX_IO.Open ("/fat/file1", POSIX_IO.Read_Only);
   --
   --    Put ("a) Seek -5 from the end"); Pause;
   --    Whence := From_End_Of_File;
   --    Offset := -5;
   --    POSIX_IO.Seek (Fd, Offset, Ret_Offset, Whence);
   --    Read (Fd, Buffer_Read, Last_R);
   --    Put_SEA (Buffer_Read (1 .. Last_R));
   --    --  redo the seek because the 'read' seeks too!
   --    POSIX_IO.Seek (Fd, Offset, Ret_Offset, Whence);
   --
   --    Put ("b) Seek +2 from current offset"); Pause;
   --    Whence := From_Current_Position;
   --    Offset := 2;
   --    POSIX_IO.Seek (Fd, Offset, Ret_Offset, Whence);
   --    Read (Fd, Buffer_Read, Last_R);
   --    Put_SEA (Buffer_Read (1 .. Last_R));
   --
   --    Put ("c) Seek to absolute offset 4"); Pause;
   --    Whence := From_Beginning;
   --    Offset := 4;
   --    POSIX_IO.Seek (Fd, Offset, Ret_Offset, Whence);
   --    Read (Fd, Buffer_Read, Last_R);
   --    Put_SEA (Buffer_Read (1 .. Last_R));
   --
   --    POSIX_IO.Close (Fd);
   ------------------------------------------------------
   Put ("END OF FAT POSIX-Ada95 INTERFACE TESTS"); Pause;
exception
      when Excep_Event:others =>
         Put ("Exception:");
         Put (Ada.Exceptions.Exception_Name (Excep_Event));
         Put ("  " & Ada.Exceptions.Exception_Message (Excep_Event));
end Tests_Fat_Posix;