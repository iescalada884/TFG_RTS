------------------------------------------------------------------------------
--                            TESTS - FAT module                            --
------------------------------------------------------------------------------
--  Test for the FAT module. (Comment/uncomment each test as desired)       --
--  The best way to test the module is to use a PC emulator and check the   --
--  results with Linux tools. There is a HOW-TO available in the MaRTE OS   --
--  Website.                                                                --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  TODO: CHECK RET VALUES !!!!                                             --
------------------------------------------------------------------------------
with MaRTE_OS;
--  for Debugging
--  with Debug_Marte; use Debug_Marte;
--  for unchecked conversions
with Ada.Unchecked_Conversion;
with System;
--  for basic types
with MaRTE.Integer_Types;
--  for output messages
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
--  IDE and FAT interface
with IDE; use IDE;
with IDE.Partitions; use IDE.Partitions;
with FAT;

procedure tests_fat is

   package BIT renames MaRTE.Integer_Types;
   use type BIT.Int;

   procedure Pause is
      H : Character;
   begin
      Get_Immediate(H);
      New_Line;
   end Pause;

   use type FAT.FSDT.Buffer_Length;
   function Address_To_Buffer_Ac is new
        Ada.Unchecked_Conversion (System.Address, FAT.FSDT.Buffer_Ac);

   File1 : FAT.FSDT.Path := "/file1          ";
   File2 : FAT.FSDT.Path := "/file2          ";
   File3 : FAT.FSDT.Path := "/file3          ";
   Part : aliased IDE.Partitions.Partition;
   Ent1, Ent2, Ent3 : aliased FAT.FAT_Entry;
   Ret : BIT.Int;
   Buffer_Ptr : FAT.FSDT.Buffer_Ac;
   Buff : String (1 .. 50);
   Pos : FAT.FSDT.Buffer_Length := 0;
   Offset, Off_Ret : FAT.FSDT.Off_t;
   Whence : BIT.Int;
   Drv : IDE.Drive := IDE.hdc;
begin
   --  Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
   --  Debug_Marte.Set_Break_Point_Here;
   Put_Line ("Tests for the FAT module");
   Pause;
   --  Init the disk
   Ret := IDE.Init (Drv);
   --  Get info of 1st partition
   Ret := IDE.Partitions.Get_Partition_Info (Drv, 1, Part'Access);
   --  Init the FAT superblock
   Ret := FAT.Fat_Create (Part);
   -------------------------------------------------------
   -- TEST 1: Read a text file and display its contents --
   -------------------------------------------------------
      New_Line;
      Put_Line ("TEST 1: Read a file named /file1 (it must exist)");
      Pause;
      --  Open files
      Ret := FAT.FAT_Open (File1, Ent1'Access, False);
      --  READ file1
      Buffer_Ptr := Address_To_Buffer_Ac (Buff'Address);
      loop
         Ret := FAT.FAT_Read (Ent1'Access, Buffer_Ptr, Buff'Length);
         exit when (Ret <= 0);
         Put (Buff (1 .. Integer(Ret)));
      end loop;
   -------------------------------------------------------
   -- TEST 2: Copy a file /file1 to another file /file2 --
   -------------------------------------------------------
   --    New_Line;
   --    Put_Line ("TEST 2: Copy a file /file1 to another file /file2");
   --    Pause;
   --    Ret := FAT.FAT_Open (File1, Ent1'Access, False);
   --    Ret := FAT.FAT_Open (File2, Ent2'Access, True);
   --    --  READ file1 and WRITE into file2
   --    Buffer_Ptr := Address_To_Buffer_Ac (Buff'Address);
   --    loop
   --       Ret := FAT.FAT_Read (Ent1'Access, Buffer_Ptr, Buff'Length);
   --       exit when (Ret <= 0);
   --       Ret := FAT.FAT_Write
   --         (Ent2'Access, Buffer_Ptr, FAT.FSDT.Buffer_Length (Ret));
   --    end loop;
   ------------------------------------------------------
   -- TEST 3: Create a file /file3 and write some text --
   ------------------------------------------------------
   --    New_Line;
   --    Put_Line ("TEST 3: Create a file /file3 and write some text");
   --    Pause;
   --    Ret := FAT.FAT_Open (File3, Ent3'Access, True);
   --    Buff (1 .. 11) := "Hello MaRTE";
   --    Buffer_Ptr := Address_To_Buffer_Ac (Buff'Address);
   --    Ret := FAT.FAT_Write
   --         (Ent3'Access, Buffer_Ptr, FAT.FSDT.Buffer_Length (11));
   -----------------------------------
   -- TEST 4: Delete a file /file3  --
   -----------------------------------
   --    New_Line;
   --    Put_Line ("TEST 4: Delete a file /file3");
   --    Pause;
   --    Ret := FAT.FAT_Open (File3, Ent3'Access, False);
   --    Ret := FAT.FAT_Delete (Ent3);
   --------------------------------------------------
   -- TEST 5: Seek and read a file /file1  --
   --------------------------------------------------
   --    New_Line;
   --    Put_Line ("TEST 5: Seek and read a file /file1");
   --    Pause;
   --    Ret := FAT.FAT_Open (File1, Ent1'Access, False);
   --
   --    Put_Line ("a) Seek -5 from the end"); Pause;
   --    Offset := -5;
   --    Whence := FAT.SEEK_END;
   --    Off_Ret := FAT.FAT_Seek (Ent1'Access, Offset, Whence);
   --    Buffer_Ptr := Address_To_Buffer_Ac (Buff'Address);
   --    Ret := FAT.FAT_Read (Ent1'Access, Buffer_Ptr, Buff'Length);
   --    Put (Buff (1 .. Ret));
   --    --  redo the seek because the READ call also seeks!
   --    Off_Ret := FAT.FAT_Seek (Ent1'Access, Offset, Whence);
   --
   --    Put_Line ("b) Seek +2 from current offset"); Pause;
   --    Offset := 2;
   --    Whence := FAT.SEEK_CUR;
   --    Off_Ret := FAT.FAT_Seek (Ent1'Access, Offset, Whence);
   --    Buffer_Ptr := Address_To_Buffer_Ac (Buff'Address);
   --    Ret := FAT.FAT_Read (Ent1'Access, Buffer_Ptr, Buff'Length);
   --    Put (Buff (1 .. Ret));
   --
   --    Put_Line ("c) Seek to absolute offset 4"); Pause;
   --    Offset := 4;
   --    Whence := FAT.SEEK_SET;
   --    Off_Ret := FAT.FAT_Seek (Ent1'Access, Offset, Whence);
   --    Buffer_Ptr := Address_To_Buffer_Ac (Buff'Address);
   --    Ret := FAT.FAT_Read (Ent1'Access, Buffer_Ptr, Buff'Length);
   --    Put (Buff (1 .. Ret));
   ------------------------------------------------------------------------
   -- TEST 6: Seek a /file1 beyond EOF and write text, then read the gap --
   ------------------------------------------------------------------------
   --    New_Line;
   --    Put_Line ("TEST 6: Seek a /file1 beyond EOF and write text");
   --    Pause;
   --    Ret := FAT.FAT_Open (File1, Ent1'Access, False);
   --    Offset := 10000;
   --    Whence := FAT.SEEK_END;
   --    Off_Ret := FAT.FAT_Seek (Ent1'Access, Offset, Whence);
   --    Buff (1 .. 11) := "Hello MaRTE";
   --    Buffer_Ptr := Address_To_Buffer_Ac (Buff'Address);
   --    Ret := FAT.FAT_Write
   --          (Ent1'Access, Buffer_Ptr, FAT.FSDT.Buffer_Length (11));
   --    Offset := 5000;
   --    Whence := FAT.SEEK_SET;
   --    Off_Ret := FAT.FAT_Seek (Ent1'Access, Offset, Whence);
   --    Ret := FAT.FAT_Read (Ent1'Access, Buffer_Ptr, Buff'Length);
   --    Put (Buff (1 .. Ret));

   --  Free the disk
   Ret := IDE.Free (Drv);
end tests_fat;
