--  Show information about selected files and directories in the disk
--
--  Set the directories and files to inspect in the arrays Dir_Names and
--  File_Names, and compile:
--    mgnatmake -g inspect_disk.adb -Imarte_src_dirs
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with FAT.Check.Root_Dir;
with FAT.Root_Directory;
with FAT.Directory_Entries;

procedure Inspect_Disk is
   Dir_Entry : FAT.Directory_Entries.Directory_Entry;
   Entry_Index : FAT.Root_Directory.Root_Dir_Entry_Index;
   Found : Boolean;

   Dir_Names : array (1 .. 1) of FAT.Directory_Entries.Short_File_Name :=
     (1 => "BOOT       ");

   File_Names : array (1 .. 3) of FAT.Directory_Entries.Short_File_Name :=
     ("LARGO      ",
      "LARGO1     ",
      "140722     ");

begin
   Put_Line ("Disk Info:");

   --  Show contents of the root directory

   FAT.Check.Root_Dir.Show_Root_Dir_Info;

   --  Show info about selected directories

   for I in Dir_Names'Range loop
      FAT.Check.Show_File_Clusters (Dir_Names (I));
      FAT.Check.Root_Dir.Show_Root_SubDir_Entries (Dir_Names (I));
   end loop;

   --  Show info about selected files

   for I in File_Names'Range loop
      FAT.Check.Show_File_Clusters (File_Names (I));

      FAT.Root_Directory.Find_Root_Dir_Entry (File_Names (I),
                                              Dir_Entry,
                                              Entry_Index,
                                              Found);
      if Found then
         FAT.Check.Root_Dir.Show_Dir_Entry (Dir_Entry,
                                            With_Dates => True);
         New_Line;
      end if;
   end loop;

   --  Show FAT table info

   FAT.Check.Show_FAT_Info;

exception
   when ex:others =>
      Ada.Text_IO.Put(Ada.Exceptions.Exception_Name(ex) &" ");
      Ada.Text_io.put_line("");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(ex));
end Inspect_Disk;
