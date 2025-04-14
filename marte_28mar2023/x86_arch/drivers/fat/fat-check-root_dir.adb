with MaRTE.Integer_Types;
with MaRTE.Kernel.File_System_Data_Types;
with MaRTE.Direct_IO;
with FAT.Superblock;
with FAT.Clusters;
with FAT.Root_Directory;
with IDE;

package body FAT.Check.Root_dir is
   use type IDE.Sector_Offset, Directory_Entries.File_Attributes;

   package BIT  renames MaRTE.Integer_Types;

   ------------------------------
   -- Check_Root_SubDir_Entries --
   ------------------------------

   function Check_Root_SubDir_Entries
     (Dir_Name : Directory_Entries.Short_File_Name) return Boolean is

      Subdir_Entry, An_Entry : Directory_Entries.Directory_Entry;
      Entry_Index : Root_Directory.Root_Dir_Entry_Index;
      Found : Boolean;
   begin
      --  Look for the sub directory

      Root_Directory.Find_Root_Dir_Entry (Dir_Name,
                                          Subdir_Entry,
                                          Entry_Index,
                                          Found);

      if not Found then
         MaRTE.Direct_IO.Put (Dir_Name);
         MaRTE.Direct_IO.Put (" dir not found");
         MaRTE.Direct_IO.New_Line;
         return false;
      end if;

      if not Directory_Entries.Entry_Is_A_Directory (Subdir_Entry) then
         MaRTE.Direct_IO.Put (Dir_Name);
         MaRTE.Direct_IO.Put (" it is not a directory");
         MaRTE.Direct_IO.New_Line;
         return false;
      end if;

      --  Traverse every entry in the entry cluster

      for Entry_Cluster_Index In 0 .. Clusters.Last_Entry_Index_In_Cluster loop
         An_Entry := Clusters.Get_Subdirectory_Entry
           (Subdir_Entry.First_Cluster_Number, Entry_Cluster_Index);

         if not Directory_Entries.Is_A_Long_Name_Entry (An_Entry) and then
           not Directory_Entries.Directory_Entry_Looks_OK (An_Entry) then

            MaRTE.Direct_IO.Put (Dir_Name);
            MaRTE.Direct_IO.Put (" entry ");
            MaRTE.Direct_IO.Put (Natural (Entry_Cluster_Index));
            MaRTE.Direct_IO.Put (" is wrong");
            MaRTE.Direct_IO.New_Line;

            return false;
         end if;
      end loop;

      return true;
   end Check_Root_SubDir_Entries;

   ------------------------------
   -- Show_Root_SubDir_Entries --
   ------------------------------

   procedure Show_Root_SubDir_Entries
     (Dir_Name : Directory_Entries.Short_File_Name) is

      Subdir_Entry, An_Entry : Directory_Entries.Directory_Entry;
      Entry_Index : Root_Directory.Root_Dir_Entry_Index;
      Found : Boolean;

      Num_Of_Files : Natural := 0;
      Num_Of_Deleted_Files : Natural := 0;
      Num_Of_Unallocated_Files : Natural := 0;
      Num_Of_Directories : Natural := 0;
      Num_Of_Win2000_Entries : Natural := 0;
   begin
      MaRTE.Direct_IO.Put (Dir_Name);
      MaRTE.Direct_IO.Put (":");

      --  Look for the sub directory

      Root_Directory.Find_Root_Dir_Entry (Dir_Name,
                                          Subdir_Entry,
                                          Entry_Index,
                                          Found);

      if not Found then
         MaRTE.Direct_IO.Put (" dir not found");
         MaRTE.Direct_IO.New_Line;
         return;
      end if;

      if not Directory_Entries.Entry_Is_A_Directory (Subdir_Entry) then
         MaRTE.Direct_IO.Put (" it is not a directory");
         MaRTE.Direct_IO.New_Line;
         return;
      end if;

      --  Traverse every entry in the entry cluster

      for Entry_Cluster_Index In 0 .. Clusters.Last_Entry_Index_In_Cluster loop
         An_Entry := Clusters.Get_Subdirectory_Entry
           (Subdir_Entry.First_Cluster_Number, Entry_Cluster_Index);

         if Directory_Entries.Is_A_Long_Name_Entry (An_Entry) then
            --  Long name entry

            Num_Of_Win2000_Entries := Num_Of_Win2000_Entries + 1;

         else
            --  short name entry

            case An_Entry.File_Name_8_3 (1) is
               when Directory_Entries.Char_For_Deleted_File =>
                  Num_Of_Deleted_Files := Num_Of_Deleted_Files + 1;

               when Directory_Entries.Char_For_Unallocated_File =>
                  Num_Of_Unallocated_Files := Num_Of_Unallocated_Files + 1;

               when Directory_Entries.Char_For_Directory =>
                  Num_Of_Directories := Num_Of_Directories + 1;

                  MaRTE.Direct_IO.Put (" I:");
                  MaRTE.Direct_IO.Put (Natural (Entry_Cluster_Index));

                  Show_Dir_Entry (An_Entry);

               when others =>
                  Num_Of_Files := Num_Of_Files + 1;

                  MaRTE.Direct_IO.Put (" I:");
                  MaRTE.Direct_IO.Put (Natural (Entry_Cluster_Index));

                  Show_Dir_Entry (An_Entry);
            end case;
         end if;
      end loop;

      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("DelFiles:");
      MaRTE.Direct_IO.Put (Num_Of_Deleted_Files);
      MaRTE.Direct_IO.Put (" UnallocFiles:");
      MaRTE.Direct_IO.Put (Num_Of_Unallocated_Files);
      MaRTE.Direct_IO.Put (" NumFiles:");
      MaRTE.Direct_IO.Put (Num_Of_Files);
      MaRTE.Direct_IO.Put (" NumDirs:");
      MaRTE.Direct_IO.Put (Num_Of_Directories);
      MaRTE.Direct_IO.Put (" Win2000:");
      MaRTE.Direct_IO.Put (Num_Of_Win2000_Entries);
      MaRTE.Direct_IO.New_Line;
   end Show_Root_SubDir_Entries;

   ------------------------
   -- Show_Root_Dir_Info --
   ------------------------

   procedure Show_Root_Dir_Info is
      use type BIT.Unsigned_32;
      An_Entry : Directory_Entries.Directory_Entry;
      Num_Of_Files : Natural := 0;
      Num_Of_Deleted_Files : Natural := 0;
      Num_Of_Unallocated_Files : Natural := 0;
      Num_Of_Directories : Natural := 0;
      Num_Of_Win2000_Entries : Natural := 0;
   begin

      --  Look every Root Dir entry looking for used entries

      for Entry_Index in 0 .. Root_Directory.Last_Root_Dir_Entry_Index loop
         An_Entry := Root_Directory.Get_Root_Dir_Entry (Entry_Index);

         if Directory_Entries.Is_A_Long_Name_Entry (An_Entry) then
            --  Long name entry

            Num_Of_Win2000_Entries := Num_Of_Win2000_Entries + 1;

         else
            --  short name entry

            case An_Entry.File_Name_8_3 (1) is
               when Directory_Entries.Char_For_Deleted_File =>
                  Num_Of_Deleted_Files := Num_Of_Deleted_Files + 1;

               when Directory_Entries.Char_For_Unallocated_File =>
                  Num_Of_Unallocated_Files := Num_Of_Unallocated_Files + 1;

               when Directory_Entries.Char_For_Directory =>
                  Num_Of_Directories := Num_Of_Directories + 1;

                  Show_Dir_Entry (An_Entry);

               when others =>
                  Num_Of_Files := Num_Of_Files + 1;

                  Show_Dir_Entry (An_Entry);
            end case;
         end if;
      end loop;

      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("DelFiles:");
      MaRTE.Direct_IO.Put (Num_Of_Deleted_Files);
      MaRTE.Direct_IO.Put (" UnallocFiles:");
      MaRTE.Direct_IO.Put (Num_Of_Unallocated_Files);
      MaRTE.Direct_IO.Put (" NumFiles:");
      MaRTE.Direct_IO.Put (Num_Of_Files);
      MaRTE.Direct_IO.Put (" NumDirs:");
      MaRTE.Direct_IO.Put (Num_Of_Directories);
      MaRTE.Direct_IO.Put (" Win2000:");
      MaRTE.Direct_IO.Put (Num_Of_Win2000_Entries);
      MaRTE.Direct_IO.New_Line;
   end Show_Root_Dir_Info;

   --------------------
   -- Show_Dir_Entry --
   --------------------

   procedure Show_Dir_Entry (The_Entry : Directory_Entries.Directory_Entry;
                             With_Dates : Boolean := False) is

      procedure Show_Date (Date : Directory_Entries.Date) is
      begin
         MaRTE.Direct_IO.Put (Integer (Date.Day));
         MaRTE.Direct_IO.Put ("-");
         MaRTE.Direct_IO.Put (Integer (Date.Month));
         MaRTE.Direct_IO.Put ("-");
         MaRTE.Direct_IO.Put (Integer (Date.Year) + 1980);
      end Show_Date;

      procedure Show_Date_And_Time (Date : Directory_Entries.Time_And_Date) is
      begin
         MaRTE.Direct_IO.Put (Integer (Date.Day));
         MaRTE.Direct_IO.Put ("-");
         MaRTE.Direct_IO.Put (Integer (Date.Month));
         MaRTE.Direct_IO.Put ("-");
         MaRTE.Direct_IO.Put (Integer (Date.Year) + 1980);
         MaRTE.Direct_IO.Put (":");
         MaRTE.Direct_IO.Put (Integer (Date.Hour));
         MaRTE.Direct_IO.Put (":");
         MaRTE.Direct_IO.Put (Integer (Date.Minutes));
         MaRTE.Direct_IO.Put (":");
         MaRTE.Direct_IO.Put (Integer (Date.Seconds) * 2);
      end Show_Date_And_Time;

   begin
      MaRTE.Direct_IO.Put ("/");
      MaRTE.Direct_IO.Put (The_Entry.File_Name_8_3);
      MaRTE.Direct_IO.Put (":");
      MaRTE.Direct_IO.Put (BIT.Unsigned_32 (The_Entry.First_Cluster_Number));
--      MaRTE.Direct_IO.Put (":");
--      MaRTE.Direct_IO.Put (BIT.Unsigned_32
--                           (The_Entry.First_Cluster_Number_High));
      MaRTE.Direct_IO.Put (":Sz=");
      MaRTE.Direct_IO.Put (BIT.Unsigned_32
                           (The_Entry.Size_Of_File_In_Bytes));
      if With_Dates then
         MaRTE.Direct_IO.Put (":C=");
         Show_Date_And_Time (The_Entry.Creation_Date);
         MaRTE.Direct_IO.Put (":M=");
         Show_Date_And_Time (The_Entry.Last_Modification_Date);
         MaRTE.Direct_IO.Put (":A=");
         Show_Date (The_Entry.Last_Access_Date);
      end if;

   end Show_Dir_Entry;

end FAT.Check.Root_Dir;
