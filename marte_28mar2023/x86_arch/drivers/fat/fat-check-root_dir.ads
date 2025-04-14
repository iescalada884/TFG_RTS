-- Show information about the root dir area

with FAT.Directory_Entries;

package FAT.Check.Root_dir is
   -------------------------------
   -- Check_Root_SubDir_Entries --
   -------------------------------

   -- Checks the contents of a root subdirectory (a first level directory)

   function Check_Root_SubDir_Entries
     (Dir_Name : Directory_Entries.Short_File_Name) return Boolean;

   ------------------------------
   -- Show_Root_SubDir_Entries --
   ------------------------------

   --  Shows the contents of a root subdirectory (a first level directory)

   procedure Show_Root_SubDir_Entries
     (Dir_Name : Directory_Entries.Short_File_Name);

   ------------------------
   -- Show_Root_Dir_Info --
   ------------------------

   --  Shows a summary of the entries in the root directory: i.e. the number
   --  of "DelFiles", "UnallocFiles", "Files", "Dirs" and "Win2000Files"

   procedure Show_Root_Dir_Info;

   --------------------
   -- Show_Dir_Entry --
   --------------------

   --  Displays the data of an entry: name, size and first cluster (and
   --  modification and creation dates when With_Dates is true)

   procedure Show_Dir_Entry (The_Entry : Directory_Entries.Directory_Entry;
                             With_Dates : Boolean := False);

end FAT.Check.Root_Dir;
