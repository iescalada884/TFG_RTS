--  Show information about the FAT table

with IDE.Partitions;

with FAT.Directory_Entries;

package FAT.Check is

   ------------------------
   -- Show_File_Clusters --
   ------------------------

   --  XXX: only looks for the file in the root directory

   procedure Show_File_Clusters
     (File_Name : Directory_Entries.Short_File_Name);

   -------------------
   -- Show_FAT_Info --
   -------------------

   --  Calls Get_FAT_Info and show the info in the console
   --  Called from fat_driver_functions.Create

   procedure Show_FAT_Info;

   -------------------------------
   -- Collected FAT information --
   -------------------------------

   type FAT_Info is record
      First_Sector_In_Partition : IDE.Sector_Offset;
      Sectors_Per_FAT : IDE.Sector_Offset;
      FAT_Sector_0 : IDE.Sector_Offset;
      Root_Directory_Sector_0 : IDE.Sector_Offset;
      Cluster_Space_Sector_0 : IDE.Sector_Offset;

      Num_Of_Free_Entries : Natural := 0;
      Num_Of_Used_Entries : Natural := 0;
      Num_Of_EOF_Entries : Natural := 0;
      --  EOF entries with the EOF value used by the MaRTE driver

      Num_Of_Bad_Entries : Natural := 0;
      Num_Of_Reserved_Entries : Natural := 0;
      Num_Of_Forbidden_Entries : Natural := 0;
      Num_Of_Other_EOF_Entries : Natural := 0;
      --  EOF entries with an EOF value different from the one used by the
      --  MaRTE driver
   end record;

   ------------------
   -- Get_FAT_Info --
   ------------------

   function Get_FAT_Info return FAT_Info;

end FAT.Check;
