with FAT.Table;
with FAT.Superblock;
with FAT.Sector_Cache;
with FAT.Directory_Entries;

package body FAT.Clusters is
   use type BIT.Unsigned_32;

   Initialized : Boolean := False;

   -----------------------------------------------------
   -- Conversions between sector and clusters numbers --
   -----------------------------------------------------

   --  First cluster in the cluster area is identified by number 2

   function Sector_To_Cluster (Sector : IDE.Sector_Offset)
                               return Cluster_Number is
   begin
      FAT.Assert (Sector >= Superblock.Data_Area_Sector_0);

      return Cluster_Number
        ((Sector - Superblock.Data_Area_Sector_0) /
           Superblock.Sectors_Per_Cluster + 2);
   end Sector_To_Cluster;

   function First_Sector_In_Cluster (Cluster_Num : Cluster_Number)
                                     return IDE.Sector_Offset is
   begin
      FAT.Assert (Cluster_Num >= 2);

      return IDE.Sector_Offset (Cluster_Num - 2) *
        Superblock.Sectors_Per_Cluster +
          Superblock.Data_Area_Sector_0;
   end First_Sector_In_Cluster;

   function Last_Sector_In_Cluster (Cluster_Num : Cluster_Number)
                                    return IDE.Sector_Offset is
   begin
      FAT.Assert (Cluster_Num >= 2);

      return First_Sector_In_Cluster (Cluster_Num) +
        Superblock.Sectors_Per_Cluster - 1;
   end Last_Sector_In_Cluster;

   -------------------
   -- Sector Cached --
   -------------------

   --  The last read sector is stored to be used without the necessity of
   --  reading it again

   Cached_Sector : aliased Raw_Data_Sector;
   for Cached_Sector'Alignment use 4;
   Cached_Sector_Dir : aliased Directory_Entries.Directory_Entries_Sector;
   for Cached_Sector_Dir'Address use Cached_Sector'Address;

   Cache : Sector_Cache.Cached_Sector;

   -------------------------
   -- Get_Raw_Data_Sector --
   -------------------------

   function Get_Raw_Data_Sector
     (Cluster_Num : Cluster_Number;
      Sector_Offset : IDE.Sector_Offset)
      return Raw_Data_Sector_Ac is
   begin
      FAT.Assert (Initialized);

      FAT.Assert (False, "Error: Get_Raw_Data_Sector not implemented");

--        Sector_Cache.Update_And_Write_When_Required
--          (Cache,
--           To_Sector_Offset (Cluster_Num, Sector_Offset));

      return Cached_Sector'Access;
   end Get_Raw_Data_Sector;

   ---------------------------------
   -- Last_Entry_Index_In_Cluster --
   ---------------------------------

   function Last_Entry_Index_In_Cluster return Entry_Index_In_Cluster is
   begin
      --  XXX: we assume each subdir only uses one cluster to store its
      --  entries.

      return
        Directory_Entries.Num_Of_Entries_Per_Sector *
        Entry_Index_In_Cluster (FAT.Sb.BPB_SecPerClus) - 1;
   end Last_Entry_Index_In_Cluster;

   --  Conversions of Entry_Index_In_Cluster to its correspondig sector and
   --  index inside the sector

   function To_Entry_Index_In_Sector
     (Entry_Index : Entry_Index_In_Cluster)
      return Directory_Entries.Entry_Index_In_Sector is
   begin
      Assert (Entry_Index <= Last_Entry_Index_In_Cluster);

      return Directory_Entries.Entry_Index_In_Sector
        (Entry_Index mod Directory_Entries.Num_Of_Entries_Per_Sector);
   end To_Entry_Index_In_Sector;

   function To_Entry_Index_In_Cluster
     (Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector)
      return Entry_Index_In_Cluster is
   begin
      Assert (Sector >= Superblock.Data_Area_Sector_0);

      return Entry_Index_In_Cluster
        ((Sector - Superblock.Data_Area_Sector_0) *
           Directory_Entries.Num_Of_Entries_Per_Sector) +
        Entry_Index_In_Cluster (Entry_Index);
   end To_Entry_Index_In_Cluster;

   function To_Sector_Offset (Cluster_Num : Cluster_Number;
                              Entry_Index : Entry_Index_In_Cluster)
                              return IDE.Sector_Offset is
   begin
      FAT.Assert (Cluster_Num >= 2);

      Assert (Entry_Index <= Last_Entry_Index_In_Cluster);

      return First_Sector_In_Cluster (Cluster_Num) +
        IDE.Sector_Offset (Entry_Index /
                             Directory_Entries.Num_Of_Entries_Per_Sector);
   end To_Sector_Offset;

   -----------------------------
   -- Get_Subdirectory_Entry --
   -----------------------------

   function Get_Subdirectory_Entry
     (Cluster_Num : Cluster_Number;
      Entry_Index : Entry_Index_In_Cluster)
      return Directory_Entries.Directory_Entry is
   begin
      return
        Get_Subdirectory_Entry (To_Sector_Offset (Cluster_Num, Entry_Index),
                                To_Entry_Index_In_Sector (Entry_Index));
   end Get_Subdirectory_Entry;

   ----------------------------
   -- Set_Subdirectory_Entry --
   ----------------------------

   --  Write data to the disk inmediatelly

   procedure Set_Subdirectory_Entry
     (Cluster_Num : Cluster_Number;
      Entry_Index : Entry_Index_In_Cluster;
      Dir_Entry : Directory_Entries.Directory_Entry) is
   begin
      Set_Subdirectory_Entry (To_Sector_Offset (Cluster_Num, Entry_Index),
                              To_Entry_Index_In_Sector (Entry_Index),
                              Dir_Entry);
   end Set_Subdirectory_Entry;

   ----------------------------
   -- Get_Subdirectory_Entry --
   ----------------------------

   function Get_Subdirectory_Entry
     (Entry_Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector)
      return Directory_Entries.Directory_Entry is

      The_Entry : Directory_Entries.Directory_Entry;
   begin
      FAT.Assert (Initialized);

      FAT.Assert (Entry_Sector >= Superblock.Data_Area_Sector_0);

      --  Read the sector containing the entry

      Sector_Cache.Write_When_Required_And_Update (Cache, Entry_Sector);

      --  Return the entry

      The_Entry := Cached_Sector_Dir (Entry_Index);
      FAT.Assert (Directory_Entries.Directory_Entry_Looks_OK (The_Entry));
      return The_Entry;
   end Get_Subdirectory_Entry;

   ----------------------------
   -- Set_Subdirectory_Entry --
   ----------------------------

   procedure Set_Subdirectory_Entry
     (Entry_Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector;
      Dir_Entry : Directory_Entries.Directory_Entry) is
   begin
      FAT.Assert (Initialized);

      FAT.Assert (Entry_Sector >= Superblock.Data_Area_Sector_0);

      FAT.Assert (Directory_Entries.Directory_Entry_Looks_OK (Dir_Entry));

      Sector_Cache.Write_When_Required_And_Update (Cache, Entry_Sector);

      Cached_Sector_Dir (Entry_Index) := Dir_Entry;

      --  Set changes are pending to be written to the disk

      Sector_Cache.Set_Changes_Pending (Cache);
   end Set_Subdirectory_Entry;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      FAT.Assert (Initialized);

      Sector_Cache.Write_When_Required (Cache);
   end Flush;

   --------------------------
   -- Flush_And_Invalidate --
   --------------------------

   procedure Flush_And_Invalidate is
   begin
      FAT.Assert (Initialized);

      Sector_Cache.Write_When_Required_And_Invalidate (Cache);
   end Flush_And_Invalidate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      FAT.Assert (not Initialized);

      FAT.Assert (Raw_Data_Sector'Size / 8 = IDE.SECTOR_SIZE);

      FAT.Assert (IDE.Sector_Offset (FAT.Sb.BPB_SecPerClus) =
                    Superblock.Sectors_Per_Cluster);

      Sector_Cache.Initialize
        (Cache,
         Cached_Sector'Address,
         S_Offset_Min => Superblock.Data_Area_Sector_0,
         S_Offset_Max => IDE.Sector_Offset'Last);

      Initialized := True;
   end Initialize;

end FAT.Clusters;
