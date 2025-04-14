--  Management of the clusters area
--
--  Clusters may contain file data or subdirectory entries.
--
--  XXX: when used to write/read data, this file should be split in two: one
--  for subdirectory entries and other for file data. A different cache should
--  be used for each of them (to be able of invalidating or flushing
--  independenly each of them). Moreover, it could be a good idea to have a
--  diferent cache for each open file.
--
--  TODO: improve the Raw data access.

with IDE.Partitions;
with MaRTE.Integer_Types;

with FAT.Directory_Entries;

package FAT.Clusters is
   package BIT renames MaRTE.Integer_Types;

   -----------------------------------------------------
   -- Conversions between sector and clusters numbers --
   -----------------------------------------------------

   function Sector_To_Cluster (Sector : IDE.Sector_Offset)
                               return Cluster_Number;

   function First_Sector_In_Cluster (Cluster_Num : Cluster_Number)
                                     return IDE.Sector_Offset;

   function Last_Sector_In_Cluster (Cluster_Num : Cluster_Number)
                                    return IDE.Sector_Offset;

   ---------------------
   -- Raw_Data_Sector --
   ---------------------

   --  Sector in the disk seen as a raw array of bytes

   type Raw_Data_Sector is
     array (0 .. IDE.SECTOR_SIZE - 1) of BIT.Unsigned_8;
   pragma Pack (Raw_Data_Sector);

   type Raw_Data_Sector_Ac is access all Raw_Data_Sector;

   -------------------------
   -- Get_Raw_Data_Sector --
   -------------------------

   --  XXX: if could be better to have functions to get/set an slice of bytes
   --  of a raw sector. The set funcion should behave like the set in
   --  FAT.Table: use a cached sector and only write it when a different
   --  sector is accessed and the former one has been changed.
   --  XXX: in case serveral files are going to be accessed at the same time
   --  maybe it would be better to have a cached sector per open file.

   function Get_Raw_Data_Sector
     (Cluster_Num : Cluster_Number;
      Sector_Offset : IDE.Sector_Offset)
      return Raw_Data_Sector_Ac;

   -----------------------------------------------------------------------
   -- Index of a Directory_Entry in a subdirectory in the clusters area --
   -----------------------------------------------------------------------

   --  In the range 0 .. Last_Sub_Dir_Entry_Index
   --  XXX: we assume each subdir only uses a cluster to store its entries.

   type Entry_Index_In_Cluster is new Natural;

   function Last_Entry_Index_In_Cluster return Entry_Index_In_Cluster;

   -----------------------------
   -- Get_Subdirectory_Entry --
   -----------------------------

   function Get_Subdirectory_Entry
     (Cluster_Num : Cluster_Number;
      Entry_Index : Entry_Index_In_Cluster)
      return Directory_Entries.Directory_Entry;

   ----------------------------
   -- Set_Subdirectory_Entry --
   ----------------------------

   --  Write data to the disk inmediatelly

   procedure Set_Subdirectory_Entry
     (Cluster_Num : Cluster_Number;
      Entry_Index : Entry_Index_In_Cluster;
      Dir_Entry : Directory_Entries.Directory_Entry);

   ----------------------------
   -- Get_Subdirectory_Entry --
   ----------------------------

   --  Gets the directory entry which is in the position indicated by the
   --  Entry_Index in the sector indicated by Entry_Sector.
   --  The Entry_Sector should be a sector in a cluster in the cluster area
   --  that belongs to a subdirectory.

   function Get_Subdirectory_Entry
     (Entry_Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector)
      return Directory_Entries.Directory_Entry;

   ----------------------------
   -- Set_Subdirectory_Entry --
   ----------------------------

   --  Sets the directory entry which is in the position indicated by the
   --  Entry_Index in the sector indicated by Entry_Sector.
   --  The Entry_Sector should be a sector in a cluster in the cluster area
   --  that belongs to a subdirectory.

   procedure Set_Subdirectory_Entry
     (Entry_Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector;
      Dir_Entry : Directory_Entries.Directory_Entry);

   -----------
   -- Flush --
   -----------

   procedure Flush;

   --------------------------
   -- Flush_And_Invalidate --
   --------------------------

   procedure Flush_And_Invalidate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

end FAT.Clusters;
