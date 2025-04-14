--  Reads the superblok of the FAT partition
--  Superblock structure is mapped into the type FAT16_Superblock

with IDE;

package FAT.Superblock is

   ---------------------
   -- Sectors_Per_FAT --
   ---------------------

   function Sectors_Per_FAT return IDE.Sector_Offset;

   -------------------
   -- FAT1_Sector_0 --
   -------------------

   --  First sector of the first copy of the FAT

   function FAT1_Sector_0 return IDE.Sector_Offset;

   -------------------
   -- FAT2_Sector_0 --
   -------------------

   --  First sector of the second copy of the FAT

   function FAT2_Sector_0 return IDE.Sector_Offset;

   -----------------------------
   -- Root_Directory_Sector_0 --
   -----------------------------

   function Root_Directory_Sector_0 return IDE.Sector_Offset;

   ------------------------------------
   -- Root_Directory_Size_In_Sectors --
   ------------------------------------

   function Root_Directory_Size_In_Sectors return IDE.Sector_Offset;

   ------------------------
   -- Data_Area_Sector_0 --
   ------------------------

   function Data_Area_Sector_0 return IDE.Sector_Offset;

   -------------------------------
   -- Data_Area_Size_In_Sectors --
   -------------------------------

   function Data_Area_Size_In_Sectors return IDE.Sector_Offset;

   -------------------
   -- Sectors_In_FS --
   -------------------

   --  Total number of sectors in the FS. Includes de sectors of the
   --  four regions which the FAT16 file system consist of: Reserved
   --  Region (incl. Boot Sector), File Allocation Table (FAT), Root
   --  Directory and Data Region

   function Sectors_In_FS return IDE.Sector_Offset;

   --------------------------
   -- Sector_Size_In_Bytes --
   --------------------------

   function Sector_Size_In_Bytes return BIT.Unsigned_16;

   -------------------------
   -- Sectors_Per_Cluster --
   -------------------------

   function Sectors_Per_Cluster return IDE.Sector_Offset;

   -----------------------
   -- Superblock_Sector --
   -----------------------

   --  Returns the the position of the superblock sector in the disk. It is the
   --  sector where the partition begins.

   function Superblock_Sector return IDE.Sector_Offset;

   ----------------
   -- Initialize --
   ----------------

   --  Read Superblock information
   --  Offset is the position of the superblock sector in the disk. It is the
   --  sector where the partition begins.

   procedure Initialize (Offset : IDE.Sector_Offset);

private

   --  FAT Superblock (BPB, Ancient acronym for Bios Parameter Block)
   --  The first sector of the disk partition

   type FAT16_Superblock is record
      Jump_To_Bootstrap_0 : BIT.Unsigned_8; -- not used
      Jump_To_Bootstrap_1 : BIT.Unsigned_8; -- not used
      Jump_To_Bootstrap_2 : BIT.Unsigned_8; -- not used
      OEM_Name : String (1 .. 8); -- not used

      Bytes_Per_Sector : BIT.Unsigned_16;
      --  The number of Bytes per sector (remember, all numbers are in the
      --  little-endian format).

      Sectors_Per_Cluster : BIT.Unsigned_8;
      --  Number of sectors per cluster.

      Reserved_Sectors : BIT.Unsigned_16;
      --  Superblock sector, and possibly other stuff.
      --  Usually this number is 1 since the boot sector is the only reserved
      --  sector.

      FAT_Copies :  BIT.Unsigned_8;
      -- Number of File Allocation Tables (FAT's) on the storage media.
      -- Often this value is 2.

      Root_Dir_Entries : BIT.Unsigned_16;
      -- Number of directory entries (must be set so that the root directory
      -- occupies entire sectors).

      Sectors_In_FS : BIT.Unsigned_16;
      -- The total sectors in the logical volume. If this value is 0,
      -- it means there are more than 65535 sectors in the volume, and the
      -- actual count is stored in Sectors_In_FS_Large (bytes 32-35).

      Media_Type : BIT.Unsigned_8; -- not used

      Sectors_Per_FAT : BIT.Unsigned_16;
      --  Number of sectors per FAT (0 for FAT32).

      Sectors_Per_Trak : BIT.Unsigned_16; -- not used
      Heads : BIT.Unsigned_16; -- not used

      Hidden_Sectors : BIT.Unsigned_32;
      --  Number of hidden sectors. Hidden sectors are sectors preceding the
      --  partition.
      --  (i.e. the LBA of the beginning of the partition.)

      Sectors_In_FS_Large : BIT.Unsigned_32;
      --  The total sectors in the logical volume. Used for FAT16
      --  volumes that use more the 65535 sectors (in this case
      --  Sectors_In_FS should be set to 0)

      Not_Used : String (1 .. 476);
      --  Part of the sector not used
   end record;
   pragma Pack (FAT16_Superblock);
   for FAT16_Superblock'Size use 512*8;

end FAT.Superblock;
