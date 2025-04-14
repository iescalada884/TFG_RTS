with Ada.Unchecked_Conversion;
with System;

with MaRTE.Kernel.File_System_Data_Types;
with MaRTE.Direct_IO;

package body FAT.Superblock is
   use type BIT.Unsigned_8, BIT.Unsigned_16, IDE.Sector_Offset;

   Initialized : Boolean := False;

   --  Bytes per each directory entry in the "Root Dir Region".
   --  It is a fixed value defined by the FAT standard

   Bytes_Per_Roor_Dir_Entry : constant := 32;

   --------------------------
   --  Superblock contents --
   --------------------------

   Superblock : FAT16_Superblock;

   Superblock_Sector_Offset : IDE.Sector_Offset;
   --  Position of the superblock sector in the disk. It is the sector where
   --  the partition begins.

   ---------------------
   -- Sectors_Per_FAT --
   ---------------------

   function Sectors_Per_FAT return IDE.Sector_Offset is
   begin
      FAT.Assert (Initialized);

      return IDE.Sector_Offset (Superblock.Sectors_Per_FAT);
   end Sectors_Per_FAT;

   -------------------
   -- FAT1_Sector_0 --
   -------------------

   function FAT1_Sector_0 return IDE.Sector_Offset is
   begin
      FAT.Assert (Initialized);

      return Superblock_Sector_Offset +
        IDE.Sector_Offset (Superblock.Reserved_Sectors);
   end FAT1_Sector_0;

   -------------------
   -- FAT2_Sector_0 --
   -------------------

   function FAT2_Sector_0 return IDE.Sector_Offset is
   begin
      FAT.Assert (Initialized);
      if Superblock.FAT_Copies = 1 then
         MaRTE.Direct_IO.Put_Error ("Error: no FAT2 in media");
      end if;

      return FAT1_Sector_0 + Sectors_Per_FAT;
   end FAT2_Sector_0;

   -----------------------------
   -- Root_Directory_Sector_0 --
   -----------------------------

   function Root_Directory_Sector_0 return IDE.Sector_Offset is
   begin
      FAT.Assert (Initialized);

      return FAT1_Sector_0 +
        IDE.Sector_Offset (BIT.Unsigned_16 (Superblock.FAT_Copies) *
                             Superblock.Sectors_Per_FAT);
   end Root_Directory_Sector_0;

   ------------------------------------
   -- Root_Directory_Size_In_Sectors --
   ------------------------------------

   function Root_Directory_Size_In_Sectors return IDE.Sector_Offset is
   begin
      FAT.Assert (Initialized);

      --  This calculation will round up to a number of sectors (that is the
      --  reason of the "(Bytes_Per_Sector - 1)" term
      return
        IDE.Sector_Offset
        (((Superblock.Root_Dir_Entries * Bytes_Per_Roor_Dir_Entry) +
            (Superblock.Bytes_Per_Sector - 1)) /
           Superblock.Bytes_Per_Sector);
   end Root_Directory_Size_In_Sectors;

   ------------------------
   -- Data_Area_Sector_0 --
   ------------------------

   function Data_Area_Sector_0 return IDE.Sector_Offset is
   begin
      FAT.Assert (Initialized);

      return Root_Directory_Sector_0 + Root_Directory_Size_In_Sectors;
   end Data_Area_Sector_0;

   -------------------------------
   -- Data_Area_Size_In_Sectors --
   -------------------------------

   function Data_Area_Size_In_Sectors return IDE.Sector_Offset is
   begin
      return Sectors_In_FS - IDE.Sector_Offset (Superblock.Reserved_Sectors)
        - Sectors_Per_FAT * IDE.Sector_Offset (Superblock.FAT_Copies)
        - Root_Directory_Size_In_Sectors;
   end Data_Area_Size_In_Sectors;

   -------------------
   -- Sectors_In_FS --
   -------------------

   function Sectors_In_FS return IDE.Sector_Offset is
      Total_Sector_Num : IDE.Sector_Offset;
   begin
      FAT.Assert (Initialized);

      Total_Sector_Num := IDE.Sector_Offset (Superblock.Sectors_In_FS);

      if Total_Sector_Num = 0 then
         Total_Sector_Num := IDE.Sector_Offset (Superblock.Sectors_In_FS_Large);
      end if;

      return Total_Sector_Num;
   end Sectors_In_FS;

   --------------------------
   -- Sector_Size_In_Bytes --
   --------------------------

   function Sector_Size_In_Bytes return BIT.Unsigned_16 is
   begin
      FAT.Assert (Initialized);

      return Superblock.Bytes_Per_Sector;
   end Sector_Size_In_Bytes;

   -------------------------
   -- Sectors_Per_Cluster --
   -------------------------

   function Sectors_Per_Cluster return IDE.Sector_Offset is
   begin
      return IDE.Sector_Offset (Superblock.Sectors_Per_Cluster);
   end Sectors_Per_Cluster;

   -----------------------
   -- Superblock_Sector --
   -----------------------

   function Superblock_Sector return IDE.Sector_Offset is
   begin
      return Superblock_Sector_Offset;
   end Superblock_Sector;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Offset : IDE.Sector_Offset) is

      function To_Buffer_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                             FSDT.Buffer_Ac);
      use type BIT.Int;
      Error : BIT.Int;
   begin
      FAT.Assert (not Initialized);
      FAT.Assert (Superblock'Size / 8 = IDE.SECTOR_SIZE);

      Superblock_Sector_Offset := Offset;

      Error := IDE.RW_Sectors (Sb.Part.Drv, IDE.Read, Offset, 1,
                               To_Buffer_Ac (Superblock'Address));
      FAT.Assert (Error /= IDE.Error);

      Initialized := True;

      --  Check values are equal to the ones read in the original driver

      FAT.Assert (FAT.Sb.BPB_BytsPerSec = Superblock.Bytes_Per_Sector,
                  "Superblock: wrong Bytes_Per_Sector");
      FAT.Assert (FAT.Sb.BPB_SecPerClus = Superblock.Sectors_Per_Cluster,
                  "Superblock: wrong Sectors_Per_Cluster");
      FAT.Assert (FAT.Sb.BPB_RsvdSecCnt = Superblock.Reserved_Sectors,
                  "Superblock: wrong Reserved_Sectors");
      FAT.Assert (FAT.Sb.BPB_NumFATs = Superblock.FAT_Copies,
                  "Superblock: wrong FAT_Copies");
      FAT.Assert (FAT.Sb.BPB_RootEntCnt = Superblock.Root_Dir_Entries,
                  "Superblock: wrong Root_Dir_Entries");
      FAT.Assert (FAT.Sb.BPB_TotSec16 = Superblock.Sectors_In_FS,
                  "Superblock: wrong Sectors_In_FS");
      FAT.Assert (FAT.Sb.BPB_FATSz16 = Superblock.Sectors_Per_FAT,
                  "Superblock: wrong Sectors_Per_FAT");
      FAT.Assert (FAT.Sb.BPB_HiddSec =
                    BIT.Unsigned_32 (Superblock.Hidden_Sectors),
                  "Superblock: wrong Hidden_Sectors");
      FAT.Assert (FAT.Sb.FAT_Table = FAT1_Sector_0,
                  "Superblock: wrong FAT_Sector_0");
      FAT.Assert (FAT.Sb.Root_Dir = Root_Directory_Sector_0,
                  "Superblock: wrong Root_Directory_Sector_0");
      FAT.Assert (FAT.Sb.Data_Area = Data_Area_Sector_0,
                  "Superblock: wrong ");
   end Initialize;

end FAT.Superblock;
