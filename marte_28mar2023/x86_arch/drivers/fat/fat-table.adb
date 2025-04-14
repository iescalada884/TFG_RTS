with System;

with IDE;
with MaRTE.Direct_IO;
with FAT.Superblock;
with FAT.Sector_Cache;

package body FAT.Table is

   use type BIT.Unsigned_32;

   Initialized : Boolean := False;

   ----------------------------------
   -- File_Cluster_Chain_Number_OK --
   ----------------------------------

   function File_Cluster_Chain_Number_OK (Cluster_Num : Cluster_Number)
                                          return Boolean is
   begin
      return Cluster_Num in Used_Entry_First .. Used_Entry_Last or
        Is_EOF_Cluster (Cluster_Num);
   end File_Cluster_Chain_Number_OK;

   --------------------
   -- Is_EOF_Cluster --
   --------------------

   function Is_EOF_Cluster (Cluster_Num : Cluster_Number)
                            return Boolean is
   begin
      return Cluster_Num in EOF_Entry_First .. EOF_Entry_Last;
   end Is_EOF_Cluster;

   --------------------
   -- Entry_Index_OK --
   --------------------

   function Entry_Index_OK (Entry_Index : FAT_Entry_Index)
                            return Boolean is
   begin
      return Entry_Index in 2 .. Last_FAT_Entry_Index;
   end Entry_Index_OK;

   ---------------------
   -- Is_Free_Cluster --
   ---------------------

   function Is_Free_Cluster (Cluster_Num : Cluster_Number)
                            return Boolean is
   begin
      return Cluster_Num = Free_Entry;
   end Is_Free_Cluster;

   ---------------------------------------------
   -- FAT sector: an array of Cluster Numbers --
   ---------------------------------------------

   Num_Of_Entries_Per_Sector : constant :=
     IDE.SECTOR_SIZE / (Cluster_Number'Size / 8);

   type Entry_Index_In_Sector is new Natural range
     0 .. Num_Of_Entries_Per_Sector - 1;

   type FAT_Sector is array (Entry_Index_In_Sector) Of Cluster_Number;
   pragma Pack (FAT_Sector);

   -- Conversions of FAT_Entry_Index to its correspondig sector and
   -- index inside the sector

   function To_FAT1_Sector_Offset (Entry_Index : FAT_Entry_Index)
                                  return IDE.Sector_Offset is
   begin
      Assert (Entry_Index_OK (Entry_Index));

      return IDE.Sector_Offset (Entry_Index / Num_Of_Entries_Per_Sector) +
        Superblock.FAT1_Sector_0;
   end To_FAT1_Sector_Offset;

   function To_Entry_Index_In_Sector (Entry_Index : FAT_Entry_Index)
                                      return Entry_Index_In_Sector is
   begin
      Assert (Entry_Index_OK (Entry_Index));

      return Entry_Index_In_Sector (Entry_Index mod Num_Of_Entries_Per_Sector);
   end To_Entry_Index_In_Sector;

   -----------------------
   -- FAT Sector Cached --
   -----------------------

   --  The last read FAT sector is stored to be used without the necessity of
   --  reading it again

   Cached_FAT_Sector : FAT_Sector;
   Cached_Sector : Sector_Cache.Cached_Sector;

   --------------------------
   -- Last_FAT_Entry_Index --
   --------------------------

   function Last_FAT_Entry_Index return FAT_Entry_Index is
      Entries_Per_FAT : FAT_Entry_Index;
      Clusters_In_Data_Area_Plus_1 : FAT_Entry_Index;
   begin
      Entries_Per_FAT := FAT_Entry_Index (Superblock.Sectors_Per_FAT) *
        Num_Of_Entries_Per_Sector - 1;
      Clusters_In_Data_Area_Plus_1 :=
        FAT_Entry_Index (Superblock.Data_Area_Size_In_Sectors /
                           Superblock.Sectors_Per_Cluster) + 1;
      --  The reason of the "+ 1" is: if we have 10 clusters in data area
      --  we need 10 entries in the FAT that will be numbered from 2 to 11.

      -- return the largest of both

      return FAT_Entry_Index'Min (Entries_Per_FAT,
                                  Clusters_In_Data_Area_Plus_1);
   end Last_FAT_Entry_Index;

   ------------------------
   -- Get_Cluster_Number --
   ------------------------

   function Get_Cluster_Number (Entry_Index : FAT_Entry_Index)
                                return Cluster_Number is
   begin
      FAT.Assert (Initialized);
      FAT.Assert (Entry_Index_OK (Entry_Index));

      Sector_Cache.Write_When_Required_And_Update
        (Cached_Sector,
         To_FAT1_Sector_Offset (Entry_Index));

      return Cached_FAT_Sector (To_Entry_Index_In_Sector (Entry_Index));
   end Get_Cluster_Number;

   ------------------------
   -- Set_Cluster_Number --
   ------------------------

   procedure Set_Cluster_Number (Entry_Index : FAT_Entry_Index;
                                 Cluster_Num : Cluster_Number) is
      Former_Cluster_Num : Cluster_Number;
   begin
      FAT.Assert (Initialized);
      FAT.Assert (Entry_Index_OK (Entry_Index));
      FAT.Assert (Cluster_Num = Free_Entry or
                    File_Cluster_Chain_Number_OK (Cluster_Num));

      --  Read the current value of the entry.
      --  In this operation the cached sector is updated (if required) and
      --  the previous is written to the disk (if there were pending changes)

      Former_Cluster_Num := Get_Cluster_Number (Entry_Index);

      --  Update the cluster number

      if Former_Cluster_Num /= Cluster_Num then
         Cached_FAT_Sector (To_Entry_Index_In_Sector (Entry_Index)) :=
           Cluster_Num;
         Sector_Cache.Set_Changes_Pending (Cached_Sector);
      end if;
   end Set_Cluster_Number;

   ----------------------------
   --  Last_Free_Entry_Found --
   ----------------------------

   --  Used for Find_Free_Entry to avoid starting from the beginning of the
   --  FAT in every search.

   Last_Free_Entry_Found : FAT_Entry_Index;

   -----------------------
   --  Find_Free_Entry  --
   -----------------------

   procedure Find_Free_Entry (Entry_Index : out FAT_Entry_Index;
                              FAT_Full : out Boolean) is
   begin
      FAT.Assert (Initialized);

      FAT_Full := True; --  asume it is full

      --  Lock for a free entry starting for the position just after the
      --  last free entry found.

      Entry_Index := Last_Free_Entry_Found;
      loop
         --  Go to next index. If reach the end of the FAT, start from
         --  the beggining

         if Entry_Index = Last_FAT_Entry_Index then
            Entry_Index := FAT_Entry_Index'First;
         else
            Entry_Index := Entry_Index + 1;
         end if;

         --  If it goes all round and return to the Last_Free_Entry_Found that
         --  means that the FAT is full

         exit when Entry_Index = Last_Free_Entry_Found; --  No free entries

         --  Check if we have found a free entry

         if Is_Free_Cluster (Get_Cluster_Number (Entry_Index)) then
            --  Found a free entry at the current value of Entry_Index
            Last_Free_Entry_Found := Entry_Index;
            FAT_Full := False;
            exit;
         end if;
      end loop;
   end Find_Free_Entry;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      FAT.Assert (Initialized);

--      FAT.Assert_Not_Fatal
--        (not Sector_Cache.Is_Invalid (Cached_Sector),
--         "Sector: " & Sector_Cache.Sector_Offset (Cached_Sector)'Img);

      Sector_Cache.Write_When_Required (Cached_Sector);
   end Flush;

   --------------------------
   -- Flush_And_Invalidate --
   --------------------------

   procedure Flush_And_Invalidate is
   begin
      FAT.Assert (Initialized);

      Sector_Cache.Write_When_Required_And_Invalidate (Cached_Sector);
   end Flush_And_Invalidate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      FAT.Assert (not Initialized);

      FAT.Assert (FAT_Sector'Size / 8 = IDE.SECTOR_SIZE);

      Last_Free_Entry_Found := Last_FAT_Entry_Index;

      Sector_Cache.Initialize
        (Cached_Sector,
         Cached_FAT_Sector'Address,
         S_Offset_Min => Superblock.FAT1_Sector_0,
         S_Offset_Max => Superblock.FAT1_Sector_0 +
           Superblock.Sectors_Per_FAT - 1,
         Mirrored => True,
         Mirror_Offset => Superblock.Sectors_Per_FAT);

      Initialized := True;
   end Initialize;

end FAT.Table;
