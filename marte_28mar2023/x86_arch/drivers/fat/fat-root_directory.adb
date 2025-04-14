with FAT.Superblock;
with FAT.Sector_Cache;

package body FAT.Root_Directory is
   use type BIT.Unsigned_32;

   Initialized : Boolean := False;

   -------------------------------
   -- Last_Root_Dir_Entry_Index --
   -------------------------------

   function Last_Root_Dir_Entry_Index return Root_Dir_Entry_Index is
   begin
      return Root_Dir_Entry_Index
        (Directory_Entries.Num_Of_Entries_Per_Sector *
           Superblock.Root_Directory_Size_In_Sectors) - 1;
   end Last_Root_Dir_Entry_Index;

   --  Conversions of Root_Dir_Entry_Index to its correspondig sector and
   --  index inside the sector

   function To_Sector_Offset (Entry_Index : Root_Dir_Entry_Index)
                              return IDE.Sector_Offset is
   begin
      Assert (Entry_Index <= Last_Root_Dir_Entry_Index,
              "Error: wrong Root_Dir_Entry_Index");

      return
        IDE.Sector_Offset (Entry_Index /
                             Directory_Entries.Num_Of_Entries_Per_Sector) +
        Superblock.Root_Directory_Sector_0;
   end To_Sector_Offset;

   function To_Entry_Index_In_Sector
     (Entry_Index : Root_Dir_Entry_Index)
      return Directory_Entries.Entry_Index_In_Sector is
   begin
      Assert (Entry_Index <= Last_Root_Dir_Entry_Index,
              "Error: wrong Root_Dir_Entry_Index");

      return Directory_Entries.Entry_Index_In_Sector
        (Entry_Index mod Directory_Entries.Num_Of_Entries_Per_Sector);
   end To_Entry_Index_In_Sector;

   ----------------------------
   -- Root dir Sector Cached --
   ----------------------------

   --  The last read Root dir sector is stored to be used without the necessity
   --  of reading it again

   Cached_Root_Dir_Sector : Directory_Entries.Directory_Entries_Sector;
   Cache : Sector_Cache.Cached_Sector;

   -------------------------
   -- Find_Root_Dir_Entry --
   -------------------------

   procedure Find_Root_Dir_Entry
     (File_Name : Directory_Entries.Short_File_Name;
      Dir_Entry : out Directory_Entries.Directory_Entry;
      Entry_Index : out Root_Dir_Entry_Index;
      Found : out Boolean) is
   begin
      FAT.Assert (Initialized, "Error Root_Directory not initialized");

      --  Look in each root directory entry looking for the file

      for I in 0 .. Last_Root_Dir_Entry_Index loop
         --  Does the entry corresponds to the file?

         if Get_Root_Dir_Entry (I).File_Name_8_3 = File_Name then
            --  File found

            Found := True;
            Entry_Index := I;
            Dir_Entry := Get_Root_Dir_Entry (I);

            return; --  search finished
         end if;
      end loop;

      -- Not found

      Found := False;
   end Find_Root_Dir_Entry;

   ------------------------
   -- Get_Root_Dir_Entry --
   ------------------------

   function Get_Root_Dir_Entry (Entry_Index : Root_Dir_Entry_Index)
                                return Directory_Entries.Directory_Entry is
   begin
      return Get_Root_Dir_Entry (To_Sector_Offset (Entry_Index),
                                 To_Entry_Index_In_Sector (Entry_Index));
   end Get_Root_Dir_Entry;

   ------------------------
   -- Set_Root_Dir_Entry --
   ------------------------

   procedure Set_Root_Dir_Entry
     (Entry_Index : Root_Dir_Entry_Index;
      Dir_Entry : Directory_Entries.Directory_Entry) is
   begin
      Set_Root_Dir_Entry (To_Sector_Offset (Entry_Index),
                          To_Entry_Index_In_Sector (Entry_Index),
                          Dir_Entry);
   end Set_Root_Dir_Entry;

   ------------------------
   -- Get_Root_Dir_Entry --
   ------------------------

   function Get_Root_Dir_Entry
     (Entry_Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector)
      return Directory_Entries.Directory_Entry is
      The_Entry : Directory_Entries.Directory_Entry;
   begin
      FAT.Assert (Initialized);

      FAT.Assert (Entry_Sector >= Superblock.Root_Directory_Sector_0 and
                    Entry_Sector < Superblock.Root_Directory_Sector_0 +
                      Superblock.Root_Directory_Size_In_Sectors);

      --  Read the sector containing the entry

      Sector_Cache.Write_When_Required_And_Update (Cache, Entry_Sector);

      --  Return the entry

      The_Entry := Cached_Root_Dir_Sector (Entry_Index);
      FAT.Assert (Directory_Entries.Directory_Entry_Looks_OK (The_Entry));
      return The_Entry;
   end Get_Root_Dir_Entry;

   ------------------------
   -- Set_Root_Dir_Entry --
   ------------------------

   procedure Set_Root_Dir_Entry
     (Entry_Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector;
      Dir_Entry : Directory_Entries.Directory_Entry) is
   begin
      FAT.Assert (Initialized);

      FAT.Assert (Entry_Sector >= Superblock.Root_Directory_Sector_0 and
                    Entry_Sector < Superblock.Root_Directory_Sector_0 +
                      Superblock.Root_Directory_Size_In_Sectors);

      FAT.Assert (Directory_Entries.Directory_Entry_Looks_OK (Dir_Entry));

      --  Read the sector containing the entry

      Sector_Cache.Write_When_Required_And_Update (Cache, Entry_Sector);

      --  Set the new values for the entry

      Cached_Root_Dir_Sector (Entry_Index) := Dir_Entry;

      --  Set changes are pending to be written to the disk

      Sector_Cache.Set_Changes_Pending (Cache);

   end Set_Root_Dir_Entry;

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

      Sector_Cache.Initialize
        (Cache,
         Cached_Root_Dir_Sector'Address,
         S_Offset_Min => Superblock.Root_Directory_Sector_0,
         S_Offset_Max => Superblock.Root_Directory_Sector_0 +
           Superblock.Root_Directory_Size_In_Sectors - 1);

      Initialized := True;
   end Initialize;

end FAT.Root_Directory;
