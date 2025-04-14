with FAT.Superblock;
with FAT.Sector_Cache;
with FAT.Clusters;
with FAT.Root_Directory;

package body FAT.Directory_Entries is
   use type BIT.Unsigned_32;

   Initialized : Boolean := False;

   ------------------------------
   -- Directory_Entry_Looks_OK --
   ------------------------------

   function Directory_Entry_Looks_OK (The_Entry : Directory_Entry)
                                      return Boolean is
      use type BIT.Unsigned_16;
   begin
      --  Unallocated or deleted?: entry OK

      if The_Entry.File_Name_8_3 (1) = Char_For_Deleted_File or
        The_Entry.File_Name_8_3 (1) = Char_For_Unallocated_File then
         return True;
      end if;

      --  Must be a file, a directory or a volume

      if (The_Entry.Attributes and (ARCHIVE or DIRECTORY or VOLUME_ID)) = 0
      then
         return False;
      end if;

      --  No unused attribute bits

      if (The_Entry.Attributes and not All_Attributes) /= 0 then
         return False;
      end if;

      --  No FAT32
      --  Long name entries are not considered since they have a 65535 in this
      --  field

      if The_Entry.First_Cluster_Number_High /= 0 and not
        ((The_Entry.Attributes and WIN2000_Attributes) = WIN2000_Attributes)
      then
         return False;
      end if;

      return True;
   end Directory_Entry_Looks_OK;

   --------------------------
   -- Entry_Is_A_Directory --
   --------------------------

   function Entry_Is_A_Directory (The_Entry : Directory_Entry)
                                  return Boolean is
   begin
      FAT.Assert (Directory_Entry_Looks_OK (The_Entry));

      return (The_Entry.Attributes and DIRECTORY) = DIRECTORY;
   end Entry_Is_A_Directory;

   -------------------------
   -- Entry_Is_An_Archive --
   -------------------------

   function Entry_Is_An_Archive (The_Entry : Directory_Entry)
                                 return Boolean is
   begin
      FAT.Assert (Directory_Entry_Looks_OK (The_Entry));

      return (The_Entry.Attributes and ARCHIVE) = ARCHIVE;
   end Entry_Is_An_Archive;

   --------------------------
   -- Is_A_Long_Name_Entry --
   --------------------------

   function Is_A_Long_Name_Entry (The_Entry : Directory_Entry)
                                  return Boolean is
   begin
      FAT.Assert (Directory_Entry_Looks_OK (The_Entry));

      return
        (The_Entry.Attributes and WIN2000_Attributes) = WIN2000_Attributes;
   end Is_A_Long_Name_Entry;

   ------------------
   -- Update_Dates --
   ------------------

   procedure Update_Dates
     (The_Entry                : in out Directory_Entry;
      Update_Creation_Time     : Boolean;
      Update_Modification_Time : Boolean;
      Update_Access_Date       : Boolean) is

      use type BIT.Integer_32;

      type Struct_Tm is record
         Tm_Sec  : BIT.Integer_32;
         Tm_Min  : BIT.Integer_32;
         Tm_Hour : BIT.Integer_32;
         Tm_MDay : BIT.Integer_32;
         Tm_Mon  : BIT.Integer_32;
         Tm_Year : BIT.Integer_32;
         Tm_WDay : BIT.Integer_32;
         Tm_YDay : BIT.Integer_32;
         Tm_Isdst : BIT.Integer_32; --  daylight saving time
      end record;
      pragma Pack (Struct_Tm);

      type Struct_Tm_Ac is access all Struct_Tm;

      function Time (T : access BIT.Integer_32) return BIT.Integer_32;
      pragma Import (C, Time, "time");

      function LocalTime_R (T : access BIT.Integer_32;
                            Tm : Struct_Tm_Ac)
                         return Struct_Tm_Ac;
      pragma Import (C, LocalTime_R, "localtime_r");

      function Get_Time return Struct_Tm is
         T : aliased BIT.Integer_32;
         Tm : aliased Struct_Tm;
         Tm_Ac : Struct_Tm_Ac;
      begin
         T := Time (T'Access);
         Tm_Ac := LocalTime_R (T'Access, Tm'Unchecked_Access);
         return Tm;
      end Get_Time;

      function Now_To_Date (Now : Struct_TM) return Time_And_Date is
      begin
         return
           (Seconds => Number_5_Bytes (Now.Tm_Sec / 2),
            Minutes => Number_6_Bytes (Now.Tm_Min),
            Hour    => Number_5_Bytes (Now.Tm_Hour),
            Day     => Number_5_Bytes (Now.Tm_MDay),
            Month   => Number_4_Bytes (Now.Tm_Mon + 1),
            Year    => Number_7_Bytes (Now.Tm_Year - (1980 - 1900)));
      end Now_To_Date;

      function Now_To_Time (Now : Struct_TM) return Date is
      begin
         return
           (Day     => Number_5_Bytes (Now.Tm_MDay),
            Month   => Number_4_Bytes (Now.Tm_Mon + 1),
            Year    => Number_7_Bytes (Now.Tm_Year - (1980 - 1900)));
      end Now_To_Time;

      Now : aliased Struct_Tm;
   begin
      Now := Get_Time;

      if Update_Creation_Time then
         The_Entry.Creation_Date := Now_To_Date (Now);
      end if;

      if Update_Modification_Time then
         The_Entry.Last_Modification_Date := Now_To_Date (Now);
      end if;

      if Update_Access_Date then
         The_Entry.Last_Access_Date := Now_To_Time (Now);
      end if;
   end Update_Dates;

   -------------------
   -- Get_Dir_Entry --
   -------------------

   function Get_Dir_Entry (Entry_Sector : IDE.Sector_Offset;
                           Entry_Index : Entry_Index_In_Sector)
                           return Directory_Entry is
   begin
      FAT.Assert (Initialized);

      FAT.Assert
        ((Entry_Sector >= Superblock.Root_Directory_Sector_0 and
           Entry_Sector < Superblock.Root_Directory_Sector_0 +
             Superblock.Root_Directory_Size_In_Sectors)
         or
           (Entry_Sector > Superblock.Data_Area_Sector_0));

      if Entry_Sector < Superblock.Data_Area_Sector_0 then
         --  Sector in the root dir area

         return Root_Directory.Get_Root_Dir_Entry (Entry_Sector,
                                                   Entry_Index);

      else
         --  Sector in the cluster area

         return Clusters.Get_Subdirectory_Entry (Entry_Sector, Entry_Index);
      end if;
   end Get_Dir_Entry;

   -------------------
   -- Set_Dir_Entry --
   -------------------

   procedure Set_Dir_Entry (Entry_Sector : IDE.Sector_Offset;
                            Entry_Index : Entry_Index_In_Sector;
                            Dir_Entry : Directory_Entry) is
   begin
      FAT.Assert (Initialized);

      FAT.Assert
        ((Entry_Sector >= Superblock.Root_Directory_Sector_0 and
           Entry_Sector < Superblock.Root_Directory_Sector_0 +
             Superblock.Root_Directory_Size_In_Sectors)
         or
           (Entry_Sector > Superblock.Data_Area_Sector_0));

      if Entry_Sector < Superblock.Data_Area_Sector_0 then
         --  Sector in the root dir area

         Root_Directory.Set_Root_Dir_Entry
           (Entry_Sector, Entry_Index, Dir_Entry);

      else
         --  Sector in the cluster area

         Clusters.Set_Subdirectory_Entry
           (Entry_Sector, Entry_Index, Dir_Entry);
      end if;
   end Set_Dir_Entry;

   ----------------------------
   -- Update_Dir_Entry_Dates --
   ----------------------------

   procedure Update_Dir_Entry_Dates
     (Entry_Sector   : IDE.Sector_Offset;
      Entry_Index : Entry_Index_In_Sector;
      Update_Creation_Time : Boolean;
      Update_Modification_Time : Boolean;
      Update_Access_Date : Boolean) is
      Dir_Entry : Directory_Entries.Directory_Entry;
   begin
      --  Get file entry

      Dir_Entry := Directory_Entries.Get_Dir_Entry (Entry_Sector,
                                                    Entry_Index);

      --  Update times

      Directory_Entries.Update_Dates (Dir_Entry,
                                      Update_Creation_Time,
                                      Update_Modification_Time,
                                      Update_Access_Date);

      --  Write the entry to the disk

      Directory_Entries.Set_Dir_Entry
        (Entry_Sector,
         Entry_Index,
         Dir_Entry);
   end Update_Dir_Entry_Dates;

   --------------------------------
   -- Update_Dir_Entry_File_Size --
   --------------------------------

   procedure Update_Dir_Entry_File_Size
     (Entry_Sector   : IDE.Sector_Offset;
      Entry_Index : Entry_Index_In_Sector;
      Size_Of_File_In_Bytes : BIT.Unsigned_32)
   is
      Dir_Entry : Directory_Entries.Directory_Entry;
   begin
      --  Get file entry

      Dir_Entry := Directory_Entries.Get_Dir_Entry (Entry_Sector,
                                                    Entry_Index);

      --  Update times

      Dir_Entry.Size_Of_File_In_Bytes := Size_Of_File_In_Bytes;

      --  Write the entry to the disk

      Directory_Entries.Set_Dir_Entry
        (Entry_Sector,
         Entry_Index,
         Dir_Entry);
   end Update_Dir_Entry_File_Size;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      FAT.Assert (Initialized);

      Root_Directory.Flush;
      Clusters.Flush;
   end Flush;

   --------------------------
   -- Flush_And_Invalidate --
   --------------------------

   procedure Flush_And_Invalidate is
   begin
      FAT.Assert (Initialized);

      Root_Directory.Flush_And_Invalidate;
      Clusters.Flush_And_Invalidate;
   end Flush_And_Invalidate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      FAT.Assert (not Initialized);

      FAT.Assert (Directory_Entries_Sector'Size / 8 = IDE.SECTOR_SIZE);

      Initialized := True;
   end Initialize;

end FAT.Directory_Entries;
