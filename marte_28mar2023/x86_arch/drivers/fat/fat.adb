------------------------------------------------------------------------------
--                                   FAT body                               --
------------------------------------------------------------------------------
--  The implementation is based on Microsoft's specification.               --
--  How to use it:  test_fat.adb                                            --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  TODO: Multiple sector writes
--  TODO: support for long file names
--  TODO: directories can have several sectors
--  XXX: only tested for files in root directory
------------------------------------------------------------------------------
--  for Unchecked_Conversions
with Ada.Unchecked_Conversion;
with System;
--  for function To_Upper (short filenames are always in upper letters)
with Ada.Characters.Handling;
--  for output messages
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
--  for debugging
--  with Debug_Marte; use Debug_Marte;
with FAT.Operations;
with FAT.Table;
with FAT.Clusters;
with FAT.Superblock;
with FAT.Check;
with FAT.Directory_Entries;
with FAT.Root_Directory;

package body FAT is

   use type BIT.Unsigned_8;
   use type BIT.Unsigned_16;
   use type BIT.Unsigned_32;

   Debug_Messages : constant Boolean := False;

   function To_Upper (S : in String) return String
      renames Ada.Characters.Handling.To_Upper;

   type Uint16_Ac is access all BIT.Unsigned_16;
   function To_Uint16_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                          Uint16_Ac);

   type Uint32_Ac is access all BIT.Unsigned_32;
   function To_Uint32_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                          Uint32_Ac);

   subtype FAT_Name is String (1 .. 8);
   type FAT_Name_Ac is access all FAT_Name;
   function To_FAT_Name_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                            FAT_Name_Ac);

   function To_Buffer_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                          FSDT.Buffer_Ac);

   ------------------------------------------------
   -- Internal functions to check sector offsets --
   ------------------------------------------------

   function Is_A_FAT1_Sector (S_Offset : IDE.Sector_Offset)
                             return Boolean is
   begin
      return S_Offset >= Sb.FAT_Table and then
        S_Offset < Sb.FAT_Table + IDE.Sector_Offset (Sb.BPB_FATSz16);
   end Is_A_FAT1_Sector;

   function Is_A_Root_Dir_Sector (S_Offset : IDE.Sector_Offset)
                                 return Boolean is
   begin
      return S_Offset >= Sb.Root_Dir and then
        S_Offset < Sb.Data_Area;
   end Is_A_Root_Dir_Sector;

   function Is_A_Cluster_Data_Sector (S_Offset : IDE.Sector_Offset)
                                 return Boolean is
   begin
      return S_Offset >= Sb.Data_Area;
   end Is_A_Cluster_Data_Sector;

   --------------------------------
   -- Internal: Global variables --
   --------------------------------

   Cache_Buffer : IDE.Sector;
   Cache_Offset : IDE.Sector_Offset := IDE.Sector_Offset'Last;

   -------------------------------
   -- Internal: CACHE functions --
   -------------------------------
   --  We use a very simple Cache in order to improve read accesses. It just
   --  consists of two global variables, a single sector cache and its
   --  physical address. When we a function wants to read a sector (multiple
   --  sector reads are not allowed yet) we first check the cache just in
   --  case we have already read that sector before. The write accesses are
   --  not cached so functions can write directly into 'Cache_Buffer' and
   --  'Cache_Offset'.

   function Cache_Read_Sector
     (S_Offset : in     IDE.Sector_Offset) return BIT.Int is
   begin
      --  Now only should be used to access sectors: Part.First_Sector,
      --  sectors in root dir area and sectors in clusters area.

      if Is_A_FAT1_Sector (S_Offset) then
         Put_Error (" ERROR: FAT sectors should be only managed " &
                    "in FAT.Table!!");
      end if;

      --  MAR: this check in not possible yet, since the root dir sector is
      --  used in FAT_Create and FAT_Delete
--      if Is_A_Root_Dir_Sector (S_Offset) then
--         Put_Error (" ERROR: Root_Dir sectors should be only managed " &
--                    "in FAT.Root_Directory!!");
--      end if;

      if Cache_Offset /= S_Offset then
         Cache_Offset := S_Offset;
         return IDE.RW_Sectors (Sb.Part.Drv, IDE.Read, S_Offset, 1,
                                To_Buffer_Ac (Cache_Buffer (0)'Address));
      end if;
      return 0;
   end Cache_Read_Sector;

   function Cache_Write_Sector return BIT.Int is
   begin
      return IDE.RW_Sectors (Sb.Part.Drv, IDE.Write, Cache_Offset, 1,
                             To_Buffer_Ac (Cache_Buffer (0)'Address));
   end Cache_Write_Sector;


   -------------------------------------
   -- Internal : Get_Next_FAT_Element --
   -------------------------------------
   --  This function follows one link in the FAT linked list of Elements.
   --  That is, it takes the 'Element' parameter as the starting point and
   --  reads the next Element of the list. The FAT starts at 'Sb.FAT_Table'
   --  and it consists of Element (16bits in FAT16) that are linked to
   --  compose a File (if one element is 3FA3h the next element will be located
   --  at the 3FA3h position). The value of these elements can be:
   --     1) 0000h = available cluster
   --     2) 0002h-FFEFh = used, next cluster in File
   --     3) FFF7h = bad clusters
   --     4) FFF8h-FFFFh = used, last cluster in File
   --  The start of a File is stored on the directories.
   --  Algorithm:
   --  a) First, we need to get the SECTOR offset where the value of
   --     'Element' is stored. In the case of FAT16 each 'Sector=512bytes'
   --     contains 512/2 = 256 elements. If we want the value of
   --     Element '0..255' they will be at 'Sb.FAT_Table + 0'. If we want
   --     the value of Element '256..511' they will be at 'Sb.FAT_Table + 1'.
   --     And so on. Therefore the sector offset (its physical address) of
   --     the N Element is: Sb.FAT_Table + rounddown (N/256)
   --  b) Then, after fetching that sector, we look for the value of the
   --     Element inside that sector. This value is located at
   --     '(Element mod 512/2)*2' (for FAT16).

--     procedure Get_Next_FAT_Element_Not_Used
--        (Element      : in out BIT.Unsigned_16)
--     is
--        S_Offset : IDE.Sector_Offset;
--     begin
--        Assert (False); --  should not be used

--        -- a)  get the SECTOR (FAT16 elements = 2bytes)
--        S_Offset := Sb.FAT_Table + BIT.Unsigned_32 (Element) /
--                    (IDE.SECTOR_SIZE / 2);

--        Assert (Is_A_FAT1_Sector (S_Offset), "Error: Get_Next_FAT_Element");
--        if Cache_Read_Sector (S_Offset) /= 0 then
--           Put_Error ("Error: could not read sector"); New_Line;
--        end if;

--         -- b) look for the value of the Element
--        Element := To_Uint16_Ac (Cache_Buffer (
--                       (Element mod (IDE.SECTOR_SIZE/2))*2)'Address).all;
--     end Get_Next_FAT_Element_Not_Used;


   ----------------------------------
   -- Internal : Find_Free_Cluster --
   ----------------------------------
   --  This function looks the FAT table for a Free Cluster
   --  (0000h = available cluster).
   --  Algorithm:
   --     a) Make a loop over every SECTOR of the FAT region (Sb.BPB_FATSz16)
   --     b) For each sector, make a loop over every Element
   --     c) Look for an Element with a 0000h value.
   --     d) If we don't find any one the disk is full and we'll return and
   --     error.

--     function Find_Free_Cluster_Not_Used
--        (Cluster      : access BIT.Unsigned_16)
--         return BIT.Int
--     is
--        S_Offset : IDE.Sector_Offset;
--        New_Cluster : BIT.Unsigned_16;
--        Fat_Value : BIT.Unsigned_16;
--     begin
--        Assert (False); --  should not be used

--        S_Offset := Sb.FAT_Table;
--        --  a) Make a loop over every SECTOR of the FAT region (Sb.BPB_FATSz16)
--        for I in 0 .. (Sb.BPB_FATSz16 - 1) loop
--           Assert (Is_A_FAT1_Sector (S_Offset + BIT.Unsigned_32 (I)),
--                   "Error: Find_Free_Cluster");
--           if Cache_Read_Sector (S_Offset + BIT.Unsigned_32 (I)) /= 0 then
--              Put_Error ("Error: could not read sector"); New_Line;
--              return -1;
--           end if;
--           --  b) For each sector, make a loop over every Element
--           for J in BIT.Unsigned_16 range 0 .. 255 loop
--              New_Cluster := J + I*IDE.SECTOR_SIZE/2;
--              Fat_Value := To_Uint16_Ac (Cache_Buffer(J*2)'Address).all;
--              --  c) Look for an Element with a 0000h value
--              if Fat_Value = 16#0000# then
--                 Cluster.all := New_Cluster;
--                 return 0;
--              end if;
--           end loop;
--        end loop;
--        return -1;
--     end Find_Free_Cluster_Not_Used;

   ---------------------------------
   -- Internal : Allocate_Cluster --
   ---------------------------------
   --  This function is aimed at adding a new cluster to a File.
   --  Algorithm:
   --     a) Look for an empty cluster in the FAT (0000h = available cluster).
   --     b) Change its value by a EOF value (FFF8h-FFFFh = used, last cluster
   --        in File).
   --     c) Copy its address to the 'Cluster' parameter.
   --        If the Cluster paramter is 16#00# (the file has no clusters)
   --        copy it to the entry (in the root dir area)
   --        MAR: only suported files in the root directory.
   --     d) Clear the cluster to 0's

   function Allocate_Cluster
      (Ent   : access FAT_Entry;
       Cluster  : access BIT.Unsigned_16)
       return BIT.Int
   is
      --  S_Offset : IDE.Sector_Offset; MAR borrar
      --  New_Cluster : aliased BIT.Unsigned_16; MAR borrar
      Entry_Index : Table.FAT_Entry_Index;
      FAT_Full : Boolean;
      Dir_Entry : Directory_Entries.Directory_Entry;
   begin
      --  Put ("DEBUG: Creating a new cluster over "
      --       &BIT.Unsigned_16'Image (Cluster.all)); New_Line;

      --  a) Look for a free entry index
      Table.Find_Free_Entry (Entry_Index, FAT_Full);
      if FAT_Full then
         Put_Error ("Error: The disk is full"); New_Line;
         return -1;
      end if;

      --  b) Change its value by a EOF value (FFF8h-FFFFh = used,
      --     last cluster in File).
      Table.Set_Cluster_Number (Entry_Index, Table.EOF_Entry_First);

      --  c) Copy its address to the 'Cluster' parameter
      --  If the Cluster paramter is 16#00# (the file has no clusters)
      --  copy it to the entry
      if Cluster.all = 16#00# then
         Dir_Entry := Root_Directory.Get_Root_Dir_Entry
           (Ent.Entry_S_Offset,
            Directory_Entries.Entry_Index_In_Sector (Ent.Entry_Pos));

         Assert (Directory_Entries.Entry_Is_An_Archive (Dir_Entry));

         --  Set the first cluster number in the directory entry
         Dir_Entry.First_Cluster_Number := Cluster_Number (Entry_Index);

         Root_Directory.Set_Root_Dir_Entry
           (Ent.Entry_S_Offset,
            Directory_Entries.Entry_Index_In_Sector (Ent.Entry_Pos),
            Dir_Entry);

         --  Copy the first cluster of the file to its FAT entry
         Ent.Cluster := BIT.Unsigned_16 (Entry_Index);
      else
         --  Set the previous index of the file (received in Cluster)
         --  to the free index found above to continue the chain of
         --  entries of the file.

         Table.Set_Cluster_Number (Table.FAT_Entry_Index (Cluster.all),
                                   Cluster_Number (Entry_Index));
      end if;

      --  Cluster parameter returns the free index
      Cluster.all := BIT.Unsigned_16 (Entry_Index);

      --  Put ("DEBUG: New cluster "&BIT.Unsigned_16'Image (Cluster.all));
      --  New_Line;

      --  d) Clear the cluster to 0's
      Cache_Buffer := (others => 0);
      Cache_Offset := Sb.Data_Area + (IDE.Sector_Offset (Entry_Index) - 2) *
        IDE.Sector_Offset (Sb.BPB_SecPerClus);
      for I in 1 .. Sb.BPB_SecPerClus loop
         Assert (Is_A_Cluster_Data_Sector (Cache_Offset),
                 "Error: Clearing cluster");
         if Cache_Write_Sector /= 0 then
            Put_Error ("Error: could not write sector (allocate-clear)");
            New_Line;
            return -1;
         end if;
         Cache_Offset := Cache_Offset + 1;
      end loop;
      Cache_Offset := Cache_Offset - 1;
      return 0;
   end Allocate_Cluster;

   ----------------
   -- FAT_Create --
   ----------------
   --  This function initializes the parameters of the filesystem. These
   --  parameters are stored on the first sector of the partition.
   --  Algorithm:
   --     a) Read First sector of the partition
   --     b) Parse bytes into variables as FAT manual says.

   function FAT_Create
      (Part : in     IDE.Partitions.Partition) return BIT.Int is
   begin
      Sb.Part := Part;
      if Cache_Read_Sector (Part.First_Sector) /= 0 then
         Put_Error ("Error: could not read BPB sector"); New_Line;
         return -1;
      end if;
      Sb.BPB_BytsPerSec := To_Uint16_Ac (Cache_Buffer (11)'Address).all;
      if Sb.BPB_BytsPerSec /= IDE.SECTOR_SIZE then
         Put_Error ("Error: Sector size not supported"); New_Line;
         return -1;
      end if;
      Sb.BPB_SecPerClus := Cache_Buffer (13);
      Sb.BPB_RsvdSecCnt := To_Uint16_Ac (Cache_Buffer (14)'Address).all;
      Sb.BPB_NumFATs    := Cache_Buffer (16);
      Sb.BPB_RootEntCnt := To_Uint16_Ac (Cache_Buffer (17)'Address).all;
      Sb.BPB_TotSec16   := To_Uint16_Ac (Cache_Buffer (19)'Address).all;
      Sb.BPB_Media      := Cache_Buffer (21);
      Sb.BPB_FATSz16    := To_Uint16_Ac (Cache_Buffer (22)'Address).all;
      Sb.BPB_SecPerTrk  := To_Uint16_Ac (Cache_Buffer (24)'Address).all;
      Sb.BPB_NumHeads   := To_Uint16_Ac (Cache_Buffer (26)'Address).all;
      Sb.BPB_HiddSec    := To_Uint32_Ac (Cache_Buffer (28)'Address).all;
      Sb.BPB_TotSec32   := To_Uint32_Ac (Cache_Buffer (32)'Address).all;
      Sb.FAT_Table      := Part.First_Sector +
                           BIT.Unsigned_32 (Sb.BPB_RsvdSecCnt);

      if Sb.BPB_FATSz16 = 0 then
         Put_Error ("Error: FAT32 not implemented yet"); New_Line;
      else
         Sb.Root_Dir := Sb.FAT_Table + BIT.Unsigned_32 (Sb.BPB_NumFATs) *
                        BIT.Unsigned_32 (Sb.BPB_FATSz16);
      end if;

      Sb.Data_Area := Sb.Root_Dir + BIT.Unsigned_32 (Sb.BPB_RootEntCnt) *
        32 / BIT.Unsigned_32 (Sb.BPB_BytsPerSec);

      if Debug_Messages then
         Put ("BPB_BytsPerSec: "&BIT.Unsigned_16'Image (Sb.BPB_BytsPerSec));
         New_Line;
         Put ("BPB_SecPerClus: "&BIT.Unsigned_8'Image (Sb.BPB_SecPerClus));
         New_Line;
         Put ("BPB_RsvdSecCnt: "&BIT.Unsigned_16'Image (Sb.BPB_RsvdSecCnt));
         New_Line;
         Put ("BPB_NumFATs: "&BIT.Unsigned_8'Image (Sb.BPB_NumFATs));
         New_Line;
         Put ("BPB_RootEntCnt: "&BIT.Unsigned_16'Image (Sb.BPB_RootEntCnt));
         New_Line;
         Put ("BPB_TotSec16: "&BIT.Unsigned_16'Image (Sb.BPB_TotSec16));
         New_Line;
--         Put ("BPB_Media: "&BIT.Unsigned_8'Image (Sb.BPB_Media));
--         New_Line;
         Put ("BPB_FATSz16: "&BIT.Unsigned_16'Image (Sb.BPB_FATSz16));
         New_Line;
--         Put ("BPB_SecPerTrk: "&BIT.Unsigned_16'Image (Sb.BPB_SecPerTrk));
--         New_Line;
--         Put ("BPB_NumHeads: "&BIT.Unsigned_16'Image (Sb.BPB_NumHeads));
--         New_Line;
         Put ("BPB_HiddSec: "&BIT.Unsigned_32'Image (Sb.BPB_HiddSec));
         New_Line;
         Put ("BPB_TotSec32: "&BIT.Unsigned_32'Image (Sb.BPB_TotSec32));
         New_Line;
         Put ("FAT_Table: "&BIT.Unsigned_32'Image (Sb.FAT_Table));
         New_Line;
         Put ("Root_Dir: "&BIT.Unsigned_32'Image (Sb.Root_Dir));
         New_Line;
         Put ("Data_Area: "&BIT.Unsigned_32'Image (Sb.Data_Area));
         New_Line;
      end if;

      --  Initialize child packages

      Superblock.Initialize (Part.First_Sector);
      Directory_Entries.Initialize;
      Clusters.Initialize;
      Root_Directory.Initialize;
      Table.Initialize;
      --  Check.Show_FAT_Info; -- Debug

      return 0;
   end Fat_Create;

   --------------
   -- FAT_Open --
   --------------
   --  This function goes through the directories of a Path until it found
   --  the File entry. In a FAT16 directories start at the ROOT directory,
   --  just after the FAT tables. The rest of directories are similar to
   --  Files (different attr) but they contain File entries. (There is
   --  another difference, the size field of a directory is always 0, we
   --  have to follow the links in the FAT until the EOC mark). A File entry
   --  consists of the name, size, attributes, first cluster, ..., of a
   --  particular File.
   --
   --  The Path must be something like /dir1/dir2/file and we are going to
   --  look for the File in order 'dir1', 'dir2',...,'file'.
   --
   --  Algorithm:
   --     a) First, we search 'dir1' in the root directory, which is a fixed
   --        size region of typically 32 sectors (512 entries of 32 bytes)
   --        starting at Sb.Root_Dir.
   --     b) Then, read the rest of the directories of the path. We will read
   --        them as we read a file with one difference. We will read the
   --        directories as an infinite size files with the same function we
   --        use to read files. When we get a '-1' we will have to stop.

   function FAT_Open
      (Str   : in     FSDT.Path;
       Ent   : access FAT_Entry;
       O_CREAT : in Boolean)
       return BIT.Int
   is
      --------------------------------------------------------
      --  Next_Item: get the next item in the path
      --------------------------------------------------------
      procedure Next_Item
         (Str     : in out FSDT.Path; --  The path less the item extracted
          Item    : out    FAT_Name;  --  The item extracted
          Chars   : out    Integer)   --  The count of Characters of the item
      is
         Tmp : FSDT.Path := (others => ' ');
      begin
         Chars := 0;
         --  Str (1) should be '/'
         for I in 2 .. Str'Last loop
            case Str (I) is
              when '/' =>
                 Tmp (1 .. Str'Last - I + 1) := Str (I .. Str'Last);
                 exit;
              when ' ' => exit;
              when others =>
                 Item (I - 1) := Str (I);
                 Chars := Chars + 1;
           end case;
        end loop;
        Str := Tmp;
      end Next_Item;
      --------------------------------------------------------
      --  Search_File: search a File
      --------------------------------------------------------
      --  This function looks for a certain File/Dir in the
      --  current sector (Cache_Buffer) by comparing the
      --  name with each File_Entry.
      --------------------------------------------------------
      function Search_File
         (Tmp_Item : in FAT_Name;
          Tmp_Chars : in Integer;
          Ent : access FAT_Entry;
          Add : in Boolean) --  Add the File if we find 16#00#
          return BIT.Int
      is
         Attr : BIT.Unsigned_8;
         Name : FAT_Name := (others => ' ');

         --  <MAR
         subtype FAT_Name_With_Ext is String (1 .. 11);
         type FAT_Name_With_Ext_Ac is access all FAT_Name_With_Ext;
         function To_FAT_Name_With_Ext_Ac is
            new Ada.Unchecked_Conversion (System.Address,
                                          FAT_Name_With_Ext_Ac);
         --  MAR>
      begin
         for I in BIT.Unsigned_16 range 0 .. IDE.SECTOR_SIZE/32 -1 loop
            case Cache_Buffer(I*32) is
               when 16#00# => --  No more entries
                  if Add then
                     Name (1 .. Tmp_Chars) := To_Upper (
                                                Tmp_Item (1 .. Tmp_Chars));
         --  <MAR
         --To_FAT_Name_Ac (Cache_Buffer(I*32)'Address).all := Name;
                     To_FAT_Name_With_Ext_Ac (Cache_Buffer(I*32)'Address).all :=
                       Name & "   ";
         --  MAR>
                     Cache_Buffer(I*32 + 11) := 16#20#;
                     Ent.Cluster := 0;
                     To_Uint16_Ac (Cache_Buffer(I*32 + 26)'Address).all :=
                        Ent.Cluster;
                     Ent.Size := 0;
                     To_Uint32_Ac (Cache_Buffer(I*32 + 28)'Address).all :=
                        BIT.Unsigned_32 (Ent.Size);
                     Ent.Entry_S_Offset := Cache_Offset;
                     Ent.Entry_Pos := I;

                     Assert (Is_A_Root_Dir_Sector (Cache_Offset),
                             "Error: Creating file");
                     if Cache_Write_Sector /= 0 then
                           Put_Error ("Could not write sector (search-add)");
                           New_Line;
                           return -1;
                     end if;
                     return 0;
                  else
                     return -2;
                  end if;
               when 16#E5# => null; --  Not valid, go to the next entry
               when others =>
                  Attr := Cache_Buffer(I*32 + 11);
                  if (Attr and 16#0F#) /= 16#0F# then -- Not a long file name
                     --  Now compare the file name with the entry
                     Name (1 .. Tmp_Chars) := To_Upper (
                                                   Tmp_Item (1 .. Tmp_Chars));

                     if Name = To_FAT_Name_Ac
                                       (Cache_Buffer(I*32)'Address).all then
                        Ent.Cluster := To_Uint16_Ac
                                       (Cache_Buffer(I*32 + 26)'Address).all;
                        Ent.Size := FSDT.Buffer_Length (To_Uint32_Ac
                                       (Cache_Buffer(I*32 + 28)'Address).all);
                        Ent.Entry_S_Offset := Cache_Offset;
                        Ent.Entry_Pos := I;
                        return 0;
                     end if;
                  end if;
            end case;
         end loop;
         return -1;
      end Search_File;
      --------------------------------------------------------
      use type FSDT.Buffer_Length;
      Tmp_Path : FSDT.Path := Str;
      Tmp_Item : FAT_Name;
      Tmp_Chars : Integer;
      Offset : IDE.Sector_Offset;
      Buff : array (1 .. 32) of BIT.Unsigned_8;
      Ret : BIT.Int;
      File_Found : Boolean := False;
   begin
      --  a) In FAT16 we start looking in the ROOT directory.
      Offset := Sb.Root_Dir;

      --  Remove starting character '/' in the name (MAR ??)
      Next_Item (Tmp_Path, Tmp_Item, Tmp_Chars);

      --  ROOT dir = 512 Entries = 32 sectors
      for I in 1 .. Sb.BPB_RootEntCnt/16 loop
         --  Read a sector in the root directory
         Assert (Is_A_Root_Dir_Sector (Offset), "Error: Open");

         --  Before reading with Cache_Read_Sector we must be sure the sectors
         --  are updated
         Root_Directory.Flush_And_Invalidate;

         if Cache_Read_Sector (Offset) /= 0 then
            Put_Error ("Error: could not read Root sector"); New_Line;
            return -1;
         end if;
         --  Look for the file
         if Tmp_Path (1) = '/' then
            -- don't create directories
            Ret := Search_File (Tmp_Item, Tmp_Chars, Ent, False);
         else
            Ret := Search_File (Tmp_Item, Tmp_Chars, Ent, O_CREAT);
         end if;
         case Ret is
            when 0  => exit;
            when -1 => --  We didn't find it in this sector or error
               if (I = Sb.BPB_RootEntCnt/16) then -- this was the last sector!
                  Put_Error ("FAT: File '" & Tmp_Item (1 .. Tmp_Chars) &
                             "' not found");
                  return -1;
               end if;
            when others =>
               --  There aren't more used entries
               Put_Error ("FAT: File '" & Tmp_Item (1 .. Tmp_Chars) &
                          "' not found");
               return -1;
         end case;
         Offset := Offset + 1; --  Maybe in the following sector...
      end loop;
      --  b) Continue with the rest of directories in the PATH
      while (Tmp_Path (1) = '/') loop
         --  Get next Item in the Path
         Next_Item (Tmp_Path, Tmp_Item, Tmp_Chars);
         --  Read the directory  TODO: dirs may have more than 1 sector
         Ent.Size := FSDT.Buffer_Length'Last; -- Dirs doesn't have size
         Ent.Current_Offset := 0;
         loop
            if FAT_Read (Ent, To_Buffer_Ac (Buff (1)'Address), 32) /= 32 then
               Put_Error ("Error: could not read directory entry"); New_Line;
               return -1;
            end if;
            if Tmp_Path (1) = '/' then
               -- don't create directories
               Ret := Search_File (Tmp_Item, Tmp_Chars, Ent, False);
            else
               Ret := Search_File (Tmp_Item, Tmp_Chars, Ent, O_CREAT);
            end if;
            case Ret is
               when 0  => File_Found := True; exit; --  We found it!
               when -1 => null; --  We didn't find it in this sector
               when others =>
                  --  There aren't more used entries/cannot create entry
                  Put_Error ("FAT: File '" & Tmp_Item (1 .. Tmp_Chars) &
                             "' not found");
                  return -1;
            end case;
            --  Force FAT_Read to load the next sector of the directory
            Ent.Current_Offset := Ent.Current_Offset + 512;
         end loop;
      end loop;

      if Ent.Cluster /= 0 and O_CREAT then
         --  MaRTE.Direct_IO.Put ("  CREAR  ");
         --  When the field Cluster of the Ent is not 0 that means that we
         --  are using an Ent of an already existing file with the same name.
         --  The old file has to be reseted

         Operations.Reset_File (Ent.Entry_S_Offset, Ent.Entry_Pos);
         Ent.Cluster := 0;
         Ent.Size := 0;

      else
         --  New file: set its initial modification dates

         Directory_Entries.Update_Dir_Entry_Dates
           (Entry_Sector             => Ent.Entry_S_Offset,
            Entry_Index                =>
              Directory_Entries.Entry_Index_In_Sector (Ent.Entry_Pos),
            Update_Creation_Time     => True,
            Update_Modification_Time => True,
            Update_Access_Date       => True);
      end if;

      Ent.Current_Offset := 0; --  The RW offset for the file
      Ent.Last_File_Cluster := 0; --  We start in the file cluster 0
      Ent.Last_Real_Cluster := Ent.Cluster; --  The real value of cluster 0
                                            --  is in Ent.Cluster
      return 0;
   end FAT_Open;

   --------------
   -- FAT_Read --
   --------------

   function FAT_Read
      (Ent        : access FAT_Entry;
       Buffer_Ptr : in FSDT.Buffer_Ac;
       Bytes      : in FSDT.Buffer_Length)
       return BIT.Int
   is
      use type FSDT.Buffer_Length;
      Buffer_Left, Bytes_To_Read : FSDT.Buffer_Length;
      File_Cluster,
      Real_Cluster : BIT.Unsigned_16;
      Sector_In_Cluster : BIT.Unsigned_8;
      S_Offset : IDE.Sector_Offset;
      Pos_In_Sector, Bytes_Read : FSDT.Buffer_Length;
      Index : FSDT.Buffer_Length := 1;
   begin
      if Ent.Current_Offset >= Ent.Size then
         return EOF;
      end if;
      --  Determine number of Bytes to read
      Bytes_To_Read := Bytes;
      if (Ent.Current_Offset + Bytes_To_Read) > Ent.Size then
         Bytes_To_Read := Ent.Size - Ent.Current_Offset;
      end if;
      if Bytes_To_Read = 0 then
         return EOF;
      end if;
      Buffer_Left := Bytes_To_Read;
      --  Where do we start?
      File_Cluster := BIT.Unsigned_16
         (Ent.Current_Offset/(IDE.SECTOR_SIZE*FSDT.Buffer_Length
         (Sb.BPB_SecPerClus)));
      Real_Cluster := Ent.Last_Real_Cluster;
      --  Check cluster boundary condition
      if File_Cluster < Ent.Last_File_Cluster then
         Real_Cluster := Ent.Cluster;
         Ent.Last_File_Cluster := 0;
      end if;
      while Ent.Last_File_Cluster /= File_Cluster loop
         Real_Cluster := BIT.Unsigned_16
           (Table.Get_Cluster_Number (Table.FAT_Entry_Index (Real_Cluster)));
         if Real_Cluster not in 16#0002# .. 16#FFEF# then
            Put_Error ("ERROR: Not a valid cluster"); New_Line;
            return -1;
         end if;
         Ent.Last_File_Cluster := Ent.Last_File_Cluster + 1;
      end loop;
      Sector_In_Cluster := BIT.Unsigned_8 (
         (Ent.Current_Offset/IDE.SECTOR_SIZE) mod FSDT.Buffer_Length
         (Sb.BPB_SecPerClus));
      Pos_In_Sector := Ent.Current_Offset - IDE.SECTOR_SIZE*(
         FSDT.Buffer_Length (File_Cluster) *
         FSDT.Buffer_Length (Sb.BPB_SecPerClus) +
         FSDT.Buffer_Length (Sector_In_Cluster));
      --  Read Data
      loop
         S_Offset := Sb.Data_Area + IDE.Sector_Offset (Real_Cluster - 2) *
                     BIT.Unsigned_32 (Sb.BPB_SecPerClus);
         S_Offset := S_Offset + IDE.Sector_Offset (Sector_In_Cluster);

         Assert (Is_A_Cluster_Data_Sector (S_Offset), "Error: read");
         if Cache_Read_Sector (S_Offset) /= 0 then
            Put_Error ("Error: could not read sector"); New_Line;
            return BIT.Int (Bytes_To_Read - Buffer_Left);
         end if;
         if Pos_In_Sector + Buffer_Left > IDE.SECTOR_SIZE then
            Bytes_Read := IDE.SECTOR_SIZE - Pos_In_Sector;
         else
            Bytes_Read := Buffer_Left;
         end if;
         Buffer_Ptr (Index .. Index + Bytes_Read - 1) :=
            To_Buffer_Ac (Cache_Buffer (0)'Address)
                           (Pos_In_Sector + 1 .. Pos_In_Sector + Bytes_Read);
         --  Update temporal values
         Index := Index + Bytes_Read;
         Pos_In_Sector := 0;
         Buffer_Left := Buffer_Left - Bytes_Read;
      exit when (Buffer_Left <= 0);  --  Sorry, I had to exit here
         Sector_In_Cluster := (Sector_In_Cluster + 1) mod Sb.BPB_SecPerClus;
         if Sector_In_Cluster = 0 Then
         Real_Cluster := BIT.Unsigned_16
           (Table.Get_Cluster_Number (Table.FAT_Entry_Index (Real_Cluster)));
            Ent.Last_File_Cluster := Ent.Last_File_Cluster + 1;
         end if;
      end loop;

      --  Update access date
      Directory_Entries.Update_Dir_Entry_Dates
        (Entry_Sector             => Ent.Entry_S_Offset,
         Entry_Index                =>
           Directory_Entries.Entry_Index_In_Sector (Ent.Entry_Pos),
         Update_Creation_Time     => False,
         Update_Modification_Time => False,
         Update_Access_Date       => True);

      --  Update RW pointer
      Ent.Current_Offset := Ent.Current_Offset + Bytes_To_Read;
      Ent.Last_Real_Cluster := Real_Cluster;

      return BIT.Int (Bytes_To_Read);
   end FAT_Read;

   ---------------
   -- FAT_Write --
   ---------------

   function FAT_Write
      (Ent        : access FAT_Entry;
       Buffer_Ptr : in FSDT.Buffer_Ac;
       Bytes      : in FSDT.Buffer_Length)
       return BIT.Int
   is
      use type FSDT.Buffer_Length;
      Buffer_Left : FSDT.Buffer_Length;
      File_Cluster,
      Real_Cluster : aliased BIT.Unsigned_16;
      Prev_Cluster : BIT.Unsigned_16;
      Sector_In_Cluster : BIT.Unsigned_8;
      S_Offset : IDE.Sector_Offset;
      Pos_In_Sector, Bytes_Write : FSDT.Buffer_Length;
      Index : FSDT.Buffer_Length := 1;
   begin
      --  Bytes to write
      if Bytes = 0 then
         return EOF;
      end if;
      Buffer_Left := Bytes;
      --  Where do we start?
      File_Cluster := BIT.Unsigned_16
         (Ent.Current_Offset/(IDE.SECTOR_SIZE*FSDT.Buffer_Length
         (Sb.BPB_SecPerClus)));
      Real_Cluster := Ent.Last_Real_Cluster;
      --  Check cluster boundary condition
      if File_Cluster < Ent.Last_File_Cluster then
         Real_Cluster := Ent.Cluster;
         Ent.Last_File_Cluster := 0;
      end if;
      while Ent.Last_File_Cluster /= File_Cluster Or Real_Cluster = 16#00# loop
         Prev_Cluster := Real_Cluster;
         if Real_Cluster = 16#00# then
            --  first cluster in file (MAR)
            Real_Cluster := 16#FFF8#;
         else
            Real_Cluster := BIT.Unsigned_16
              (Table.Get_Cluster_Number (Table.FAT_Entry_Index (Real_Cluster)));
         end if;
         if Real_Cluster not in 16#0002# .. 16#FFEF# then
            if Real_Cluster in 16#FFF8# .. 16#FFFF# then
               Real_Cluster := Prev_Cluster;
               if Allocate_Cluster (Ent, Real_Cluster'Access) = -1 then
                  Put_Error ("ERROR: the disk is full"); New_Line;
                  return -1;
               end if;
            else
               Put_Error ("ERROR: Not a valid cluster"); New_Line;
               return -1;
            end if;
         end if;
         if Prev_Cluster /= 16#00# then
            Ent.Last_File_Cluster := Ent.Last_File_Cluster + 1;
         end if;
      end loop;
      Sector_In_Cluster := BIT.Unsigned_8 (
         (Ent.Current_Offset/IDE.SECTOR_SIZE) mod FSDT.Buffer_Length
         (Sb.BPB_SecPerClus));
      Pos_In_Sector := Ent.Current_Offset - IDE.SECTOR_SIZE*(
         FSDT.Buffer_Length (File_Cluster) *
         FSDT.Buffer_Length (Sb.BPB_SecPerClus) +
         FSDT.Buffer_Length (Sector_In_Cluster));
      --  Write Data
      loop
         S_Offset := Sb.Data_Area + IDE.Sector_Offset (Real_Cluster - 2) *
                     BIT.Unsigned_32 (Sb.BPB_SecPerClus);
         S_Offset := S_Offset + IDE.Sector_Offset (Sector_In_Cluster);

         Assert (Is_A_Cluster_Data_Sector (S_Offset), "Error: write");
         if Cache_Read_Sector (S_Offset) /= 0 then
            Put_Error ("Error: could not read sector"); New_Line;
            return BIT.Int (Bytes - Buffer_Left);
         end if;
         if Pos_In_Sector + Buffer_Left > IDE.SECTOR_SIZE then
            Bytes_Write := IDE.SECTOR_SIZE - Pos_In_Sector;
         else
            Bytes_Write := Buffer_Left;
         end if;
         --  Debug_Marte.Set_Break_Point_Here;
         To_Buffer_Ac (Cache_Buffer (0)'Address)
            (Pos_In_Sector + 1 .. Pos_In_Sector + Bytes_Write) :=
                              Buffer_Ptr (Index .. Index + Bytes_Write - 1);

         Assert (Is_A_Cluster_Data_Sector (Cache_Offset),
                 "Error: writing data");
         if Cache_Write_Sector /= 0 then
            Put_Error ("Error: could not write sector (write) Index:"&
                  FSDT.Buffer_Length'Image(Index)); New_Line;
            return BIT.Int (Bytes - Buffer_Left);
         end if;
         --  Update temporal values
         Index := Index + Bytes_Write;
         Pos_In_Sector := 0;
         Buffer_Left := Buffer_Left - Bytes_Write;
      exit when (Buffer_Left <= 0);  --  Sorry, I had to do it again :)
         --  Debug_Marte.Set_Break_Point_Here;
         Sector_In_Cluster := (Sector_In_Cluster + 1) mod Sb.BPB_SecPerClus;
         if Sector_In_Cluster = 0 then
            Prev_Cluster := Real_Cluster;
            Real_Cluster := BIT.Unsigned_16
              (Table.Get_Cluster_Number (Table.FAT_Entry_Index (Real_Cluster)));
            if Real_Cluster not in 16#0002# .. 16#FFEF# then
               if Real_Cluster in 16#FFF8# .. 16#FFFF# then
                  Real_Cluster := Prev_Cluster;
                  if Allocate_Cluster (Ent, Real_Cluster'Access) = -1 then
                     Put_Error ("ERROR: the disk is full"); New_Line;
                     return -1;
                  end if;
               else
                  Put_Error ("ERROR: Not a valid cluster"); New_Line;
                  return -1;
               end if;
            end if;
            Ent.Last_File_Cluster := Ent.Last_File_Cluster + 1;
         end if;
      end loop;

      --  Update size of file
      --  TODO: if error update size and return the written bytes
      if (Ent.Current_Offset + Bytes) > Ent.Size then

         Ent.Size := Ent.Current_Offset + Bytes;

         Directory_Entries.Update_Dir_Entry_File_Size
           (Entry_Sector             => Ent.Entry_S_Offset,
            Entry_Index              =>
              Directory_Entries.Entry_Index_In_Sector (Ent.Entry_Pos),
            Size_Of_File_In_Bytes    => BIT.Unsigned_32 (Ent.Size));

      end if;

      --  Update RW pointer
      Ent.Current_Offset := Ent.Current_Offset + Bytes;
      Ent.Last_Real_Cluster := Real_Cluster;

      --  Update modification and acess dates
      Directory_Entries.Update_Dir_Entry_Dates
        (Entry_Sector             => Ent.Entry_S_Offset,
         Entry_Index                =>
           Directory_Entries.Entry_Index_In_Sector (Ent.Entry_Pos),
         Update_Creation_Time     => False,
         Update_Modification_Time => True,
         Update_Access_Date       => True);

      --  Write the (possible) last changes to the FAT
      Table.Flush;

      return BIT.Int (Bytes);
   end FAT_Write;

   ---------------
   -- FAT_Close --
   ---------------

   function FAT_Close
     (Ent         : access FAT_Entry;
      Access_Mode : FSDT.File_Access_Mode)
      return BIT.Int is

      use type FSDT.File_Access_Mode;

      Update_Modification_Time : Boolean;

   begin
      Update_Modification_Time :=  (Access_Mode and FSDT.READ_WRITE) /= 0
        or else (Access_Mode and FSDT.WRITE_ONLY) /= 0;

      --  Write to the disk last changes to the file entry information (size
      -- of the file and modification/access dates)

      Root_Directory.Flush;

      return 0;
   end FAT_Close;

   ----------------
   -- FAT_Delete --
   ----------------
   --  This function deletes a file.
   --  Algorithm:
   --     a) Free all clusters in the file by writing a 16#00# value
   --     b) Delete the file entry by writing a 0xE5 value in the first byte

   function FAT_Delete
      (Ent   : in FAT_Entry)
       return BIT.Int
   is
      --------------------------------------------------------
      procedure Delete_FAT_Element
         (Element      : in out BIT.Unsigned_16)
      is
         S_Offset : IDE.Sector_Offset;
         Next_Element : BIT.Unsigned_16;
      begin
         S_Offset := Sb.FAT_Table + BIT.Unsigned_32 (Element) /
           (IDE.SECTOR_SIZE/2); -- a) FAT16 elements = 2bytes

         Assert (Is_A_FAT1_Sector (S_Offset),
                 "Error: reading to delete element");

         --  Before reading with Cache_Read_Sector we must be sure the sectors
         --  are updated
         Table.Flush_And_Invalidate;

         if Cache_Read_Sector (S_Offset) /= 0 then
            Put_Error ("Error: could not read sector"); New_Line;
         end if;
         Next_Element := To_Uint16_Ac (Cache_Buffer (
                           (Element mod (IDE.SECTOR_SIZE/2))*2)'Address).all;
         To_Uint16_Ac (Cache_Buffer (
               (Element mod (IDE.SECTOR_SIZE/2))*2)'Address).all := 16#0000#;

         Assert (Is_A_FAT1_Sector (Cache_Offset), "Error: deleting element");
         if Cache_Write_Sector /= 0 then
            Put_Error ("Error: could not write sector (delete-element)");
            New_Line;
         end if;
         Element := Next_Element;
      end Delete_FAT_Element;
      --------------------------------------------------------
      Cluster : BIT.Unsigned_16;
   begin
      --  a) Free all clusters
      Cluster := Ent.Cluster;
      while Cluster not in 16#FFF8# .. 16#FFFF# loop
         Delete_FAT_Element (Cluster);
      end loop;
      --  b) Delete the file entry by writing a 0xE5 value
      --  FALTA si el siguiente es 0x00 poner 0x00
      Assert (Is_A_Root_Dir_Sector (Ent.Entry_S_Offset),
              "Error: reading to delete");

      --  Before reading with Cache_Read_Sector we must be sure the sectors
      --  are updated
      Root_Directory.Flush_And_Invalidate;


      if Cache_Read_Sector (Ent.Entry_S_Offset) /= 0 then
         Put_Error ("Error: could not read sector"); New_Line;
         return -1;
      end if;
      Cache_Buffer (Ent.Entry_Pos*32) := 16#E5#;

      Assert (Is_A_Root_Dir_Sector (Cache_Offset),
              "Error: deleting file");
      if Cache_Write_Sector /= 0 then
         Put_Error ("Error: could not write sector (delete)"); New_Line;
         return -1;
      end if;
      return 0;
   end FAT_Delete;

   --------------
   -- FAT_Seek --
   --------------
   --  This function moves the RW pointer of a file.
   --  Algorithm:
   --     a) Simply change the Current_Offset

   function FAT_Seek
      (Ent    : access FAT_Entry;
       Offset : in FSDT.Off_t;
       Whence : in BIT.Int)
       return FSDT.Off_t
   is
      use type FSDT.Buffer_Length;
   begin
      case Whence is
         when FSDT.SEEK_SET =>
            Ent.Current_Offset := FSDT.Buffer_Length (Offset);
         when FSDT.SEEK_CUR =>
            Ent.Current_Offset := FSDT.Buffer_Length (
                                    FSDT.Off_t (Ent.Current_Offset) +
                                    Offset);
         when FSDT.SEEK_END =>
            Ent.Current_Offset := FSDT.Buffer_Length (
                                    FSDT.Off_t (Ent.Size) +
                                    Offset);
         when others =>
            return -1;
      end case;
      return FSDT.Off_t (Ent.Current_Offset);
   end FAT_Seek;

   ------------
   -- Assert --
   ------------

   procedure Assert (Cond : Boolean;
                     Msg : String := GNAT.Source_Info.Source_Location) is
   begin
      if not Cond then
         MaRTE.Direct_IO.Put_Error (Msg, Fatal => True);
         MaRTE.Direct_IO.New_Line;
      end if;
   end Assert;

   ----------------------
   -- Assert_Not_Fatal --
   ----------------------

   procedure Assert_Not_Fatal
     (Cond     : Boolean;
      Msg      : String;
      Location : String := GNAT.Source_Info.Source_Location) is
   begin
      if not Cond then
         MaRTE.Direct_IO.Put_Error (Msg & " at " & Location, Fatal => False);
         MaRTE.Direct_IO.New_Line;
      end if;
   end Assert_Not_Fatal;

end FAT;
