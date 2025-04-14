--  Manages directory entries in the root directory area and in the cluster
--  area (subdirectories).
--
--  Uses cached sector for reading but Set_* procedures writes data directly
--  to the disk

with MaRTE.Integer_Types;

with IDE;
with FAT.Table;

package FAT.Directory_Entries is

   package BIT renames MaRTE.Integer_Types;

   ---------------------
   -- Short_File_Name --
   ---------------------

   --  8.3 file name. The first 8 characters are the name and the last 3
   --  are the extension.
   --  The space (' ') is used to mark unused characters.

   subtype Short_File_Name is String (1 .. 11);

   -----------------------------------------
   -- First character name special values --
   -----------------------------------------

   Char_For_Deleted_File     : constant Character := Character'Val (16#E5#);
   Char_For_Unallocated_File : constant Character := Character'Val (16#00#);
   Char_For_Directory        : constant Character := Character'Val (16#2E#);

   -----------------------------------------
   --  File attribues of a Root_Dir_Entry --
   -----------------------------------------

   type File_Attributes is new BIT.Unsigned_8;
   for File_Attributes'Size use 8;

   READ_ONLY   : constant File_Attributes := 16#01#;
   HIDDEN      : constant File_Attributes := 16#02#;
   SYSTEM_ATRR : constant File_Attributes := 16#04#;
   VOLUME_ID   : constant File_Attributes := 16#08#;
   DIRECTORY   : constant File_Attributes := 16#10#;
   ARCHIVE     : constant File_Attributes := 16#20#;
   All_Attributes : constant File_Attributes :=
     READ_ONLY or HIDDEN or SYSTEM_ATRR or VOLUME_ID or DIRECTORY or ARCHIVE;

   WIN2000_Attributes : constant File_Attributes :=
     VOLUME_ID or READ_ONLY or SYSTEM_ATRR or HIDDEN;
   --  Used to mark long file name entries

   --------------------------------
   --  Dates in a Root_Dir_Entry --
   --------------------------------

   type Number_4_Bytes is mod 2**4;
   type Number_5_Bytes is mod 2**5;
   type Number_6_Bytes is mod 2**6;
   type Number_7_Bytes is mod 2**7;

   type Date is record
      Day     : Number_5_Bytes; -- day number (1-31)
      Month   : Number_4_Bytes; -- month number (1-12)
      Year    : Number_7_Bytes; -- offset from 1980 (0-119): years 1980 to 2099
   end record;

   for Date use record
      Day     at 0 range  0 ..  4;
      Month   at 0 range  5 ..  8;
      Year    at 0 range  9 .. 15;
   end record;
   for Date'Size use 2*8;

   type Time_And_Date is record
      Seconds : Number_5_Bytes; -- two-second periods (0-29): seconds 0 to 58
      Minutes : Number_6_Bytes; -- number of minutes (0-59)
      Hour    : Number_5_Bytes; -- number of hours (0-23)
      Day     : Number_5_Bytes; -- day number (1-31)
      Month   : Number_4_Bytes; -- month number (1-12)
      Year    : Number_7_Bytes; -- offset from 1980 (0-119): years 1980 to 2099
   end record;

   for Time_And_Date use record
      Seconds at 0 range  0 ..  4;
      Minutes at 0 range  5 .. 10;
      Hour    at 0 range 11 .. 15;
      Day     at 2 range  0 ..  4;
      Month   at 2 range  5 ..  8;
      Year    at 2 range  9 .. 15;
   end record;
   for Time_And_Date'Size use 4*8;

   ---------------------
   -- Directory_Entry --
   ---------------------

   Directory_Entry_Size_In_Bytes : constant := 32;

   type Directory_Entry is record
      File_Name_8_3 : Short_File_Name;
      -- 8.3 file name. The first 8 characters are the name and the last 3
      -- are the extension.

      Attributes : File_Attributes;
      --  Attributes of the file. The possible attributes are:
      --  READ_ONLY=0x01 HIDDEN=0x02 SYSTEM=0x04 VOLUME_ID=0x08
      --  DIRECTORY=0x10 ARCHIVE=0x20

      Reserved_By_Win_NT : BIT.Unsigned_8;

      Creation_Time_Not_Used : BIT.Unsigned_8;
      --  Creation time in tenths of a second.
      --  XXX: not clear what this means

      Creation_Date : Time_And_Date;
      --  The date on which the file was created.

      Last_Access_Date : Date;
      --  The date on which the file was last accessed

      First_Cluster_Number_High : BIT.Unsigned_16;
      --  The high 16 bits of this entry's first cluster number.
      --  For FAT 12 and FAT 16 this is always zero.

      Last_Modification_Date : Time_And_Date;
      --  The date on which the file was last modified

      First_Cluster_Number : Cluster_Number;
      --  The low 16 bits of this entry's first cluster number.
      --  Use this number to find the first cluster for this entry.

      Size_Of_File_In_Bytes : BIT.Unsigned_32;
      --  The size of the file in bytes.
   end record;
   pragma Pack (Directory_Entry);
   for Directory_Entry'Size use Directory_Entry_Size_In_Bytes*8;

   ------------------------------
   -- Directory_Entry_Looks_OK --
   ------------------------------

   function Directory_Entry_Looks_OK (The_Entry : Directory_Entry)
                                      return Boolean;

   --------------------------
   -- Entry_Is_A_Directory --
   --------------------------

   function Entry_Is_A_Directory (The_Entry : Directory_Entry)
                                  return Boolean;

   -------------------------
   -- Entry_Is_An_Archive --
   -------------------------

   function Entry_Is_An_Archive (The_Entry : Directory_Entry)
                                 return Boolean;

   --------------------------
   -- Is_A_Long_Name_Entry --
   --------------------------

   function Is_A_Long_Name_Entry (The_Entry : Directory_Entry)
                                  return Boolean;

   ------------------
   -- Update_Dates --
   ------------------

   procedure Update_Dates
     (The_Entry                : in out Directory_Entry;
      Update_Creation_Time     : Boolean;
      Update_Modification_Time : Boolean;
      Update_Access_Date       : Boolean);

   ------------------------------
   -- Directory_Entries_Sector --
   ------------------------------

   --  A sector in the Root_Dir secction (or a subdirectory sector in the
   --  cluster area corresponding to a subdirectory) is an array of
   --  Directory_Entry.

   Num_Of_Entries_Per_Sector : constant :=
     IDE.SECTOR_SIZE/Directory_Entry_Size_In_Bytes;

   type Entry_Index_In_Sector is new Natural range
     0 .. Num_Of_Entries_Per_Sector - 1;

   type Directory_Entries_Sector is array (Entry_Index_In_Sector)
     of Directory_Entry;
   pragma Pack (Directory_Entries_Sector);

   type Directory_Entries_Sector_Ac is access all Directory_Entries_Sector;

   -------------------
   -- Get_Dir_Entry --
   -------------------
   --  Gets the directory entry which is in the position indicated by the
   --  Entry_Index in the sector indicated by Entry_Sector.
   --  The Entry_Sector should be a sector in the root dir area or in a cluster
   --  in the cluster area that belongs to a subdirectory.

   function Get_Dir_Entry (Entry_Sector : IDE.Sector_Offset;
                           Entry_Index : Entry_Index_In_Sector)
                           return Directory_Entry;

   -------------------
   -- Set_Dir_Entry --
   -------------------

   --  Sets the directory entry which is in the position indicated by the
   --  Entry_Index in the sector indicated by Entry_Sector.
   --  The Entry_Sector should be a sector in the root dir area or in a cluster
   --  in the cluster area that belongs to a subdirectory.

   procedure Set_Dir_Entry (Entry_Sector : IDE.Sector_Offset;
                            Entry_Index : Entry_Index_In_Sector;
                            Dir_Entry : Directory_Entry);

   ----------------------------
   -- Update_Dir_Entry_Dates --
   ----------------------------

   procedure Update_Dir_Entry_Dates
     (Entry_Sector   : IDE.Sector_Offset;
      Entry_Index : Entry_Index_In_Sector;
      Update_Creation_Time : Boolean;
      Update_Modification_Time : Boolean;
      Update_Access_Date : Boolean);

   --------------------------------
   -- Update_Dir_Entry_File_Size --
   --------------------------------

   procedure Update_Dir_Entry_File_Size
     (Entry_Sector   : IDE.Sector_Offset;
      Entry_Index : Entry_Index_In_Sector;
      Size_Of_File_In_Bytes : BIT.Unsigned_32);

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


end FAT.Directory_Entries;
