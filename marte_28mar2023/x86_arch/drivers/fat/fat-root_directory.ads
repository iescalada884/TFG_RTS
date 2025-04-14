--  Manages the root directory area

with FAT.Directory_Entries;

package FAT.Root_Directory is

   -----------------------------------------------------------
   -- Index of a Directory_Entry in the Root directory area --
   -----------------------------------------------------------

   --  In the range 0 .. Last_Root_Dir_Entry_Index

   type Root_Dir_Entry_Index is new BIT.Unsigned_32;

   function Last_Root_Dir_Entry_Index return Root_Dir_Entry_Index;

   -------------------------
   -- Find_Root_Dir_Entry --
   -------------------------

   --  Find a file or directory with the provided name in the root dir area

   procedure Find_Root_Dir_Entry
     (File_Name : Directory_Entries.Short_File_Name;
      Dir_Entry : out Directory_Entries.Directory_Entry;
      Entry_Index : out Root_Dir_Entry_Index;
      Found : out Boolean);

   ------------------------
   -- Get_Root_Dir_Entry --
   ------------------------

   function Get_Root_Dir_Entry (Entry_Index : Root_Dir_Entry_Index)
                                return Directory_Entries.Directory_Entry;

   ------------------------
   -- Set_Root_Dir_Entry --
   ------------------------

   --  The data is inmediatelly written to the disk.

   procedure Set_Root_Dir_Entry
     (Entry_Index : Root_Dir_Entry_Index;
      Dir_Entry : Directory_Entries.Directory_Entry);

   ------------------------
   -- Get_Root_Dir_Entry --
   ------------------------

   --  Gets the directory entry which is in the position indicated by the
   --  Entry_Index in the sector indicated by Entry_Sector.
   --  The Entry_Sector should be a sector in the root dir area.

   function Get_Root_Dir_Entry
     (Entry_Sector : IDE.Sector_Offset;
      Entry_Index : Directory_Entries.Entry_Index_In_Sector)
      return Directory_Entries.Directory_Entry;

   ------------------------
   -- Set_Root_Dir_Entry --
   ------------------------

   --  Gets the directory entry which is in the position indicated by the
   --  Entry_Index in the sector indicated by Entry_Sector.
   --  The Entry_Sector should be a sector in the root dir area.

   procedure Set_Root_Dir_Entry
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


end FAT.Root_Directory;
