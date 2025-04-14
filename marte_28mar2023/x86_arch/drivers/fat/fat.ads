------------------------------------------------------------------------------
--                                   FAT spec                               --
------------------------------------------------------------------------------
--  This module provides a means to read/write from a FAT 16 filesystem.    --
--  It is aimed at embedded applicatons and so it has some limitations:     --
--     1.-  No FAT12 nor FAT32 support.                                     --
--     2.-  Short filenames (XXXXXXXX.XXX).                                 --
--     3.-  For IDE disks (depends on IDE module).                          --
--  How to use it:  tests_fat.adb                                           --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
with MaRTE.Integer_Types;
with IDE.Partitions; use IDE.Partitions;
with MaRTE.Kernel.File_System_Data_Types;

with GNAT.Source_Info;

package FAT is

   package FSDT renames MaRTE.Kernel.File_System_Data_Types;
   package BIT renames MaRTE.Integer_Types;
   use type BIT.Int;

   ---------------------
   --  0)  Constants  --
   ---------------------
   EOF : constant BIT.Int := 0;
   ERROR : constant BIT.Int := -1;
   SEEK_SET : constant BIT.Int := 0;
   SEEK_CUR : constant BIT.Int := 1;
   SEEK_END : constant BIT.Int := 2;
   ----------------------
   --  1)  Data types  --
   ----------------------
   type FAT_Entry is private;

   ---------------------
   --  2)  Functions  --
   ---------------------
   function FAT_Create
      (Part : in     IDE.Partitions.Partition)
      return BIT.Int;

   function FAT_Open
      (Str   : in     FSDT.Path;
       Ent   : access FAT_Entry;
       O_CREAT : in Boolean)
       return BIT.Int;

   function FAT_Read
      (Ent        : access FAT_Entry; --  in out (RW Offset is modified)
       Buffer_Ptr : in FSDT.Buffer_Ac;
       Bytes      : in FSDT.Buffer_Length)
       return BIT.Int; -- returns 0 = EOF, -1 = ERROR, 1+ = bytes read

   function FAT_Write
      (Ent        : access FAT_Entry; --  in out (FileSize can be increased)
       Buffer_Ptr : in FSDT.Buffer_Ac;
       Bytes      : in FSDT.Buffer_Length)
       return BIT.Int; -- returns 0 = EOF, -1 = ERROR 1+ = bytes written

   function FAT_Close
     (Ent         : access FAT_Entry;
      Access_Mode : FSDT.File_Access_Mode)
      return BIT.Int;

   function FAT_Delete
      (Ent   : in FAT_Entry)
       return BIT.Int;

   function FAT_Seek
      (Ent    : access FAT_Entry;
       Offset : in FSDT.Off_t;
       Whence : in BIT.Int)
       return FSDT.Off_t;

   --------------------------------------------
   --  DEBUG) INTERNAL FUNCTIONS Data types  --
   --------------------------------------------
   --    function Find_Free_Cluster
   --       (Cluster      : access Unsigned_16)
   --        return Int;
   --
   --    procedure Get_Next_FAT_Element
   --       (Element      : in out Unsigned_16);

   --------------------
   -- Cluster_Number --
   --------------------

   type Cluster_Number is new Natural range 0 .. 2**16-1;
   for Cluster_Number'Size use 16;

private
   type FAT_Superblock is record
      Part : IDE.Partitions.Partition;
      BPB_BytsPerSec : BIT.Unsigned_16;
      BPB_SecPerClus : BIT.Unsigned_8;
      BPB_RsvdSecCnt : BIT.Unsigned_16;
      BPB_NumFATs    : BIT.Unsigned_8;
      BPB_RootEntCnt : BIT.Unsigned_16;
      BPB_TotSec16   : BIT.Unsigned_16;
      BPB_Media      : BIT.Unsigned_8;
      BPB_FATSz16    : BIT.Unsigned_16;
      BPB_SecPerTrk  : BIT.Unsigned_16;
      BPB_NumHeads   : BIT.Unsigned_16;
      BPB_HiddSec    : BIT.Unsigned_32;
      BPB_TotSec32   : BIT.Unsigned_32;
      FAT_Table      : IDE.Sector_Offset;
      Root_Dir       : IDE.Sector_Offset;
      Data_Area      : IDE.Sector_Offset;
   end record;

   ---------------
   -- FAT_Entry --
   ---------------

   --  Structure used by MaRTE and the driver to identify a file in the
   --  FAT file system (It is not written in the FAT file system).

   type FAT_Entry is record
      Cluster : BIT.Unsigned_16; --  where does the file start?
      Size : FSDT.Buffer_Length; --  The size of the file
      Entry_S_Offset : IDE.Sector_Offset; -- Root Dir sector the Entry is stored
      Entry_Pos : BIT.Unsigned_16; --  the position in that sector
      Current_Offset : FSDT.Buffer_Length; --  the current RW offset in a file
      Last_File_Cluster : BIT.Unsigned_16; --  Last_* variables are used for
      Last_Real_Cluster : BIT.Unsigned_16; --  caching the last cluster used.
   end record;

   --  FAT Superblock
   --  Get values in FAT_Create

   Sb : FAT.FAT_Superblock;

   --  (July-14) From here below added to be used in the new FAT.*
   --  child packages

   procedure Assert (Cond : Boolean;
                     Msg : String := GNAT.Source_Info.Source_Location);

   procedure Assert_Not_Fatal
     (Cond     : Boolean;
      Msg      : String;
      Location : String := GNAT.Source_Info.Source_Location);

end FAT;
