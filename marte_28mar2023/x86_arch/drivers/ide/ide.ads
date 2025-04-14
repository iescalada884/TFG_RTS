------------------------------------------------------------------------------
--                                   IDE spec                               --
------------------------------------------------------------------------------
--  This module provides a means to read/write sectors to an IDE hard disk. --
--  It doesn't provide concurrency facilities. If you want to use it        --
--  from different tasks you need to provide mutual exclusion access to the --
--  functions. It doesn't make use of DMA facilities, just IO instructions. --
--  How to use it: test_ide.adb                                             --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
with MaRTE.Integer_Types;
with MaRTE.Kernel.File_System_Data_Types;

package IDE is

   package FSDT renames MaRTE.Kernel.File_System_Data_Types;
   package BIT  renames MaRTE.Integer_Types;

   use type BIT.Int;
   use type BIT.Unsigned_16;

   ---------------------
   --  0)  Constants  --
   ---------------------
   SECTOR_SIZE : constant := 512;
   MAX_RETRIES : constant := 100_000;
   WARN_RETRY  : constant := 30; --  warning message if retries > WARN_RETRY

   RETRY_DELAY : constant := 0.000_441;
   DELAY_400NS : constant := 0.000_000_400; --  D400NS must be >= 400 ns

   ERROR       : constant BIT.Int := -1; --  return value
   OK          : constant BIT.Int := 0;  --  return value

   ----------------------
   --  1)  Data types  --
   ----------------------
   type Drive is (hda, hdb, hdc, hdd);
   type Read_Write is (Read, Write);
   type Sector is array
      (BIT.Unsigned_16 range 0 .. IDE.SECTOR_SIZE - 1) of BIT.Unsigned_8;
   subtype Sector_Offset is BIT.Unsigned_32;
   subtype Sector_Count is BIT.Unsigned_8;

   ---------------------
   --  2)  Functions  --
   ---------------------
   function Init  (Drv : in Drive) return BIT.Int;
   function Reset (Drv : in Drive) return BIT.Int;
   function Free  (Drv : in Drive) return BIT.Int;

   function RW_Sectors
     (Drv    : in     Drive;
      RW     : in     Read_Write;
      Start  : in     Sector_Offset; --  physical sector offset
      Count  : in     Sector_Count;  --  0=256 sectors
      Buffer : in     FSDT.Buffer_Ac)
      return BIT.Int;

end IDE;
