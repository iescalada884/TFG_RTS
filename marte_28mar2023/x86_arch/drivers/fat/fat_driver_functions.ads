------------------------------------------------------------------------------
--                     FAT_Driver_Functions (spec)                          --
------------------------------------------------------------------------------
--  This module provides a layer over FAT module in order to install it on  --
--  MaRTE OS, and be able to use a POSIX_IO interface. You have to install  --
--  it like any other driver and create the files (each file has the same   --
--  major number and a different minor number) in kernel-devices_table.ads  --
--  How to use it:  test_fat_driver.adb                                     --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  TODO:  Use a global variable to be able to use more than one partition
------------------------------------------------------------------------------
--  for Filesystem data types and Get_Path_Of_Fd
with Drivers_MaRTE;
--  FAT interface
with FAT; use FAT;
--  IDE interface
with IDE, IDE.Partitions;

package Fat_Driver_Functions is

   package DM renames Drivers_MaRTE;

   ---------------------
   --  0)  Constants  --
   ---------------------
   Disk : constant IDE.Drive := IDE.Hda;
   Part_Num : constant IDE.Partitions.Partition_Number := 1;

   ---------------------
   --  1)  Functions  --
   ---------------------
   function Create return DM.Int;

   function Open (Fd   : in DM.File_Descriptor;
                  Mode : in DM.File_Access_Mode) return DM.Int;

   function Read (Fd         : in DM.File_Descriptor;
                  Buffer_Ptr : in DM.Buffer_Ac;
                  Bytes      : in DM.Buffer_Length) return DM.Ssize_T;

   function Write (Fd         : in DM.File_Descriptor;
                   Buffer_Ptr : in DM.Buffer_Ac;
                   Bytes      : in DM.Buffer_Length) return DM.Ssize_T;

   function Close (Fd : in DM.File_Descriptor) return DM.Int;

   function Delete (Fd : in DM.File_Descriptor) return DM.Int;

   function Lseek (Fd     : in DM.File_Descriptor;
                   Offset : in DM.Off_t;
                   Whence : in DM.Int) return FSDT.Off_t;

end Fat_Driver_Functions;
