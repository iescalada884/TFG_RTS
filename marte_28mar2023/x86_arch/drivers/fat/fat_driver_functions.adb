------------------------------------------------------------------------------
--                     FAT_Driver_Functions (body)                          --
------------------------------------------------------------------------------
--  This module provides a layer over FAT module in order to install it on  --
--  MaRTE OS, and be able to use a POSIX_IO interface. You have to install  --
--  it like any other driver and create the files (each file has the same   --
--  major number and a different minor number) in kernel-devices_table.ads  --
--  How to use it:  test_fat_driver.adb                                     --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  For unchecked conversions in Specific_Data
with Ada.Unchecked_Conversion;
with System;
--  for output messages
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
--  for basic types
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with MaRTE.Kernel.File_System_Data_Types;

package body Fat_Driver_Functions is

   package FSDT renames MaRTE.Kernel.File_System_Data_Types;
   use type DM.Int;

   ------------------------------------------------------------------
   -- Stuff for Specific_Data of each file (Entry and RW position) --
   ------------------------------------------------------------------

   type FAT_Specific_Data is record
      Ent : aliased FAT_Entry;
   end record;

   type FAT_Specific_Data_Ac is access all FAT_Specific_Data;
   function To_FAT_Specific_Data_Ac is new Ada.Unchecked_Conversion (Integer, FAT_Specific_Data_Ac);

   function FAT_Malloc (Size : in Size_T) return Integer;
   pragma Import (C, FAT_Malloc, "malloc");

   ------------
   -- Create --
   ------------
   Not_Initialized : boolean := true;

   function Create return DM.Int is
      Part : aliased IDE.Partitions.Partition;
   begin
      if Not_Initialized then
         --  Initialize the disk
         if IDE.Init (Disk) /= IDE.OK then
            return -1;
         end if;
         --  Get info of the partition we are going to be mount on
         if IDE.Partitions.Get_Partition_Info
                        (Disk, Part_Num, Part'Access) /= IDE.OK then
            Put ("Could not get partition info from Disk: "
                 &IDE.Drive'Image(Disk));
            New_Line;
            Put ("Hint: Change the variable Fat_Driver_Functions.Disk");
            New_Line;
            return -1;
         end if;

         if Part.P_Type = IDE.Partitions.Empty then
            Put ("Could not mount partition "&
                 IDE.Partitions.Partition_Number'Image (Part.Num)&
                 ", it is empty");
            New_Line;
            Put ("Hint: Configure Fat_Driver_Functions.Part_Num or");
            New_Line;
            Put ("format the partition with a FAT filesystem");
            New_Line;
            return -1;
         end if;

         if Part.P_Type /= IDE.Partitions.FAT16 and then
            Part.P_Type /= IDE.Partitions.FAT16_less32M and then
            Part.P_Type /= IDE.Partitions.W95_FAT16_LBA then
            Put ("Partition "&
                 IDE.Partitions.Partition_Number'Image (Part.Num)&
                 " type is not FAT16");
            New_Line;
            Put ("Hint: format part with FAT16 (0x06, 0x04, 0x0E) filesystem");
            New_Line;
            Put ("current type is "&
                 IDE.Partitions.Partition_Type'Image(Part.P_Type));
            New_Line;
            return -1;
         end if;

         --  Fill the filesystem info
         if FAT.Fat_Create (Part) /= 0 then
            Put ("Could not mount FAT partition correctly ");
            New_Line;
            Put ("Hint: format partition with a FAT16 (0x06) filesystem again");
            New_Line;
            return -1;
         end if;

         Not_Initialized := False;
         return 0;
      end if;
      return 0;
   end Create;

   ----------
   -- Open --
   ----------

   function Open (Fd   : in DM.File_Descriptor;
                  Mode : in DM.File_Access_Mode) return DM.Int
   is
      use type DM.File_Access_Mode;
      Ret : DM.Int;
      File_Name : DM.FSDT.Path;
      Invalid_Fd : Boolean;
      Data : Integer := FAT_Malloc (Size_T (FAT_Specific_Data'Size));
   begin
      if Not_Initialized then
         Put ("FAT driver was not correctly initialized"); New_Line;
         Put ("Hint: Check ERROR messages at initialization"); New_Line;
         return -1;
      end if;

      DM.Get_Path_Of_Fd (Fd, File_Name, Invalid_Fd);
      if Invalid_Fd then
         return -1;
      else
         Ret := FAT.FAT_Open (File_Name,
                              To_FAT_Specific_Data_Ac (Data).Ent'Access,
                           (Mode and DM.File_Access_Mode (DM.O_CREAT)) = DM.O_CREAT);
         DM.Set_Specific_Data (Fd, Data, Invalid_Fd);
         return Ret;
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   function Read (Fd         : in DM.File_Descriptor;
                  Buffer_Ptr : in DM.Buffer_Ac;
                  Bytes      : in DM.Buffer_Length) return DM.Ssize_T
   is
      use type DM.Buffer_Length;
      Ret : DM.Int;
      Invalid_Fd : Boolean;
      Data : Integer;
   begin
      if Not_Initialized then
         Put ("FAT driver was not correctly initialized"); New_Line;
         Put ("Hint: Check ERROR messages at initialization"); New_Line;
         return -1;
      end if;

      DM.Get_Specific_Data (Fd, Data, Invalid_Fd);
      if Invalid_Fd then
         return -1;
      else
         Ret := FAT.FAT_Read (To_FAT_Specific_Data_Ac (Data).Ent'access,
                             Buffer_Ptr,
                             Bytes);
         return DM.Ssize_T (Ret);
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   function Write (Fd         : in DM.File_Descriptor;
                   Buffer_Ptr : in DM.Buffer_Ac;
                   Bytes      : in DM.Buffer_Length) return DM.Ssize_T
   is
      use type DM.Buffer_Length;
      Ret : DM.Int;
      Invalid_Fd : Boolean;
      Data : Integer;
   begin
      if Not_Initialized then
         Put ("FAT driver was not correctly initialized"); New_Line;
         Put ("Hint: Check ERROR messages at initialization"); New_Line;
         return -1;
      end if;

      DM.Get_Specific_Data (Fd, Data, Invalid_Fd);
      if Invalid_Fd then
         return -1;
      else
         Ret := FAT.FAT_Write (To_FAT_Specific_Data_Ac (Data).Ent'Access,
                               Buffer_Ptr,
                               Bytes);
         return DM.Ssize_T (Ret);
      end if;
   end Write;

   -----------
   -- Close --
   -----------

   function Close (Fd : in DM.File_Descriptor) return DM.Int is
      Invalid_Fd : Boolean;
      Data : Integer;
      Access_Mode : FSDT.File_Access_Mode;
   begin
      if Not_Initialized then
         Put ("FAT driver was not correctly initialized"); New_Line;
         Put ("Hint: Check ERROR messages at initialization"); New_Line;
         return -1;
      end if;

      DM.Get_Specific_Data (Fd, Data, Invalid_Fd);
      if Invalid_Fd then
         return -1;
      end if;

      DM.Get_File_Access_Mode (Fd, Access_Mode, Invalid_Fd);
      if Invalid_Fd then
         return -1;
      end if;

      return FAT.FAT_Close (To_FAT_Specific_Data_Ac (Data).Ent'Access,
                            Access_Mode);
   end Close;

   ------------
   -- Delete --
   ------------

   function Delete (Fd : in DM.File_Descriptor) return DM.Int is
      Invalid_Fd : Boolean;
      Data : Integer;
   begin
      if Not_Initialized then
         Put ("FAT driver was not correctly initialized"); New_Line;
         Put ("Hint: Check ERROR messages at initialization"); New_Line;
         return -1;
      end if;

      DM.Get_Specific_Data (Fd, Data, Invalid_Fd);
      if Invalid_Fd then
         return -1;
      else
         return FAT.FAT_Delete (To_FAT_Specific_Data_Ac (Data).Ent);
      end if;
   end Delete;

   -----------
   -- Lseek --
   -----------

   function Lseek (Fd     : in DM.File_Descriptor;
                   Offset : in DM.Off_t;
                   Whence : in DM.Int) return FSDT.Off_t
   is
      Invalid_Fd : Boolean;
      Data : Integer;
   begin
      if Not_Initialized then
         Put ("FAT driver was not correctly initialized"); New_Line;
         Put ("Hint: Check ERROR messages at initialization"); New_Line;
         return -1;
      end if;

      DM.Get_Specific_Data (Fd, Data, Invalid_Fd);
      if Invalid_Fd then
         return -1;
      else
         return FAT.FAT_Seek (To_FAT_Specific_Data_Ac (Data).Ent'Access,
                              Offset,
                              Whence);
      end if;
   end Lseek;

end Fat_Driver_Functions;
