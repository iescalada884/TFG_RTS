------------------------------------------------------------------------------
--                            IDE.Partitions (body)                         --
------------------------------------------------------------------------------
--  Read the first sector of the disk (MBR) and extract the partition table --
--  The partition table has 4 32-bit entrys nearly the end of the sector.   --
--  How to use it: test_ide.adb                                             --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System;
with MaRTE.Direct_IO; use MaRTE.Direct_IO;

package body IDE.Partitions is

   -------------------------
   -- Get_Partition_Info  --
   -------------------------

   function Get_Partition_Info
     (Drv  : in Drive;
      Num  : in Partition_Number;
      Part : access Partition)
      return BIT.Int
   is
      use type BIT.Unsigned_32;
      type Uint32_Ac is access all BIT.Unsigned_32;
      function To_Uint32_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                             Uint32_Ac);
      function To_Buffer_Ac is new Ada.Unchecked_Conversion (System.Address,
                                                             FSDT.Buffer_Ac);
      Buffer      : Sector;
      Base_Offset : constant BIT.Unsigned_16 :=
                        446 + (BIT.Unsigned_16 (Num) - 1)*16;
   begin
      if IDE.RW_Sectors (Drv, Read, 0, 1,
                         To_Buffer_Ac (Buffer (0)'Address)) /= IDE.OK then
         Put ("Error: could not read MBR"); New_Line;
         return IDE.ERROR;
      end if;
      Part.Drv := Drv;
      Part.Num := Num;
      Part.Stat := Buffer (Base_Offset + 0);
      Part.P_Type := Buffer (Base_Offset + 4);
      Part.First_Sector := To_Uint32_Ac (Buffer (Base_Offset + 8)'Address).all;
      Part.Size := To_Uint32_Ac (Buffer (Base_Offset + 12)'Address).all - 1;
      return IDE.OK;
   end Get_Partition_Info;

end IDE.Partitions;
