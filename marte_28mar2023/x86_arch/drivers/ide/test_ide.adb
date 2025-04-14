------------------------------------------------------------------------------
--                            TEST of IDE module                            --
------------------------------------------------------------------------------
--  Test for the ide and ide.partitions files. It also works as a model for --
--  understanding how to use them.                                          --
--  The best way to test the module is to use a PC emulator and check the   --
--  results with Linux tools.                                               --
--  i.e.:  dd if=mydisk.img bs=512 count=1 skip=63 | od -t x1               --
--  i.e.:  qemu -fda grubfloppy.img -hda mydisk.img -boot a                 --
--                                                                          --
--  To compile: mgnatmake test_ide.adb -Imarte_src_dirs                     --
--                                                                          --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  TODO: buffer with more than one sector
--  TODO: check Return values for posible errors
------------------------------------------------------------------------------
--  Basic types
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
--  Text input/output
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
--  For creating the buffer
with Ada.Unchecked_Conversion, System;
--  IDE interface
with IDE;
with IDE.Partitions;
--  for measurements
with MaRTE.HAL.IO; use MaRTE.HAL.IO;

procedure Test_IDE is

   procedure Pause is
      H : Character;
   begin
      Put("...");
      Get_Immediate(H);
      New_Line;
   end Pause;

   use type IDE.Sector_Count;
   function To_Buffer_Ac is new Ada.Unchecked_Conversion (System.Address, IDE.FSDT.Buffer_Ac);
   Drv : IDE.Drive := IDE.hda;
   Start : IDE.Sector_Offset := 0;
   Count : IDE.Sector_Count  := 3;
   Sect : IDE.Sector;
   Buffer : IDE.FSDT.Buffer_Ac := To_Buffer_Ac (Sect'Address);
   Part : aliased IDE.Partitions.Partition;
   Ret : Int;
begin
   Put_Line("Init the disk");
   Pause;

   Ret := IDE.Init (Drv);

   Put_Line ("Get the information of the 1st partition from the MBR");
   Pause;

   Ret := IDE.Partitions.Get_Partition_Info (Drv, 1, Part'Access);
   Put_Line ("State: "&IDE.Partitions.Partition_State'Image (Part.Stat));
   Put_Line ("Type: "&IDE.Partitions.Partition_Type'Image (Part.P_Type));
   Put_Line ("First sector: "&IDE.Sector_Offset'Image (Part.First_Sector));
   Put_Line ("Size: "&IDE.Sector_Offset'Image (Part.Size));

   Put_Line ("Go to that partition and read the first sectors");
   Pause;

   for I in IDE.Sector_Offset range 0 .. 10 loop
      Ret := IDE.RW_Sectors (Drv, IDE.Read, Part.First_Sector + I, 1, Buffer);
      for j in IDE.FSDT.Buffer_Length range 1 .. 512 loop
         Put (Integer (Buffer (j)), 5, 16); Put(" ");
      end loop;
      New_Line;
      New_Line;
      Pause;
   end loop;

   Put_Line ("Read and Write sectors");
   Pause;

   for I in IDE.Sector_Offset range 0 .. 10 loop
      Ret := IDE.RW_Sectors (Drv, IDE.Read, Part.First_Sector + I, 1, Buffer);
      for j in IDE.FSDT.Buffer_Length range 1 .. 10 loop
         Put (Integer (Buffer (j)), 5, 16); Put(" ");
      end loop;
      New_Line;
      New_Line;

      Ret := IDE.RW_Sectors (Drv, IDE.Write, Part.First_Sector + I, 1, Buffer);

      Ret := IDE.RW_Sectors (Drv, IDE.Read, Part.First_Sector + I, 1, Buffer);
      for j in IDE.FSDT.Buffer_Length range 1 .. 10 loop
         Put (Integer (Buffer (j)), 5, 16); Put(" ");
      end loop;
      New_Line;
      New_Line;
      Pause;
   end loop;

   Put_Line ("MEASUREMENTS for Read/Write operation (Parallel Port + osc)");
   Pause;

   for I in IDE.Sector_Offset range 0 .. 1000 loop
      Outb_P (16#378#, 16#FF#); --  Parallel Port
      Ret := IDE.RW_Sectors (Drv, IDE.Read, Part.First_Sector + I, 1, Buffer);
      Outb_P (16#378#, 16#00#); --  Parallel Port
      Ret := IDE.RW_Sectors (Drv, IDE.Write, Part.First_Sector + I, 1, Buffer);
   end loop;

   Put_Line ("Free the disk");
   Pause;

   Ret := IDE.Free (Drv);

end Test_IDE;
