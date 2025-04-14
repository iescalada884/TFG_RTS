with Ada.Unchecked_Conversion;

with MaRTE.Integer_Types;
with MaRTE.Kernel.File_System_Data_Types;

package body FAT.Sector_Cache is

   package FSDT renames MaRTE.Kernel.File_System_Data_Types;
   package BIT  renames MaRTE.Integer_Types;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Cache  : in out Cached_Sector;
                         Sector : System.Address;
                         S_Offset_Min : IDE.Sector_Offset;
                         S_Offset_Max : IDE.Sector_Offset;
                         Mirrored : Boolean := False;
                         Mirror_Offset : IDE.Sector_Offset :=
                           IDE.Sector_Offset'Last) is
   begin
      FAT.Assert (not Cache.Initialized);

      Cache := (Initialized => True,
                Invalid => True,
                Sector => Sector,
                Offset => IDE.Sector_Offset'Last,
                Changes_Pending => False,
                S_Offset_Min => S_Offset_Min,
                S_Offset_Max => S_Offset_Max,
                Mirrored => Mirrored,
                Mirror_Offset => Mirror_Offset);
   end Initialize;

   -------------------------
   -- Set_Changes_Pending --
   -------------------------

   procedure Set_Changes_Pending (Cache  : in out Cached_Sector) is
   begin
      FAT.Assert (Cache.Initialized);
      FAT.Assert (not Cache.Invalid);

      if not Cache.Invalid then
         Cache.Changes_Pending := True;
      end if;
   end Set_Changes_Pending;

   ------------------
   -- To_Buffer_Ac --
   ------------------

   function To_Buffer_Ac is new
     Ada.Unchecked_Conversion (System.Address, FSDT.Buffer_Ac);

   -------------------------
   -- Write_When_Required --
   -------------------------

   procedure Write_When_Required (Cache : in out Cached_Sector) is
      use type IDE.Sector_Offset;
   begin
      FAT.Assert (Cache.Initialized);

      if Cache.Invalid then
         FAT.Assert (Cache.Offset = IDE.Sector_Offset'Last and
                       Cache.Changes_Pending = False);

         return; --  Nothing to do
      end if;

      if Cache.Changes_Pending then
         Write (Cache);
      end if;
   end Write_When_Required;

   ----------------------------------------
   -- Write_When_Required_And_Invalidate --
   ----------------------------------------

   procedure Write_When_Required_And_Invalidate
     (Cache : in out Cached_Sector) is
      use type IDE.Sector_Offset;
   begin
      FAT.Assert (Cache.Initialized);

      if Cache.Invalid then
         FAT.Assert (Cache.Offset = IDE.Sector_Offset'Last and
                       Cache.Changes_Pending = False);

         return; --  Nothing to do
      end if;

      if Cache.Changes_Pending then
         Write (Cache);
      end if;

      --  Invalidate the cache

      Cache.Invalid := True;
      Cache.Offset := IDE.Sector_Offset'Last;
      Cache.Changes_Pending := False;
   end Write_When_Required_And_Invalidate;

   -----------
   -- Write --
   -----------

   procedure Write (Cache : in out Cached_Sector) is
      use type BIT.Int, IDE.Sector_Offset;
      Error : BIT.Int;
   begin
      FAT.Assert (Cache.Initialized);
      FAT.Assert (not Cache.Invalid);

      if Cache.Invalid then
         FAT.Assert (Cache.Offset = IDE.Sector_Offset'Last and
                       Cache.Changes_Pending = False);
         return;
      end if;

      --  Write sector

      Error := IDE.RW_Sectors (FAT.Sb.Part.Drv, IDE.Write, Cache.Offset, 1,
                               To_Buffer_Ac (Cache.Sector));
      FAT.Assert (Error /= IDE.Error, "Error: Write_Cached_Sector");

      if Cache.Mirrored then
         --  Write mirrored sector

         Error := IDE.RW_Sectors (FAT.Sb.Part.Drv, IDE.Write,
                                  Cache.Offset + Cache.Mirror_Offset, 1,
                                  To_Buffer_Ac (Cache.Sector));
         FAT.Assert (Error /= IDE.Error, "Error: Write Mirrored Cached Sector");
      end if;

      Cache.Changes_Pending := False;
   end Write;

   ------------------------------------
   -- Write_When_Required_And_Update --
   ------------------------------------

   procedure Write_When_Required_And_Update
     (Cache : in out Cached_Sector;
      S_Offset : IDE.Sector_Offset;
      Force_Reading : Boolean := False) is

      use type BIT.Int, IDE.Sector_Offset;

      Error : BIT.Int;
   begin
      FAT.Assert (Cache.Initialized);

      FAT.Assert (S_Offset >= Cache.S_Offset_Min and
                    S_Offset <= Cache.S_Offset_Max);

      if Force_Reading or else S_Offset /= Cache.Offset or else Cache.Invalid
      then
         --  Write the cached sector in case it has been changed

         if not Cache.Invalid then
            Write_When_Required (Cache);
         else
            Cache.Invalid := False;
         end if;


         --  Update cached sector

         Cache.Offset := S_Offset;

         Error := IDE.RW_Sectors (FAT.Sb.Part.Drv, IDE.Read, Cache.Offset, 1,
                                  To_Buffer_Ac (Cache.Sector));
         FAT.Assert (Error /= IDE.Error);
      end if;
   end Write_When_Required_And_Update;

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Cache : Cached_Sector) return Boolean is
   begin
      FAT.Assert (Cache.Initialized);

      return Cache.Invalid;
   end Is_Invalid;

   -------------------
   -- Sector_Offset --
   -------------------

   function Sector_Offset (Cache : Cached_Sector) return IDE.Sector_Offset is
   begin
      FAT.Assert (Cache.Initialized);

      return Cache.Offset;
   end Sector_Offset;

end FAT.Sector_Cache;
