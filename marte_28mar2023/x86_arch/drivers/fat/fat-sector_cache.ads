--  Cached disk sector
--  Operations to keep a sector cached and read and write it only when
--  necessary

with System;
with IDE;

package FAT.Sector_Cache is

   --------------------------
   --  Cached disk sector  --
   --------------------------

   type Cached_Sector is private;

   ----------------
   -- Initialize --
   ----------------

   --  Sector must be the address of a memory block of IDE.SECTOR_SIZE bytes
   --  Valid S_Offset values in Update_And_Write_When_Required will must be in
   --  the range S_Offset_Min .. S_Offset_Max

   procedure Initialize (Cache  : in out Cached_Sector;
                         Sector : System.Address;
                         S_Offset_Min : IDE.Sector_Offset;
                         S_Offset_Max : IDE.Sector_Offset;
                         Mirrored : Boolean := False;
                         Mirror_Offset : IDE.Sector_Offset :=
                           IDE.Sector_Offset'Last);

   -------------------------
   -- Set_Changes_Pending --
   -------------------------

   --  Cached sector will be written to the disk in the next call to
   --  Write_When_Required

   procedure Set_Changes_Pending (Cache  : in out Cached_Sector);

   -------------------------
   -- Write_When_Required --
   -------------------------

   --  Write the cached sector to the disk when there are changes pending

   procedure Write_When_Required (Cache : in out Cached_Sector);

   ----------------------------------------
   -- Write_When_Required_And_Invalidate --
   ----------------------------------------

   --  Write the cached sector to the disk when there are changes pending and
   --  invalidate the cache, that means its contents are not
   --  valid and will not be written to the disk even if a write operation
   --  is invoked. The sector will be read in the next call
   --  to Write_When_Required_And_Update.

   procedure Write_When_Required_And_Invalidate
     (Cache : in out Cached_Sector);

   -----------
   -- Write --
   -----------

   --  Write inconditionally the cached sector to the disk

   procedure Write (Cache : in out Cached_Sector);

   ------------------------------------
   -- Write_When_Required_And_Update --
   ------------------------------------

   --  Calls to Write_When_Required in order to write the former cached sector
   --  in case it has changes pending.
   --  Update the cached sector with the sector at S_Offset.
   --  If Force_Reading is true, the sector is always read from the disk, even
   --  if S_Offset is the current cached sector.

   procedure Write_When_Required_And_Update
     (Cache : in out Cached_Sector;
      S_Offset : IDE.Sector_Offset;
      Force_Reading : Boolean := False);

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Cache : Cached_Sector) return Boolean;

   -------------------
   -- Sector_Offset --
   -------------------

   function Sector_Offset (Cache : Cached_Sector) return IDE.Sector_Offset;

private

   type Cached_Sector is record
      Initialized : Boolean := False;

      Invalid : Boolean := False;
      --  The cache has been invalidated, that means its contents are not
      --  valid and will not be written to the disk even if a write operation
      --  is invoked. The sector will be read in the next call
      --  to Write_When_Required_And_Update

      Sector : System.Address := System.Null_Address;
      Offset : IDE.Sector_Offset := IDE.Sector_Offset'Last;
      Changes_Pending : Boolean := False;

      S_Offset_Min : IDE.Sector_Offset := IDE.Sector_Offset'Last;
      S_Offset_Max : IDE.Sector_Offset := IDE.Sector_Offset'First;
      --  Valid range will be S_Offset_Min .. S_Offset_Max
      --  Default values define a null range

      Mirrored : Boolean;
      Mirror_Offset : IDE.Sector_Offset;
      --  When Mirrored is true, the sector is mirrored.
      --  Than means every time the IDE sector "Offset" is written, it is
      --  also written its mirror sector at "Offset + Mirror_Offset"
      --  Sector data cached in "Sector" is always read from the main sector
      --  pointed by "Offset".
      --  Mirrored sectors are useful to keep sinchroniced both copies of the
      --  FAT table.
   end record;

end FAT.Sector_Cache;
