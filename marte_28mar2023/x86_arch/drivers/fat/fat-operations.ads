--  Complex operations with files that involves operations in several
--  FAT.* packages

with IDE;

with MaRTE.Integer_Types;

package FAT.Operations is
   package BIT renames MaRTE.Integer_Types;

   ----------------
   -- Reset_File --
   ----------------

   --  Resets the file which entry is in the position indicated by the
   --  Entry_Sector and the entry position in that sector (Entry_Pos).
   --  The reset of the file involves making free all its clusters in the FAT
   --  table, setting its size to 0 and updating its Creation, Last access and
   --  modification dates

   procedure Reset_File (Entry_Sector : IDE.Sector_Offset;
                         Entry_Pos    : BIT.Unsigned_16);

end FAT.Operations;
