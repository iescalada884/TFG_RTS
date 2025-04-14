with MaRTE.Direct_IO;

with FAT.Directory_Entries;
with FAT.Table;

package body FAT.Operations is
   use type Directory_Entries.File_Attributes, BIT.Unsigned_16;

   ----------------
   -- Reset_File --
   ----------------

   procedure Reset_File (Entry_Sector : IDE.Sector_Offset;
                         Entry_Pos    : BIT.Unsigned_16) is
      Dir_Entry : Directory_Entries.Directory_Entry;
      Cluster_Num, Cluster_Num_Next : Cluster_Number;
   begin
      --  Get file entry

      Dir_Entry := Directory_Entries.Get_Dir_Entry
        (Entry_Sector, Directory_Entries.Entry_Index_In_Sector (Entry_Pos));

      --  Make sure its attributes are correct

      FAT.Assert (Directory_Entries.Entry_Is_An_Archive (Dir_Entry));

      --  Free its clusters

      Cluster_Num := Dir_Entry.First_Cluster_Number;
      loop
         Cluster_Num_Next :=
           Table.Get_Cluster_Number (Table.FAT_Entry_Index (Cluster_Num));

         Table.Set_Cluster_Number (Table.FAT_Entry_Index (Cluster_Num),
                                   Cluster_Num => Table.Free_Entry);

         --  The exit condition should be an EOF cluster, but we exit in
         --  any unexpected cluster value to deal with corrupted FATs

         exit when Table.Is_EOF_Cluster (Cluster_Num_Next) or
           not Table.File_Cluster_Chain_Number_OK (Cluster_Num_Next);

         Cluster_Num := Cluster_Num_Next;
      end loop;

      --  Inform of a corrupted file

      if not Table.Is_EOF_Cluster (Cluster_Num_Next) then
         MaRTE.Direct_IO.Put_Error ("(No EOF found: corrupted FS)");
      end if;

      --  Write to the disk the pending changes in the FAT table (changes in
      --  the last sector)

      Table.Flush;

      --  Update times

      Directory_Entries.Update_Dates (The_Entry                => Dir_Entry,
                                      Update_Creation_Time     => True,
                                      Update_Modification_Time => True,
                                      Update_Access_Date       => True);

      --  Reset size

      Dir_Entry.Size_Of_File_In_Bytes := 0;

      --  Mark that no clusters are used by the file.
      --  The convention used by operations in fat.adb is setting
      --  First_Cluster_Number to 0 to indicate this situation (maybe not the
      --  best option, but this is the way it is done)

      Dir_Entry.First_Cluster_Number := 0;

      --  Write the entry to the disk

      Directory_Entries.Set_Dir_Entry
        (Entry_Sector,
         Directory_Entries.Entry_Index_In_Sector (Entry_Pos),
         Dir_Entry);
   end Reset_File;

end FAT.Operations;
