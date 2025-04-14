
with IDE;
with MaRTE.Direct_IO;
with FAT.Directory_Entries;
with FAT.Root_Directory;
with FAT.Superblock;
with FAT.Table;

package body FAT.Check is

   use type Table.FAT_Entry_Index;

   -------------------
   -- Show_FAT_Info --
   -------------------

   procedure Show_FAT_Info is
      Info : FAT_Info;
   begin
      Info := Get_FAT_Info;

      MaRTE.Direct_IO.Put ("Sector_Size_In_Bytes:");
      MaRTE.Direct_IO.Put (Superblock.Sector_Size_In_Bytes'Img);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("Sectors_Per_Cluster:");
      MaRTE.Direct_IO.Put (Superblock.Sectors_Per_Cluster'Img);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("Superblock_Sector:");
      MaRTE.Direct_IO.Put (Superblock.Superblock_Sector'Img);
      MaRTE.Direct_IO.New_Line;

      MaRTE.Direct_IO.Put ("FAT1_Sector_0:");
      MaRTE.Direct_IO.Put (Info.FAT_Sector_0);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("FAT2_Sector_0:");
      MaRTE.Direct_IO.Put (Superblock.FAT2_Sector_0);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("Sectors_Per_FAT:");
      MaRTE.Direct_IO.Put (Info.Sectors_Per_FAT);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("FAT entries in range:");
      MaRTE.Direct_IO.Put ("[" & FAT.Table.FAT_Entry_Index'First'Img & ".." &
                             FAT.Table.Last_FAT_Entry_Index'Img & "]");
      MaRTE.Direct_IO.New_Line;

      MaRTE.Direct_IO.Put ("Root_Directory_Sector_0:");
      MaRTE.Direct_IO.Put (Info.Root_Directory_Sector_0);
      MaRTE.Direct_IO.New_Line;

      MaRTE.Direct_IO.Put ("Cluster_Space_Sector_0:");
      MaRTE.Direct_IO.Put (Info.Cluster_Space_Sector_0);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("Data_Area_Size_In_Sectors:");
      MaRTE.Direct_IO.Put (Superblock.Data_Area_Size_In_Sectors);
      MaRTE.Direct_IO.New_Line;


      MaRTE.Direct_IO.Put ("First_Sector_In_Partition:");
      MaRTE.Direct_IO.Put (Info.First_Sector_In_Partition);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("Sectors_In_FS:");
      MaRTE.Direct_IO.Put (Superblock.Sectors_In_FS);
      MaRTE.Direct_IO.New_Line;

      MaRTE.Direct_IO.Put
        ("FAT entries: Free Forbiden Used Reserved Bad   EOF   OtherEOF");
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("             ");
      MaRTE.Direct_IO.Put (Info.Num_Of_Free_Entries);
      MaRTE.Direct_IO.Put ("  +  ");
      MaRTE.Direct_IO.Put (Info.Num_Of_Forbidden_Entries);
      MaRTE.Direct_IO.Put ("  +  ");
      MaRTE.Direct_IO.Put (Info.Num_Of_Used_Entries);
      MaRTE.Direct_IO.Put ("  +  ");
      MaRTE.Direct_IO.Put (Info.Num_Of_Reserved_Entries);
      MaRTE.Direct_IO.Put ("  +  ");
      MaRTE.Direct_IO.Put (Info.Num_Of_Bad_Entries);
      MaRTE.Direct_IO.Put ("  +  ");
      MaRTE.Direct_IO.Put (Info.Num_Of_EOF_Entries);
      MaRTE.Direct_IO.Put ("  +  ");
      MaRTE.Direct_IO.Put (Info.Num_Of_Other_EOF_Entries);
      MaRTE.Direct_IO.New_Line;
      MaRTE.Direct_IO.Put ("Total:");
      MaRTE.Direct_IO.Put (Info.Num_Of_Free_Entries +
                             Info.Num_Of_Used_Entries +
                             Info.Num_Of_EOF_Entries +
                             Info.Num_Of_Bad_Entries +
                             Info.Num_Of_Reserved_Entries +
                             Info.Num_Of_Forbidden_Entries +
                             Info.Num_Of_Other_EOF_Entries);
      MaRTE.Direct_IO.New_Line;

   end Show_FAT_Info;

   ------------------
   -- Get_FAT_Info --
   ------------------

   function Get_FAT_Info return FAT_Info is
      use type IDE.Sector_Offset;
      Info : FAT_Info;
   begin
      Info := (First_Sector_In_Partition => Superblock.Superblock_Sector,
               Sectors_Per_FAT => Superblock.Sectors_Per_FAT,
               FAT_Sector_0 => Superblock.FAT1_Sector_0,
               Root_Directory_Sector_0 => Superblock.Root_Directory_Sector_0,
               Cluster_Space_Sector_0 => Superblock.Data_Area_Sector_0,
               Num_Of_Free_Entries => 0,
               Num_Of_Used_Entries => 0,
               Num_Of_EOF_Entries => 0,
               Num_Of_Bad_Entries => 0,
               Num_Of_Reserved_Entries => 0,
               Num_Of_Forbidden_Entries => 0,
               Num_Of_Other_EOF_Entries => 0);

      --  Traverses entries in FAT table

      for I in Table.FAT_Entry_Index'First .. Table.Last_FAT_Entry_Index loop

         case Table.Get_Cluster_Number (I) is
            when Table.Free_Entry =>
               Info.Num_Of_Free_Entries := Info.Num_Of_Free_Entries + 1;

            when Table.Forbidden_Entry =>
               Info.Num_Of_Forbidden_Entries :=
                 Info.Num_Of_Forbidden_Entries + 1;

            when Table.Used_Entry_First .. Table.Used_Entry_Last =>
               Info.Num_Of_Used_Entries := Info.Num_Of_Used_Entries + 1;

            when Table.Reserved_Entry_First .. Table.Reserved_Entry_Last =>
               Info.Num_Of_Reserved_Entries :=
                 Info.Num_Of_Reserved_Entries + 1;

            when Table.Bad_Entry =>
               Info.Num_Of_Bad_Entries := Info.Num_Of_Bad_Entries + 1;

            when Table.EOF_Entry_First =>
               Info.Num_Of_EOF_Entries := Info.Num_Of_EOF_Entries + 1;

            when Table.EOF_Entry_First + 1 .. Table.EOF_Entry_Last  =>
               Info.Num_Of_Other_EOF_Entries :=
                 Info.Num_Of_Other_EOF_Entries + 1;

         end case;
      end loop;

      return Info;
   end Get_FAT_Info;

   ------------------------
   -- Show_File_Clusters --
   ------------------------

   procedure Show_File_Clusters
     (File_Name : Directory_Entries.Short_File_Name) is
      File_Entry : Directory_Entries.Directory_Entry;
      Entry_Index : Root_Directory.Root_Dir_Entry_Index;
      Found : Boolean;
      Cluster_Num : Cluster_Number;
      Cluster_Count : Natural := 0;
   begin
      MaRTE.Direct_IO.Put (File_Name);
      MaRTE.Direct_IO.Put (":");

      -- Lock for the file

      Root_Directory.Find_Root_Dir_Entry (File_Name, File_Entry,
                                          Entry_Index, Found);

      if not Found then
         MaRTE.Direct_IO.Put (" not found");
         MaRTE.Direct_IO.New_Line;
         return;
      end if;

      --  Follow the clusters chain

      Cluster_Num := File_Entry.First_Cluster_Number;

      loop
         MaRTE.Direct_IO.Put (BIT.Unsigned_32 (Cluster_Num), Base => 16);

         --  The exit condition should be an EOF cluster, but we exit in
         --  any unexpected cluster value to deal with corrupted FATs

         exit when Table.Is_EOF_Cluster (Cluster_Num) or
           not Table.File_Cluster_Chain_Number_OK (Cluster_Num);

         Cluster_Count := Cluster_Count + 1;

         MaRTE.Direct_IO.Put ("->");
         Cluster_Num :=
           Table.Get_Cluster_Number (Table.FAT_Entry_Index (Cluster_Num));
      end loop;

      --  Summary of the end of the file

      if Table.Is_EOF_Cluster (Cluster_Num) then
         MaRTE.Direct_IO.Put ("(EOF)");

      else
         if Cluster_Count = 0 and Table.Is_Free_Cluster (Cluster_Num) then
            --  Empty files initial cluster has value 0

            MaRTE.Direct_IO.Put ("(Empty file)");

         else
            MaRTE.Direct_IO.Put ("(No EOF found: corrupted FS)");
         end if;
      end if;

      --  Cluster count

      MaRTE.Direct_IO.Put (" Cluster_Count:");
      MaRTE.Direct_IO.Put (Cluster_Count);

      --  valid range of sizes of the file

      if Cluster_Count > 0 then
         declare
            Last_Cluster_Begin : constant Natural :=
              (Cluster_Count - 1) *
              Natural (Superblock.Sector_Size_In_Bytes) *
              Natural (Superblock.Sectors_Per_Cluster);
            Last_Cluster_End : constant Natural :=
              Cluster_Count *
              Natural (Superblock.Sector_Size_In_Bytes) *
              Natural (Superblock.Sectors_Per_Cluster) - 1;
         begin
            MaRTE.Direct_IO.Put ("(");
            MaRTE.Direct_IO.Put (Last_Cluster_Begin);
            MaRTE.Direct_IO.Put ("..");
            MaRTE.Direct_IO.Put (Last_Cluster_End);
            MaRTE.Direct_IO.Put (")");
            if not (Natural (File_Entry.Size_Of_File_In_Bytes) in
                      Last_Cluster_Begin .. Last_Cluster_End) then
               MaRTE.Direct_IO.Put
                 ("ERROR: size and clusters count does not fit!!");
            end if;
         end;

      else --  Cluster_Count = 0
         if Natural (File_Entry.Size_Of_File_In_Bytes) /= 0 then
               MaRTE.Direct_IO.Put
                 ("ERROR: if cluster count is 0, size should be 0 aswell!!");
         end if;
      end if;


      MaRTE.Direct_IO.New_Line;
   end Show_File_Clusters;

end FAT.Check;
