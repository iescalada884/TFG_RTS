--  Manages the FAT table.
--  Keeps updated the 2 copies of the FAT table.
--
--  Internally mantains a Cached sector of the FAT table from where the data
--  is read and written. The cached sector is the sector that contains the
--  last entry read (with Get_Cluster_Number) or written (with
--  Set_Cluster_Number).
--
--  The cached sector (in the case it has suffered changes respect to the
--  sector read from the disk), is written to the disk when an entry in another
--  sector is accessed (with [GS]et_Cluster_Number) or when the Flush procedure
--  is called.

with IDE.Partitions;
with MaRTE.Integer_Types;

package FAT.Table is
   package BIT renames MaRTE.Integer_Types;

   -------------------------------------
   -- Index of a FAT_Entry in the FAT --
   -------------------------------------

   --  In the range 2 .. Last_FAT_Entry_Index
   --  It starts at number 2 because the first cluster of the data area is
   --  cluster #2. That leaves the first two entries of the FAT unused.

   type FAT_Entry_Index is new Positive range 2 .. Positive'Last;

   function Last_FAT_Entry_Index return FAT_Entry_Index;

   -------------------------------
   --  Possible Cluster Numbers --
   -------------------------------

   Free_Entry                 : constant := 0;
   --  Entry available to be used

   Forbidden_Entry            : constant := 1;
   --  Not a valid value

   Used_Entry_First           : constant := 2;
   Used_Entry_Last            : constant := 16#FFEF#;
   --  Used entries are in the range Used_Entry_First .. Used_Entry_Last

   Reserved_Entry_First       : constant := 16#FFF0#;
   Reserved_Entry_Last        : constant := 16#FFF6#;
   --  Reserved values

   Bad_Entry                  : constant := 16#FFF7#;
   --  mark a bad cluster

   EOF_Entry_First            : constant := 16#FFF8#;
   EOF_Entry_Last             : constant := 16#FFFF#;
   --  EOF value used by MaRTE is EOF_Entry_First

   ----------------------------------
   -- File_Cluster_Chain_Number_OK --
   ----------------------------------

   --  Check if the cluster number is a valid value to be used in a file
   --  cluster chain (value in the range of used entries or EOF entries)

   function File_Cluster_Chain_Number_OK (Cluster_Num : Cluster_Number)
                                          return Boolean;

   --------------------
   -- Is_EOF_Cluster --
   --------------------

   function Is_EOF_Cluster (Cluster_Num : Cluster_Number)
                            return Boolean;

   --------------------
   -- Entry_Index_OK --
   --------------------

   --  Valid index of a FAT entry. Range 2 .. Last_FAT_Entry_Index

   function Entry_Index_OK (Entry_Index : FAT_Entry_Index)
                           return Boolean;

   ---------------------
   -- Is_Free_Cluster --
   ---------------------

   function Is_Free_Cluster (Cluster_Num : Cluster_Number)
                            return Boolean;

   ------------------------
   -- Get_Cluster_Number --
   ------------------------

   --  Get the cluster number stored in the index.

   function Get_Cluster_Number (Entry_Index : FAT_Entry_Index)
                                return Cluster_Number;

   ------------------------
   -- Set_Cluster_Number --
   ------------------------

   --  Change the cluster number assigned to an entry of the FAT table.
   --  IMPORTANT: Changes are actually written to the disk FAT table when
   --  [GS]et_Cluster_Number is called for an Entry_Index in a sector different
   --  from the one the entry/entries have been previously changed using
   --  this procedure. They are also written when the Flush procedure
   --  is called.

   procedure Set_Cluster_Number (Entry_Index : FAT_Entry_Index;
                                 Cluster_Num : Cluster_Number);

   -----------------------
   --  Find_Free_Entry  --
   -----------------------

   --  Looks for the first free entry in the FAT table.

   procedure Find_Free_Entry (Entry_Index : out FAT_Entry_Index;
                              FAT_Full : out Boolean);

   -----------
   -- Flush --
   -----------

   --  Write to the disk the pending changes in FAT table entries performed
   --  with Set_Cluster_Number

   procedure Flush;

   --------------------------
   -- Flush_And_Invalidate --
   --------------------------

   procedure Flush_And_Invalidate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

end FAT.Table;
