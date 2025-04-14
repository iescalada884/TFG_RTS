------------------------------------------------------------------------------
--                       IDE.Partitions (spec)                              --
------------------------------------------------------------------------------
--  This module provides a means to get the partitions info from the MBR    --
--  of an IDE disk. It only supports primary partitions so far.             --
--  How to use it: test_ide.adb                                             --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
package IDE.Partitions is

   ----------------------
   --  0)  Data types  --
   ----------------------
   subtype Partition_State is BIT.Unsigned_8; -- see constants bellow

   subtype Partition_Type is BIT.Unsigned_8;  -- see constants bellow

   type Partition_Number is range 1 .. 4;

   type Partition is record
      Drv : IDE.Drive;
      Num : Partition_Number;
      Stat : Partition_State;
      P_Type : Partition_Type;
      First_Sector : Sector_Offset;
      Size : Sector_Offset;
   end record;

   ---------------------
   --  1)  Functions  --
   ---------------------
   function Get_Partition_Info
     (Drv  : in Drive;  --  Init the driver before
      Num  : in Partition_Number;
      Part : access Partition)  -- out parameter
      return BIT.Int;

   ---------------------
   --  2)  Constants  --
   ---------------------
   --  Bootable flag constants
   Active   : constant Partition_State := 16#00#;
   Inactive : constant Partition_State := 16#80#;
   --  Filesystem constants
   Empty : constant Partition_Type := 16#00#;
   FAT12 : constant Partition_Type := 16#01#;
   XENIX_root : constant Partition_Type := 16#02#;
   XENIX_usr : constant Partition_Type := 16#03#;
   FAT16_less32M : constant Partition_Type := 16#04#;
   Extended : constant Partition_Type := 16#05#;
   FAT16 : constant Partition_Type := 16#06#;
   HPFS_NTFS : constant Partition_Type := 16#07#;
   AIX : constant Partition_Type := 16#08#;
   AIX_bootable : constant Partition_Type := 16#09#;
   OS_2_Boot_Manag : constant Partition_Type := 16#0a#;
   W95_FAT32 : constant Partition_Type := 16#0b#;
   W95_FAT32_LBA : constant Partition_Type := 16#0c#;
   W95_FAT16_LBA : constant Partition_Type := 16#0e#;
   W95_Ext_d_LBA : constant Partition_Type := 16#0f#;
   OPUS : constant Partition_Type := 16#10#;
   Hidden_FAT12 : constant Partition_Type := 16#11#;
   Compaq_diagnost : constant Partition_Type := 16#12#;
   Hidden_FAT16_3 : constant Partition_Type := 16#14#;
   Hidden_FAT16 : constant Partition_Type := 16#16#;
   Hidden_HPFS_NTF : constant Partition_Type := 16#17#;
   AST_SmartSleep : constant Partition_Type := 16#18#;
   Hidden_W95_FAT3 : constant Partition_Type := 16#1b#;
   Hidden_W95_FAT3_2 : constant Partition_Type := 16#1c#;
   Hidden_W95_FAT1 : constant Partition_Type := 16#1e#;
   NEC_DOS : constant Partition_Type := 16#24#;
   Plan_9 : constant Partition_Type := 16#39#;
   PartitionMagic : constant Partition_Type := 16#3c#;
   Venix_80286 : constant Partition_Type := 16#40#;
   PPC_PReP_Boot : constant Partition_Type := 16#41#;
   SFS : constant Partition_Type := 16#42#;
   QNX4x : constant Partition_Type := 16#4d#;
   QNX4x_2nd_part : constant Partition_Type := 16#4e#;
   QNX4x_3rd_art : constant Partition_Type := 16#4f#;
   OnTrack_DM : constant Partition_Type := 16#50#;
   OnTrack_DM6_Aux : constant Partition_Type := 16#51#;
   CP_M : constant Partition_Type := 16#52#;
   OnTrack_DM6_Aux_2 : constant Partition_Type := 16#53#;
   OnTrackDM6 : constant Partition_Type := 16#54#;
   EZ_Drive : constant Partition_Type := 16#55#;
   Golden_Bow : constant Partition_Type := 16#56#;
   Priam_Edisk : constant Partition_Type := 16#5c#;
   SpeedStor : constant Partition_Type := 16#61#;
   GNU_HURD_or_Sys : constant Partition_Type := 16#63#;
   Novell_Netware : constant Partition_Type := 16#64#;
   Novell_Netware_2 : constant Partition_Type := 16#65#;
   DiskSecure_Mult : constant Partition_Type := 16#70#;
   PC_IX : constant Partition_Type := 16#75#;
   Old_Minix : constant Partition_Type := 16#80#;
   Minix_old_Lin : constant Partition_Type := 16#81#;
   Linux_swap : constant Partition_Type := 16#82#;
   Linux : constant Partition_Type := 16#83#;
   OS_2_hidden_C : constant Partition_Type := 16#84#;
   Linux_extended : constant Partition_Type := 16#85#;
   NTFS_volume_set : constant Partition_Type := 16#86#;
   NTFS_volume : constant Partition_Type := 16#87#;
   Linux_plaintext : constant Partition_Type := 16#88#;
   Linux_LVM : constant Partition_Type := 16#8e#;
   Amoeba : constant Partition_Type := 16#93#;
   Amoeba_BBT : constant Partition_Type := 16#94#;
   BSD_OS : constant Partition_Type := 16#9f#;
   IBM_Thinkpad_hi : constant Partition_Type := 16#a0#;
   FreeBSD : constant Partition_Type := 16#a5#;
   OpenBSD : constant Partition_Type := 16#a6#;
   NeXTSTEP : constant Partition_Type := 16#a7#;
   Darwin_UFS : constant Partition_Type := 16#a8#;
   NetBSD : constant Partition_Type := 16#a9#;
   Darwin_boot : constant Partition_Type := 16#ab#;
   BSDI_fs : constant Partition_Type := 16#b7#;
   BSDI_swap : constant Partition_Type := 16#b8#;
   Boot_Wizard_hid : constant Partition_Type := 16#bb#;
   Solaris_boot : constant Partition_Type := 16#be#;
   Solaris : constant Partition_Type := 16#bf#;
   DRDOS_sec : constant Partition_Type := 16#c1#;
   DRDOS_sec_2 : constant Partition_Type := 16#c4#;
   DRDOS_sec_3 : constant Partition_Type := 16#c6#;
   Syrinx : constant Partition_Type := 16#c7#;
   Non_FS_data : constant Partition_Type := 16#da#;
   CP_M_CTOS : constant Partition_Type := 16#db#;
   Dell_Utility : constant Partition_Type := 16#de#;
   BootIt : constant Partition_Type := 16#df#;
   DOS_access : constant Partition_Type := 16#e1#;
   DOS_R_O : constant Partition_Type := 16#e3#;
   SpeedStor_2 : constant Partition_Type := 16#e4#;
   BeOS_fs : constant Partition_Type := 16#eb#;
   EFI_GPT : constant Partition_Type := 16#ee#;
   EFI_FAT12_16 : constant Partition_Type := 16#ef#;
   Linux_PA_RISC_b : constant Partition_Type := 16#f0#;
   SpeedStor_3 : constant Partition_Type := 16#f1#;
   DOS_secondary : constant Partition_Type := 16#f2#;
   SpeedStor_4 : constant Partition_Type := 16#f4#;
   Linux_raid_auto : constant Partition_Type := 16#fd#;
   LANstep : constant Partition_Type := 16#fe#;
   BBT : constant Partition_Type := 16#ff#;

end IDE.Partitions;
