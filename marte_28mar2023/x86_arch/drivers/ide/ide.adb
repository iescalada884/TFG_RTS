------------------------------------------------------------------------------
--                             IDE body                                     --
------------------------------------------------------------------------------
--  The implementation is based on the ATA/ATAPI-4 specification            --
--  (http://www.t13.org) and it uses PIO (Programmed Input/Output) mode.    --
--                                                                          --
--  DMA or UDMA mode are not supported (although DMA release CPU usage      --
--  it affects the real-time properties of other tasks).                    --
--                                                                          --
--  The module has been tested on QEMU emulator and a CompactFlash (they    --
--  provide a True IDE interface) on a PC-104 x86 motherboard.              --
--                                                                          --
--  How to use it: see test_ide.adb or FAT filesystem driver                --
--                                                                          --
--                                   contact: daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  for System.Address
with System;
--  for Insw, Outsw
with MaRTE.HAL.IO; use MaRTE.HAL.IO;
pragma Elaborate_All (MaRTE.HAL.IO);
--  for the Interrupt handler:
with Marte_Hardware_Interrupts;
pragma Elaborate_All (Marte_Hardware_Interrupts);
--  for synchronizing IRQ_handler with the task
with Marte_Semaphores;
pragma Elaborate_All (Marte_Semaphores);
--  for timing funcions
--  with Ada.Real_Time; use Ada.Real_Time;
with MaRTE.Timespec; use MaRTE.Timespec;
with MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;
--  for output messages
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
--  for measurements
with MaRTE.HAL.IO; use MaRTE.HAL.IO;

package body IDE is

   package Mhi renames Marte_Hardware_Interrupts;
   package TON renames MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;

   use type BIT.Unsigned_8;
   use type BIT.Unsigned_32;

   ------------------------------------
   -- CONSTANTS AND GLOBAL VARIABLES --
   ------------------------------------

   BASE : constant array (Drive) of Io_Port := (16#1F0#, 16#1F0#,
                                                16#170#, 16#170#);

   --  OFFSETS
   IDE_DATA         : constant := 0; --  RW 16 bit - data blocks
   IDE_ERROR        : constant := 1; --  RO BBK/CRC.UNC.0.IDNF.0.ABRT.0.AMNF
   IDE_SECTOR_COUNT : constant := 2; --  RW 0=256 - return success rw
   IDE_SECTOR_START : constant := 3; --  RW LBA[7:0]
   IDE_LOW_CYL      : constant := 4; --  RW LBA[15:8]
   IDE_HIGH_CYL     : constant := 5; --  RW LBA[23:16]
   IDE_DRIVE_HEAD   : constant := 6; --  RW 1.LBA.1.DRIVE.LBA[27:24]
   IDE_STATUS       : constant := 7; --  RO BUSY.RDY.DWF.DSC.DRQ.CORR.0.ERR
   IDE_FEATURES     : constant := 1; --  WO
   IDE_COMMAND      : constant := 7; --  WO - See commands
   IDE_DEVICE_CTRL  : constant := 16#206#; --  WO 0.0.0.0.0.SOFTRESET.IRQ.0
   IDE_STATUS_AUX   : constant := 16#206#; --  RO IDE_STATUS but no clear IRQ
                                           --  16#1F0# + 16#206# => 16#3F6#

   --  ATA protocol commands
   IDE_CMD_READ_SECTOR    : constant := 16#20#;
   IDE_CMD_WRITE_SECTOR   : constant := 16#30#;
   IDE_CMD_READ_MULTIPLE  : constant := 16#C4#;
   IDE_CMD_WRITE_MULTIPLE : constant := 16#C5#;
   IDE_CMD_SET_MULTIPLE   : constant := 16#C6#;

   --  Important bits in the status register of an ATA controller.
   IDE_BITS_STATUS_BSY    : constant := 16#80#;
   IDE_BITS_STATUS_DRDY   : constant := 16#40#;
   IDE_BITS_STATUS_DRQ    : constant := 16#08#;
   IDE_BITS_STATUS_ERR    : constant := 16#01#;

   --  Important bits in the device control register.
   IDE_BITS_CTRL_nIEN     : constant := 16#02#; --  disable interrupts
   IDE_BITS_CTRL_RESET    : constant := 16#04#; --  soft RESET

   --  Interrupts
   subtype DEBUG_IRQ is Boolean range False .. False;

   IRQs : constant array (Drive) of Mhi.Hardware_Interrupt :=
     (hda => Mhi.FIXED_DISK_INTERRUPT, --  IRQ = 14
      hdb => Mhi.FIXED_DISK_INTERRUPT, --  IRQ = 14
      hdc => Mhi.RESERVED4_INTERRUPT,  --  IRQ = 15
      hdd => Mhi.RESERVED4_INTERRUPT); --  IRQ = 15

   --  Semaphore for IRQ handler - User communication
   Sem  : array (Drive) of aliased Marte_Semaphores.Semaphore;

   --  the currently selected drive (master-slave drives share IRQ)
   Selected_Drv : Drive;

   --  Time Measurements
   subtype ENABLE_MEASURES is Boolean range False .. False;

   ----------------
   -- delay400NS --
   ----------------

   procedure Spin_Wait (Interval : Duration) is
      use type MaRTE.HAL.HWTime;
      Time_End_Wait : constant MaRTE.HAL.HWTime :=
        MaRTE.HAL.Duration_To_HWTime (Interval) +
        MaRTE.HAL.Get_HWTime_Slow;
   begin
      loop
         exit when MaRTE.HAL.Get_HWTime_Slow > Time_End_Wait;
      end loop;
   end Spin_Wait;

   procedure delay400NS is
   begin
      Spin_Wait (DELAY_400NS);
   end Delay400NS;

   ------------------
   -- Poll_BSY_Bit --
   ------------------

   function Poll_BSY_Bit (Drv : in Drive;
                          BSY : in Boolean := False) return BIT.Int
   is
      Status : BIT.Unsigned_8;
      BSY_Bit : Boolean;
   begin
      for I in 1 .. MAX_RETRIES loop
         Status  := Inb_P (BASE (Drv) + IDE_STATUS_AUX);
         BSY_Bit := (Status and IDE_BITS_STATUS_BSY) /= 0;

         if BSY_Bit = BSY then
            if I > WARN_RETRY then
               Put_Error("Warning: Poll_BSY_Bit retries:"&
                         Integer'Image (I)); New_Line;
            end if;
            return OK;
         end if;

         Spin_Wait (RETRY_DELAY * (I mod 10 + 1));
         --  Waits a variable interval that could be 10 times longer than
         --  the minimum value (RETRY_DELAY)
      end loop;

      Put_Error("Error: Poll_BSY_Bit Status:"&
                BIT.Unsigned_8'Image (Status)&" BSY:"&Boolean'Image (BSY));
      New_Line;
      return ERROR;
   end Poll_BSY_Bit;

   ----------------------
   -- Poll_Status_Bits --
   ----------------------

   function Poll_Status_Bits (Drv : in Drive;
                              BSY : in Boolean := False;
                              DRQ : in Boolean := False) return BIT.Int
   is
      Status : BIT.Unsigned_8;
      BSY_Bit : Boolean;
      DRQ_Bit : Boolean;
   begin
      for I in 1 .. MAX_RETRIES loop
         Status  := Inb_P (BASE (Drv) + IDE_STATUS_AUX);
         BSY_Bit := (Status and IDE_BITS_STATUS_BSY) /= 0;
         DRQ_Bit := (Status and IDE_BITS_STATUS_DRQ) /= 0;

         if (BSY_Bit = BSY) and (DRQ_Bit = DRQ) then
            if I > WARN_RETRY then
               Put_Error("Warning: Poll_Status_Bits retries:"&
                         Integer'Image (I)); New_Line;
            end if;
            return OK;
         end if;

         Spin_Wait (RETRY_DELAY * (I mod 10 + 1));
         --  Waits a variable interval that could be 10 times longer than
         --  the minimum value (RETRY_DELAY)
      end loop;

      Put_Error("Error: Poll_Status_Bits Status:"&
                BIT.Unsigned_8'Image (Status)&
                " BSY:"&Boolean'Image (BSY)&" DRQ:"&Boolean'Image (DRQ));
      New_Line;
      return ERROR;
   end Poll_Status_Bits;

   -------------------------------
   -- Device_Selection_Protocol --
   -------------------------------

   function Device_Selection_Protocol (Drv : in Drive) return BIT.Int is
   begin
      if Poll_Status_Bits (Drv) /= OK then
         Put_Error("Error: Device_Selection_Protocol->Poll_Status_Bits 1");
         New_Line;
         return ERROR;
      end if;

      case Drv is
         when hda | hdc =>
            Outb_P (BASE (Drv) + IDE_DRIVE_HEAD, 2#1110_0000#);
         when hdb | hdd =>
            Outb_P (BASE (Drv) + IDE_DRIVE_HEAD, 2#1111_0000#);
      end case;

      delay400NS;

      if Poll_Status_Bits (Drv) /= OK then
         Put_Error("Error: Device_Selection_Protocol->Poll_Status_Bits 2");
         New_Line;
         return ERROR;
      end if;

      Selected_Drv := Drv;

      return OK;
   end Device_Selection_Protocol;

   ----------------------
   -- Check_For_Errors --
   ----------------------

   function Check_For_Errors (Drv : in Drive) return BIT.Int is
      Status : BIT.Unsigned_8;
   begin
      Status := Inb_P (BASE(Drv) + IDE_STATUS_AUX);
      if (Status and IDE_BITS_STATUS_ERR) = IDE_BITS_STATUS_ERR then
         Put_Error("Error: Check_For_Errors"); New_Line;
         return ERROR;
      end if;
      return OK;
   end Check_For_Errors;

   ---------------------
   -- IDE_IRQ_Handler --
   ---------------------

   function IDE_IRQ_Handler (
         Area : in System.Address;
         Intr : in Mhi.Hardware_Interrupt) return Mhi.Handler_Return_Code
   is
     use type Marte_Semaphores.Semaphore_Value;
     use type Mhi.Hardware_Interrupt;

     Value  : aliased Marte_Semaphores.Semaphore_Value;
     Ret    : BIT.Int;
     Status : BIT.Unsigned_8;
   begin
      if DEBUG_IRQ'First then
         Put ("Debug: Intr called"); New_Line;
      end if;

      if Intr /= IRQs (Selected_Drv) then
            Put_Error("Warning: IRQ "&Mhi.Hardware_Interrupt'Image (Intr)&
                      "not handled"); New_Line;
            return Mhi.POSIX_INTR_NOT_HANDLED;
      end if;

      --  Check for errors and clear IRQ by reading status register
      Status := Inb_P (BASE(Selected_Drv) + IDE_STATUS);
      if (Status and IDE_BITS_STATUS_ERR) = IDE_BITS_STATUS_ERR then
         Put_Error("Error: IRQ errors detected"); New_Line;
      end if;

      Ret := Marte_Semaphores.Getvalue (Sem (Selected_Drv)'access,
                                        Value'access);
      if Value = 0 then
         Ret := Marte_Semaphores.Post (Sem (Selected_Drv)'access);
      end if;

      return Mhi.POSIX_INTR_HANDLED_NOTIFY;
   end IDE_IRQ_Handler;

   ----------
   -- Init --
   ----------

   function Init (Drv  : in  Drive) return BIT.Int is
   begin
      if Marte_Semaphores.Initialize (Sem (Drv)'access, 0, 0) /= 0 then
         Put_Error("Error: Could not initialize the semaphore"); New_Line;
         return ERROR;
      end if;

      if Mhi.Associate (IRQs (Drv),
                        IDE_IRQ_Handler'access,
                        System.Null_Address, 0) /= 0 then
         Put_Error("Error: Could not associate to the IRQ"); New_Line;
         return ERROR;
      end if;

      if Mhi.Unlock (IRQs (Drv)) /= 0 then
         Put_Error("Error: Could not Lock the IRQ"); New_Line;
         return ERROR;
      end if;

      if Device_Selection_Protocol (Drv) /= OK then
         Put_Error("Error: Init->Device_Selection_Protocol"); New_Line;
         return ERROR;
      end if;

      --  Interrupts enabled
      Outb_P (BASE(Drv) + IDE_DEVICE_CTRL, 2#0000_0000#);
      delay400NS;

      return Poll_Status_Bits (Drv);
   end Init;

   ----------------
   -- RW_Sectors --
   ----------------
   --  TODO: make this protected

   function RW_Sectors
     (Drv    : in     Drive;
      RW     : in     Read_Write;
      Start  : in     Sector_Offset; --  physical offset
      Count  : in     Sector_Count;  --  0=256 sectors
      Buffer : in     FSDT.Buffer_Ac)
      return BIT.Int
   is
      Sec, Cyl_L, Cyl_H, Head : BIT.Unsigned_8;
      Pointer : System.Address;
      Words : BIT.Unsigned_32;
   begin
      --  Put (" [IDE" & RW'Img & " S:" & Start'Img & " C:" & Count'Img & "] ");

      if RW = Write then
         if ENABLE_MEASURES'First then
            Outb_P (16#378#, 16#FF#); --  Parallel Port
         end if;
      end if;

      if Count > 1 then
         --  Execute device selection protocol for set multiple command
         if Device_Selection_Protocol (Drv) /= OK then
            Put_Error("Error: Device_Selection_Protocol failed"); New_Line;
            return ERROR;
         end if;

         --  Use Set_Multiple command to enable Multiple sector transfer
         if Device_Selection_Protocol (Drv) /= OK then
            Put_Error("Error: Device_Selection_Protocol failed"); New_Line;
            return ERROR;
         end if;

         Outb_P (BASE(Drv) + IDE_SECTOR_COUNT, Count);
         Outb_P (BASE(Drv) + IDE_COMMAND, IDE_CMD_SET_MULTIPLE);
         delay400NS;

         --  Wait for the device to clear the BUSY flag
         if Poll_BSY_Bit (Drv) /= OK then
            Put_Error("Error: RW_Sectors->Poll_BSY_Bit"); New_Line;
            return ERROR;
         end if;

         --  Check the operation for errors
         if Check_For_Errors (Drv) /= OK then
            Put_Error("Error: RW_Sectors->Check_For_Errors"); New_Line;
            return ERROR;
         end if;
      end if;

      --  Execute device selection protocol for the RW command
      if Device_Selection_Protocol (Drv) /= OK then
         Put_Error("Error: Device_Selection_Protocol failed"); New_Line;
         return ERROR;
      end if;

      --  Set Pointer and number of Words to transmit
      Pointer := Buffer (1)'Address;
      if Count = 0 then
         Words := 256*SECTOR_SIZE/2; -- 0=256 sectors,  a_sector=256 words
      else
         Words := BIT.Unsigned_32 (Count)*SECTOR_SIZE/2;
      end if;

      --  Parse the LBA address
      Sec   := BIT.Unsigned_8 (Start and 16#FF#);
      Cyl_L := BIT.Unsigned_8 (BIT.Shift_Right (Start, 8)  and 16#FF#);
      Cyl_H := BIT.Unsigned_8 (BIT.Shift_Right (Start, 16) and 16#FF#);
      Head  := BIT.Unsigned_8 (BIT.Shift_Right (Start, 24) and 16#0F#);

      --  Set command parameters (num sectors, LBA[0:23], LBA[27:24], drive)
      Outb_P (BASE(Drv) + IDE_SECTOR_COUNT, Count);
      Outb_P (BASE(Drv) + IDE_SECTOR_START, Sec );
      Outb_P (BASE(Drv) + IDE_LOW_CYL, Cyl_L);
      Outb_P (BASE(Drv) + IDE_HIGH_CYL, Cyl_H);
      case Drv is
         when hda | hdc =>
            Outb_P (BASE(Drv) + IDE_DRIVE_HEAD, 2#1110_0000# or Head);
         when hdb | hdd =>
            Outb_P (BASE(Drv) + IDE_DRIVE_HEAD, 2#1111_0000# or Head);
      end case;

      --  Send the command
      if Count = 1 then
         if RW = Read then
            Outb_P (BASE(Drv) + IDE_COMMAND, IDE_CMD_READ_SECTOR);
         else --  Write
            Outb_P (BASE(Drv) + IDE_COMMAND, IDE_CMD_WRITE_SECTOR);
         end if;
      else
         if RW = Read then
            Outb_P (BASE(Drv) + IDE_COMMAND, IDE_CMD_READ_MULTIPLE);
         else --  Write
            Outb_P (BASE(Drv) + IDE_COMMAND, IDE_CMD_WRITE_MULTIPLE);
         end if;
      end if;

      --  The host shall wait at least 400 ns before reading the Status
      --  register. See PIO data in/out protocol in ATA/ATAPI-4 spec.
      delay400NS;

      --  If we are writing wait for DRQ to be set and write the data
      if RW = Write then
         --  Wait until DRQ is set
         if Poll_Status_Bits (Drv, BSY => False, DRQ => True) /= OK then
            Put_Error("Error: RW_Sectors->Poll_Status_Bits 1"); New_Line;
            return ERROR;
         end if;

         --  Write the data (TODO: do it in chunks)
         Outsw (BASE(Drv) + IDE_DATA, Pointer, Words);
      end if;

      if DEBUG_IRQ'First then
         Put ("Debug: before sem"); New_Line;
      end if;

      --  Wait for interrupt
      if Marte_Semaphores.Wait (Sem (Drv)'access) /= 0 then
         Put_Error("Error: waiting the semaphore"); New_Line;
         return ERROR;
      end if;

      if DEBUG_IRQ'First then
         Put ("Debug: after sem"); New_Line;
      end if;

      --  Check the operation for errors
      if Check_For_Errors (Drv) /= OK then
         Put_Error("Error: RW_Sectors->Check_For_Errors"); New_Line;
         return ERROR;
      end if;

      if RW = Read then
         --  Check that DRQ is set (it should be after IRQ)
         if Poll_Status_Bits (Drv, BSY => False, DRQ => True) /= OK then
            Put_Error("Error: RW_Sectors->Poll_Status_Bits 2"); New_Line;
            return ERROR;
         end if;

         --  Read the data into the buffer
         Insw (BASE(Drv) + IDE_DATA, Pointer, Words);
      end if;

      --  Check the operation for errors
      if Check_For_Errors (Drv) /= OK then
         Put_Error("Error: RW_Sectors->Check_For_Errors"); New_Line;
         return ERROR;
      end if;

      if RW = Write then
         if ENABLE_MEASURES'First then
            Outb_P (16#378#, 16#00#); --  Parallel Port
         end if;
      end if;

      return OK;
   end RW_Sectors;

   -----------
   -- Reset --
   -----------

   function Reset (Drv : in Drive) return BIT.Int is
   begin
      --  Select the Drive
      if Device_Selection_Protocol (Drv) /= OK then
         Put_Error("Error: Reset->Device_Selection_Protocol"); New_Line;
         return ERROR;
      end if;

      --  Soft Reset + Interrupts enabled
      Outb_P (BASE (Drv) + IDE_DEVICE_CTRL, 2#0000_0100#); delay400NS;
      Outb_P (BASE (Drv) + IDE_DEVICE_CTRL, 2#0000_0000#); delay400NS;

      return Poll_Status_Bits (Drv);
   end Reset;

   ----------
   -- Free --
   ----------

   function Free (Drv  : in Drive) return BIT.Int is
   begin
      --  Select the Drive
      case Drv is
         when hda | hdc =>
            Outb_P (BASE (Drv) + IDE_DRIVE_HEAD, 2#1110_0000#);
         when hdb | hdd =>
            Outb_P (BASE (Drv) + IDE_DRIVE_HEAD, 2#1111_0000#);
      end case;
      --  Disable Interrupts
      Outb_P (BASE(Drv) + IDE_DEVICE_CTRL, 2#0000_0010#);
      delay400NS;
      if Mhi.Lock (IRQs (Drv)) /= 0 then
         Put_Error("Error: Could not Lock the IRQ"); New_Line;
         return ERROR;
      end if;
      if Mhi.Disassociate (IRQs (Drv), IDE_IRQ_Handler'access) /= 0 then
         Put_Error("Error: Could not disassociate to the IRQ"); New_Line;
         return ERROR;
      end if;
      if Marte_Semaphores.Destroy (Sem (Drv)'access) /= 0 then
         Put_Error("Error: Could not destroy the semaphore"); New_Line;
         return ERROR;
      end if;
      return OK;
   end Free;

end IDE;
