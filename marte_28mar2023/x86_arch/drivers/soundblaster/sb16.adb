------------------------------------------------------------------------------
--                                   Sb16 body                              --
------------------------------------------------------------------------------
--  This module is a driver for a Soundblaster 16 Card. It doesn't support  --
--  all of its features but just a few limited ones (by now):               --
--        1) Playback using Single-Cycle                                    --
--  The module has been tested with a PC emulator (QEMU) but not with a     --
--  device yet.                                                             --
--  How to use it: test_sb16.adb                                            --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  for timing funcions
with Ada.Real_Time; use Ada.Real_Time;
with MaRTE.Timespec; use MaRTE.Timespec;
with MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;
--  Text input/output
with MaRTE.Direct_IO; use MaRTE.Direct_IO;
with Interfaces.C;

package body Sb16 is

   use type Interfaces.C.Int;

   ----------------------
   -- Register Offsets --
   ----------------------

   MIX_ADDRESS    : constant := 16#4#;
   MIX_DATA       : constant := 16#5#;
   RESET          : constant := 16#6#;
   READ           : constant := 16#A#;
   WRITE          : constant := 16#C#;
   WRITE_STATUS   : constant := 16#C#; -- bit 7
   READ_STATUS    : constant := 16#E#; -- bit 7
   IRQ_ACK        : constant := 16#E#;
   IRQ_ACK_16BITS : constant := 16#F#;

   ------------------------
   -- Internal functions --
   ------------------------
   procedure delay_nanosleep (Seconds : in Duration) is
      Interval, Remaining : aliased Duration;
      Ret : Int;
      package TON renames MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;
   begin
      Interval := Seconds;
      Ret := TON.Nanosleep_Gnat (Interval'Unchecked_Access,
                Remaining'Unchecked_Access);
   end delay_nanosleep;
   ---------------------------------------------------------------------------
   function data_available (BASE : in IO_Port := SB16_BASE) return Int is
      Byte : Unsigned_8;
   begin
      for i in 1 .. 1000 loop
         Byte := Inb_P (BASE + READ_STATUS) And 2#1000_0000#;
         if (Byte = 2#1000_0000#) then
            return 0;
         end if;
      end loop;
      return -1;
   end data_available;
   ---------------------------------------------------------------------------
   procedure dspwait (BASE : in IO_Port := SB16_BASE) is
      Byte : Unsigned_8;
   begin
      for i in 1 .. 1000 loop
         Byte := Inb_P (BASE + WRITE_STATUS) And 2#1000_0000#;
         if (Byte = 2#0000_0000#) then
            return;
         end if;
      end loop;
      Put ("ERROR: retries exceeded in dspwait");
   end dspwait;

   -----------------
   -- IRQ Handler --
   -----------------

   function Sb16_IRQ_Handler (
         Area : in     System.Address;
         Intr : in     Mhi.Hardware_Interrupt)
     return Mhi.Handler_Return_Code is
     use type DMA.DMA_Controller;
     Byte : Unsigned_8;
   begin
      Put ("DEBUG: Sb16_IRQ_Handler called"); New_Line;
      if SB16_DMAC = DMA.DMAC1 then
         Byte := Inb_P (SB16_BASE + IRQ_ACK);
      else
         Byte := Inb_P (SB16_BASE + IRQ_ACK_16BITS);
      end if;
      --  FALTA si IRQ8-15 write 20h to A0h
      return Mhi.Posix_Intr_Handled_Notify;
   end Sb16_IRQ_Handler;

   Intr : aliased Mhi.Hardware_Interrupt;
   Handler : aliased Mhi.Interrupt_Handler_Function;

   ----------------
   -- Sb16_Probe --
   ----------------

   function Sb16_Probe return Int is
      BASE : IO_Port := 16#220#;
      Byte, Irq, Chan : Unsigned_8;
      Ret : Int := -1;
   begin
      Put ("---------------------------------------------------------");
      New_Line; Put (" PROBING..."); New_Line;
      Put ("---------------------------------------------------------");
      New_Line;
      for i in 1 .. 4  loop
         -- reset
         Outb_P (BASE + RESET, 1);
         delay_nanosleep (0.001);
         Outb_P (BASE + RESET, 0);
         -- dsp data available
         if (data_available (BASE) = 0) then
            -- dsp ready?
            for i in 1 .. 1000 loop
               Byte := Inb_P (BASE + READ);
               exit when (Byte = 16#AA#);
            end loop;
            if Byte = 16#AA# then
               --  Get the Version
               dspwait (BASE);
               Outb_P (BASE + WRITE, 16#E1#);
               if data_available (BASE) /= 0 then
                  exit;
               end if;
               --  GET the IRQ
               Outb_P (BASE + MIX_ADDRESS, 16#80#);
               Irq := Inb_P (BASE + MIX_DATA);
               --  GET the DMA Channel
               Outb_P (BASE + MIX_ADDRESS, 16#81#);
               Chan := Inb_P (BASE + MIX_DATA);
               --  DISPLAY BASE ADDRESS
               Put ("Sb16 found at "); Put (Integer (BASE), 16);
               Put ("   Ver: "); Put (Unsigned_8'Image (Inb_P (BASE + READ)));
               Put (".");
               Put (Unsigned_8'Image (Inb_P (BASE + READ))); New_Line;
               --  DISPLAY IRQ LEVEL
               Put ("The IRQ is (1=>IRQ2 2=>IRQ5 4=>IRQ7 8=>IRQ10):");
               Put (Unsigned_8'Image (Irq)); New_Line;
               --  DISPLAY DMA CHANNEL
               Put ("The DMA Channels (16#Chan16bit+Chan8bit#):   ");
               Put (Chan And 2#1110_1011#, 16); New_Line;
               Ret := 0;
            end if;
         end if;
         BASE := BASE + 20;
      end loop;
      Put ("---------------------------------------------------------");
      New_Line;New_Line;
      return Ret;
   end Sb16_Probe;

   ---------------
   -- Sb16_Init --
   ---------------

   function Sb16_Init return Int is
      Ret : Int := 0;
   begin
      -- reset
      Outb_P (SB16_BASE + RESET, 1);
      delay_nanosleep (0.001);
      Outb_P (SB16_BASE + RESET, 0);
      -- dsp data available
      if data_available = 0 then
         -- dsp ready?
         for i in 1 .. 1000 loop
            if Inb_P (SB16_BASE + READ) = 16#AA# then
               exit;
            end if;
         end loop;
      end if;
      --  IRQ
      if Mhi.Associate (SB16_IRQ, Sb16_IRQ_Handler'Unrestricted_Access,
         System.Null_Address, 0) /= 0 then
         Put ("Error: Could not associate to the IRQ"); New_Line;
         Ret := -1;
      end if;
      if Mhi.Unlock (SB16_IRQ) /= 0 then
         Put ("Error: Could not unlock the IRQ"); New_Line;
         Ret := -1;
      end if;
      return Ret;
   end Sb16_Init;

   ------------------
   -- Sb16_Program --
   ------------------

   function Sb16_Program
      (Rate : in Unsigned_16; --  from 5000 to 44100 Hz
       Bits : in Bits_Type;     --  8 or 16 bits
       Tracks : in Tracks_Type;   --  1 or 2 (mono, stereo)
       Buffer : in System.Address; -- < 16MBytes
       Length : in Unsigned_16)
       return Int
   is
      function LSB (Num : in Unsigned_16) return Unsigned_8 is
      begin
         return Unsigned_8 (Num And 16#FF#);
      end LSB;
      ------------------------------------------------
      function MSB (Num : in Unsigned_16) return Unsigned_8 is
      begin
         return Unsigned_8 (Shift_Right (Num, 8));
      end MSB;
      ------------------------------------------------
      use type DMA.DMA_Mode;
      Mode : DMA.DMA_Mode := DMA.Demand Or DMA.Addr_Increment Or
                             DMA.Single_Cycle Or DMA.Memory_To_IO;
   begin
      ------------------------------------------------
      --  DMA
      ------------------------------------------------
      DMA.DMA_Disable (SB16_DMAC, SB16_DMA_Ch);
      DMA.DMA_Program (SB16_DMAC, SB16_DMA_Ch, Mode, Buffer, Length);
      DMA.DMA_Enable (SB16_DMAC, SB16_DMA_Ch);
      ------------------------------------------------
      --  DSP
      ------------------------------------------------
      --  send frequency
      dspwait; Outb_P (SB16_BASE + WRITE, 16#41#);
      dspwait; Outb_P (SB16_BASE + WRITE, MSB (Rate));
      dspwait; Outb_P (SB16_BASE + WRITE, LSB (Rate));
      --  Set bits and tracks
      if Bits = Bits8 then
         case Tracks is
            when Mono =>
               dspwait; Outb_P (SB16_BASE + WRITE, 16#C0#);
               dspwait; Outb_P (SB16_BASE + WRITE, 16#00#);
            when Stereo =>
               dspwait; Outb_P (SB16_BASE + WRITE, 16#C0#);
               dspwait; Outb_P (SB16_BASE + WRITE, 16#20#);
         end case;
      else
         case Tracks is
            when Mono =>
               dspwait; Outb_P (SB16_BASE + WRITE, 16#B0#);
               dspwait; Outb_P (SB16_BASE + WRITE, 16#00#);
            when Stereo =>
               dspwait; Outb_P (SB16_BASE + WRITE, 16#B0#);
               dspwait; Outb_P (SB16_BASE + WRITE, 16#20#);
         end case;
      end if;
      --  Send length
      dspwait; Outb_P (SB16_BASE + WRITE, LSB (Length));
      dspwait; Outb_P (SB16_BASE + WRITE, MSB (Length));
      return 0;
   end Sb16_Program;

   -----------------------
   -- Sb16_Wait_for_IRQ --
   -----------------------

   function Sb16_Wait_for_IRQ return Int is
   begin
      return Mhi.Timedwait (0, null, Intr'Access, Handler'Access);
   end Sb16_Wait_for_IRQ;

   ---------------
   -- Sb16_Free --
   ---------------

   function Sb16_Free return Int is
   begin
      return 0;
   end Sb16_Free;

end Sb16;