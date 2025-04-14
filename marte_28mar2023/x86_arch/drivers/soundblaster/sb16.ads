------------------------------------------------------------------------------
--                                   Sb16 spec                              --
------------------------------------------------------------------------------
--  This module is a driver for a Soundblaster 16 Card. It doesn't support  --
--  all of its features but just a few limited ones (by now):               --
--        1) Playback using Single-Cycle                                    --
--  The module has been tested with a PC emulator (QEMU) but not with a     --
--  device yet.                                                             --
--  How to use it: test_sb16.adb                                            --
--                                            daniel.sangorrin_at_gmail.com --
------------------------------------------------------------------------------
--  Sb16_Probe: look for a Sb16 card on the bus and display its values.     --
--  Useful to fill the constants above                                      --
--                                                                          --
--  Sb16_Init: init the Sb16 card and install an IRQ handler                --
--                                                                          --
--  Sb16_Program: program the Sb16 to playback/record. Due to the limita-   --
--  tions of the DMA controller the Buffer with the samples must be placed  --
--  bellow 16MB, its size can be up to 64KB (128KB for 16bits mode) and it  --
--  doesn't have to cross a 64KB physical page boundary.                    --
--                                                                          --
--  Sb16_Wait_for_IRQ: after programming the Sb16 it will start             --
--  playing/recording samples from/to the Buffer from 1 to Length. Then the --
--  Sb16 will raise an interrupt (IRQ) so you can fill the buffer and       --
--  program the card again.                                                 --
--                                                                          --
--  Sb16_Free: free the resources that were taken by the Card (IRQ ..)      --
--                                                                          --
--  In all cases returned values are '0' in case of success and '-1' in     --
--  case of fail.                                                           --
------------------------------------------------------------------------------
with Marte_Hardware_Interrupts;  --  for IRQ
with DMA; --  for DMA channels
with System; --  for System.Address
with MaRTE.HAL.IO; use MaRTE.HAL.IO; --  for IO_Port
with MaRTE.Integer_Types; use MaRTE.Integer_Types; --  for Unsigned_16

package Sb16 is

   package Mhi renames Marte_Hardware_Interrupts;

   ---------------------
   --  0)  Constants  --
   ---------------------
   SB16_BASE   : constant IO_Port := 16#220#; --  16#220# 16#240# 16#260# 16#280#
   SB16_IRQ    : constant Mhi.Hardware_Interrupt := 5; --  2, 3, 5, 7, 10
   SB16_DMAC   : constant DMA.DMA_Controller := DMA.DMAC1;
   SB16_DMA_Ch : constant DMA.DMA_Channel := DMA.Ch_1;

   -----------------
   --  1)  Types  --
   -----------------
   type Bits_Type is (Bits8, Bits16);
   for Bits_Type'Size use Int'Size;
   for Bits_Type use (Bits8 => 0, Bits16 => 1);

   type Tracks_Type is (Mono, Stereo);
   for Tracks_Type'Size use Int'Size;
   for Tracks_Type use (Mono => 0, Stereo => 1);

   ---------------------
   --  2)  Functions  --
   ---------------------
   function Sb16_Probe return Int;
   pragma Export (C, Sb16_Probe, "sb16_probe");

   function Sb16_Init return Int;
   pragma Export (C, Sb16_Init, "sb16_init");

   function Sb16_Program
      (Rate   : in Unsigned_16;    --  from 5000 to 44100 Hz
       Bits   : in Bits_Type;      --  8 or 16 bits
       Tracks : in Tracks_Type;    -- mono, stereo
       Buffer : in System.Address; -- < 16MBytes
       Length : in Unsigned_16) return Int;
   pragma Export (C, Sb16_Program, "sb16_program");

   function Sb16_Wait_for_IRQ return Int;
   pragma Export (C, Sb16_Wait_for_IRQ, "sb16_wait_for_irq");

   function Sb16_Free return Int;
   pragma Export (C, Sb16_Free, "sb16_free");

end Sb16;
