with Ada.Unchecked_Conversion;
with MaRTE.HAL.IO; use MaRTE.HAL.IO; --  Io_Port, Outb_P, Inb_P
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body DMA is

   function To_Uint32 is new Ada.Unchecked_Conversion (System.Address, Unsigned_32);

   ---------------
   -- REGISTERS --
   ---------------

   REG_Mode : constant array (DMA_Controller) of Io_Port := (DMAC1 => 16#0B#, DMAC2 => 16#D6#);
   REG_Mask : constant array (DMA_Controller) of Io_Port := (DMAC1 => 16#0A#, DMAC2 => 16#D4#);
   REG_Flip : constant array (DMA_Controller) of Io_Port := (DMAC1 => 16#0C#, DMAC2 => 16#D8#);
   REG_Page : constant array (DMA_Controller, DMA_Channel) of Io_Port :=
      (DMAC1 => (16#87#, 16#83#, 16#81#, 16#82#),
       DMAC2 => (16#8F#, 16#8B#, 16#89#, 16#8A#));
   REG_Addr : constant array (DMA_Controller, DMA_Channel) of Io_Port :=
      (DMAC1 => (16#00#, 16#02#, 16#04#, 16#06#),
       DMAC2 => (16#C0#, 16#C4#, 16#C8#, 16#CC#));
   REG_Count : constant array (DMA_Controller, DMA_Channel) of Io_Port :=
      (DMAC1 => (16#01#, 16#03#, 16#05#, 16#07#),
       DMAC2 => (16#C2#, 16#C6#, 16#CA#, 16#CE#));

   -----------------
   -- DMA_Disable --
   -----------------

   procedure DMA_Disable (DMAC    : in     DMA_Controller;
                          Channel : in     DMA_Channel) is
   begin
      Outb_P (REG_Mask (DMAC), 2#0000_1000# Or Unsigned_8 (DMA_Channel'Pos(Channel)));
   end DMA_Disable;

   -----------------
   -- DMA_Program --
   -----------------

   procedure DMA_Program (DMAC      : in     DMA_Controller;
                          Channel   : in     DMA_Channel;
                          Mode      : in     DMA_Mode;
                          Buffer_Ac : in     System.Address;
                          Length    : in     Unsigned_16)
   is
      Addr : Unsigned_16 := Unsigned_16 (To_Uint32 (Buffer_Ac) And 16#FFFF#);
      Page : Unsigned_8 := Unsigned_8 (Shift_Right (To_Uint32 (Buffer_Ac) And 16#FFFFF#, 16));
      DMA_Length : Unsigned_16 := Length - 1; --  (0 = 1byte etc...)
   begin
      --  Clear Flip-Flop
      Outb_P (REG_Flip (DMAC), 0);
      --  Set Mode
      Outb_P (REG_Mode (DMAC), Unsigned_8 (Mode) Or Unsigned_8 (DMA_Channel'Pos(Channel)));
      --  Page (bits 16-19 of Buffer_Ac)
      Outb_P (REG_Page (DMAC, Channel), Page);
      --  Offset address (bits 0 - 15 of Buffer_Ac) and Length
      if DMAC = DMAC1 then
         Outb_P (REG_Addr (DMAC, Channel), Unsigned_8 (Addr And 16#FF#)); -- LSB
         Outb_P (REG_Addr (DMAC, Channel), Unsigned_8 (Shift_Right (Addr, 8))); -- MSB
         Outb_P (REG_Count (DMAC, Channel), Unsigned_8 (DMA_Length And 16#FF#)); -- LSB
         Outb_P (REG_Count (DMAC, Channel), Unsigned_8 (Shift_Right (DMA_Length, 8))); -- MSB
      else
         Outw_P (REG_Addr (DMAC, Channel), Addr);
         Outw_P (REG_Count (DMAC, Channel), DMA_Length);
      end if;
   end DMA_Program;

   ----------------
   -- DMA_Enable --
   ----------------

   procedure DMA_Enable (DMAC    : in     DMA_Controller;
                         Channel : in     DMA_Channel) is
   begin
      Outb_P (REG_Mask (DMAC), 2#0000_0000# Or Unsigned_8 (DMA_Channel'Pos(Channel)));
   end DMA_Enable;

end DMA;