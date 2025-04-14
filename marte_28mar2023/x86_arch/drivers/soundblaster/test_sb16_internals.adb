with MaRTE_OS;
--  Basic types
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
--  Text input/output
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
--  for System.Address
with System;
with System.Storage_Elements;
with Ada.Unchecked_Conversion;
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
with Ada.Real_Time; use Ada.Real_Time;
with MaRTE.Timespec; use MaRTE.Timespec;
with MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;

with POSIX_IO;  use POSIX_IO;
with Ada.Streams;

with DMA; use DMA;
with Debug_Marte; use Debug_Marte;
procedure test_sb16_internals is

   use type Ada.Streams.Stream_Element_Offset;

   procedure See_Memory (ptr : in System.Address);
   pragma Import (C, See_Memory, "see_memory_c");

   BASE : IO_Port := 16#220#; --  probar en 0x210 to 0x260
   MIX_ADDRESS    : constant := 16#4#;
   MIX_DATA       : constant := 16#5#;
   RESET          : constant := 16#6#;
   READ           : constant := 16#A#;
   WRITE          : constant := 16#C#;
   WRITE_STATUS   : constant := 16#C#; -- bit 7
   READ_STATUS    : constant := 16#E#; -- bit 7
   IRQ_ACK        : constant := 16#E#;
   IRQ_ACK_16BITS : constant := 16#F#;

   procedure delay_nanosleep (Seconds : in Duration) is
      Interval, Remaining : aliased Duration;
      Ret : Int;
      package TON renames MaRTE.Kernel.Tasks_Operations.Nanosleep_Gnat;
   begin
      Interval := Seconds;
      Ret := TON.Nanosleep_Gnat (Interval'Unchecked_Access, Remaining'Unchecked_Access);
   end delay_nanosleep;

   procedure data_available is
      Byte : Unsigned_8;
   begin
      loop
         Byte := Inb_P (BASE + READ_STATUS) And 2#1000_0000#;
         exit when (Byte = 2#1000_0000#);
      end loop;
   end data_available;

   procedure dspwait is
      Byte : Unsigned_8;
   begin
      loop
         Byte := Inb_P (BASE + WRITE_STATUS) And 2#1000_0000#;
         exit when (Byte = 2#0000_0000#);
      end loop;
   end dspwait;

   package Mhi renames Marte_Hardware_Interrupts;

   function Sb16_IRQ_Handler (
         Area : in     System.Address;
         Intr : in     Mhi.Hardware_Interrupt)
     return Mhi.Handler_Return_Code is
     Byte : Unsigned_8;
   begin
      Put ("Debug: Intr called"); New_Line;
      Byte := Inb_P (BASE + IRQ_ACK);
      return Mhi.Posix_Intr_Handled_Notify;
   end Sb16_IRQ_Handler;

   Intr : aliased Mhi.Hardware_Interrupt;
   Handler : aliased Mhi.Interrupt_Handler_Function;

   Byte : Unsigned_8;
   Ptr : System.Address := System.Storage_Elements.To_Address (16#10000#);
   Buffer : Ada.Streams.Stream_Element_Array (1 .. 50000);
   for Buffer'Address use Ptr;
      --  Put (Integer (Buffer (1)), 4, 16);   See_Memory (ptr);
   Mode : DMA_Mode;
   Fd : POSIX_IO.File_Descriptor;
   Position     : Ada.Streams.Stream_Element_Offset;
   Ret : Int;
begin
   Put_Line ("TEST para sb16");
--    Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
--    Debug_Marte.Set_Break_Point_Here;
   ------------------------------------------------
   --  Read WAV file HEADER
   ------------------------------------------------
   Fd := POSIX_IO.Open ("/fat/test", POSIX_IO.Read_Only);
   POSIX_IO.Read (Fd, Buffer (1 .. 44), Position);
   ------------------------------------------------
   --  Inicializar la soundblaster
   ------------------------------------------------
   -- reset
   Outb_P (BASE + RESET, 1);
   delay_nanosleep (0.001);
   Outb_P (BASE + RESET, 0);
   -- dsp data available
   data_available;
   -- dsp ready?
   loop
      Byte := Inb_P (BASE + READ);
      exit when (Byte = 16#AA#);
   end loop;
   Put ("Sb16 found at "); Put (Integer (Base), 4, 16); New_Line;
   --  get version
   dspwait;
   Outb_P (BASE + WRITE, 16#E1#);
   data_available;
   Put ("Version: "); Put (Integer (Inb_P (BASE + READ)));
   Put ("."); Put (Integer (Inb_P (BASE + READ))); New_Line;
   --  IRQ
   if Mhi.Associate (5, Sb16_IRQ_Handler'Unrestricted_Access, System.Null_Address, 0) /= 0 then
      Put("Error: Could not associate to the IRQ"); New_Line;
   end if;
   if Mhi.Unlock (5) /= 0 then
      Put("Error: Could not unlock the IRQ"); New_Line;
   end if;

   loop
      ------------------------------------------------
      --  Read WAV
      ------------------------------------------------
      POSIX_IO.Read (Fd, Buffer, Position);
      exit when Position = (Buffer'First - 1);
      ------------------------------------------------
      --  DMA
      ------------------------------------------------
      Mode := Demand Or Addr_Increment Or Single_Cycle Or Memory_To_IO;
      DMA_Disable (DMAC1, Ch_1);
      DMA_Program (DMAC1, Ch_1, Mode, Ptr, Unsigned_16 (Position));
      DMA_Enable (DMAC1, Ch_1);
      ------------------------------------------------
      --  DSP
      ------------------------------------------------
      --  send frequency
      dspwait; Outb_P (BASE + WRITE, 16#41#);
      dspwait; Outb_P (BASE + WRITE, 16#2B#); -- MSB de 11025=2B11
      dspwait; Outb_P (BASE + WRITE, 16#11#); -- LSB
      --  8bit mono
      dspwait; Outb_P (BASE + WRITE, 16#C0#);  --  8bit
      dspwait; Outb_P (BASE + WRITE, 16#00#);  --  mono
      --  send length
      dspwait; Outb_P (BASE + WRITE, Unsigned_8 (Unsigned_16 (Position) And 16#FF#)); -- LSB (Lenght)
      dspwait; Outb_P (BASE + WRITE, Unsigned_8 (Shift_Right (Unsigned_16 (Position), 8))); -- MSB (Lenght)
      --  IRQ wait
      Ret := Mhi.Timedwait (0, null, Intr'Access, Handler'Access);
   end loop;
   POSIX_IO.Close (Fd);
   Put_Line ("END OF TEST");

end test_sb16_internals;