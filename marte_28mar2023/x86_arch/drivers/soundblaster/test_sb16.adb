with MaRTE_OS;
--  Basic types
with MaRTE.Integer_Types; use MaRTE.Integer_Types;
with Interfaces.C;
--  Text input/output
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
--  for System.Address
with System;
with System.Storage_Elements;
with Ada.Unchecked_Conversion;
--  for Debugging
with Debug_Marte; use Debug_Marte;
--  for reading files
with POSIX_IO;  use POSIX_IO;
with Ada.Streams;
--  the target of this test
with Sb16; use Sb16;

procedure test_sb16 is

   use type Ada.Streams.Stream_Element_Offset;
   use type Interfaces.C.Int;

   --  WAV File
   Fd : POSIX_IO.File_Descriptor;
   Position : Ada.Streams.Stream_Element_Offset;
   --  SB Program values
   Rate : Unsigned_16; --  from 5000 to 44100 Hz
   Bits : Bits_Type;     --  8 or 16 bits
   Tracks : Tracks_Type;   --  1 or 2 (mono, stereo)
   Length : Unsigned_16; -- number of samples in the buffer
   --  Buffer for the samples
   Ptr : System.Address :=
      System.Storage_Elements.To_Address (16#10000#); -- < 16MBytes
   Buffer : Ada.Streams.Stream_Element_Array (1 .. 50000);
   for Buffer'Address use Ptr;
begin
   Put_Line ("*********************************************************");
   Put_Line ("********* TEST FOR THE SOUNDBLASTER SB16 MODULE *********");
   Put_Line ("*********************************************************");
   New_Line;
   --  Debug_Marte.Init_Serial_Communication_With_Gdb (Serial_Port_1);
   --  Debug_Marte.Set_Break_Point_Here;
   ------------------------------------------------
   --  Read WAV file HEADER
   ------------------------------------------------
   Fd := POSIX_IO.Open ("/fat/test", POSIX_IO.Read_Only);
   POSIX_IO.Read (Fd, Buffer (1 .. 44), Position);
   --  FALTA!!! Decodificar el header (Rate ...)
   ------------------------------------------------
   --  Probe for a soundblaster card
   ------------------------------------------------
   if Sb16_Probe = 0 then
      Put_Line ("DEBUG: Some Sb16 cards were found");
   else
      Put_Line ("ERROR: No Sb16 cards were found");
   end if;
   ------------------------------------------------
   --  Init the soundblaster card
   ------------------------------------------------
   if Sb16_Init = 0 then
      Put_Line ("DEBUG: Sb16 correctly initialized");
   else
      Put_Line ("ERROR: Could not initialize Sb16");
   end if;
   ------------------------------------------------
   --  Play the music
   ------------------------------------------------
   loop
      --  Read a chunk from the WAV File
      POSIX_IO.Read (Fd, Buffer, Position);
      exit when Position = (Buffer'First - 1);
      --  Set Values for programming the DSP
      Rate := 11025; --  from 5000 to 44100 Hz
      Bits := Sb16.Bits8;     --  8 or 16 bits
      Tracks := Sb16.Mono;   --  1 or 2 (mono, stereo)
      Length := Unsigned_16 (Position); -- number of samples in the buffer
      --  Program the DSP
      if Sb16_Program (Rate, Bits, Tracks, Ptr, Length) = 0 then
         Put_Line ("DEBUG: Sb16 correctly programmed");
      else
         Put_Line ("ERROR: Could not program the Sb16 Card");
      end if;
      --  IRQ wait
      if Sb16_Wait_for_IRQ = 0 then
         Put_Line ("DEBUG: IRQ correctly acknowledge");
      else
         Put_Line ("ERROR: waiting for the IRQ");
      end if;
   end loop;
   POSIX_IO.Close (Fd);
   New_Line;
   Put_Line ("*********************************************************");
   Put_Line ("*********** END OF THE SOUNDBLASTER SB16 TEST ***********");
   Put_Line ("*********************************************************");
end test_sb16;