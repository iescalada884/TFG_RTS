------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                           'test_pcm3718_buffer'
--
--                                   Body
--
--
--  File 'test_pcm3718_buffer.adb'                              By Sangorrin
--
--  This is an internal test for the driver's developer.
--
--  ----------------------------------------------------------------------
--   Copyright (C) 2000-2019, Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael Gonzalez Harbour      mgh@unican.es
--
--  MaRTE OS  is free software; you can  redistribute it and/or  modify it
--  under the terms of the GNU General Public License  as published by the
--  Free Software Foundation;  either  version 2, or (at  your option) any
--  later version.
--
--  MaRTE OS  is distributed  in the  hope  that  it will be   useful, but
--  WITHOUT  ANY  WARRANTY;     without  even the   implied   warranty  of
--  MERCHANTABILITY  or  FITNESS FOR A  PARTICULAR PURPOSE.    See the GNU
--  General Public License for more details.
--
--  You should have received  a  copy of  the  GNU General Public  License
--  distributed with MaRTE  OS;  see file COPYING.   If not,  write to the
--  Free Software  Foundation,  59 Temple Place  -  Suite 330,  Boston, MA
--  02111-1307, USA.
--
--  As a  special exception, if you  link this  unit  with other  files to
--  produce an   executable,   this unit  does  not  by  itself cause  the
--  resulting executable to be covered by the  GNU General Public License.
--  This exception does  not however invalidate  any other reasons why the
--  executable file might be covered by the GNU Public License.
--
-------------------------------------------------------------------------------
with Ada.Text_Io;
use Ada.Text_Io;

with MaRTE.Integer_Types;

with Pcm3718;
use Pcm3718;
with Pcm3718_Buffer;

procedure Test_Pcm3718_Buffer is

   package Bit renames MaRTE.Integer_Types;

   procedure Pause is
      H : Character;
   begin
      Ada.Text_Io.Put("Press..");
      Ada.Text_Io.Get_Immediate(H);
      Ada.Text_Io.New_Line;
   end Pause;

   Byte_Low  : Bit.Unsigned_8;
   Byte_High : Bit.Unsigned_8;
   Datos     : Analog_Data (1 .. 256);
   C         : Num_Conv_Type;

begin

   Pcm3718_Buffer.Flush;
   -- test one data
   Put_Line("This is the Pcm3718_Buffer test program.");
   Put_Line("1- Test one data.");
   Pause;
   Byte_Low  := 2#0000_0100#; -- channel 4
   Byte_High := 2#0000_0001#; -- sample 10000=16
   Pcm3718_Buffer.Write(Byte_Low,Byte_High);
   Pcm3718_Buffer.Read(Datos,C);
   Put_Line("Datos(1).The_Channel:"&Channel_Type'Image(Datos(1).
         The_Channel)&
      "Datos(1).The_Sample:"&Sample_Type'Image(Datos(1).The_Sample)&
      "Count: "&Num_Conv_Type'Image(C)&
      "status: "&Num_Conv_Type'Image(Pcm3718_Buffer.Get_Status));
   Pause;

   -- test full buffer
   Put_Line("2- Test full buffer.");
   Pause;
   for I in 0 .. 255 loop
      Pcm3718_Buffer.Write(Byte_Low,Byte_High);
   end loop;
   Pcm3718_Buffer.Read(Datos,C);
   Put_Line("Datos(1).The_Channel:"&Channel_Type'Image(Datos(1).
         The_Channel)&
      "Datos(1).The_Sample:"&Sample_Type'Image(Datos(1).The_Sample)&
      "Count: "&Num_Conv_Type'Image(C)&
      "status: "&Num_Conv_Type'Image(Pcm3718_Buffer.Get_Status));
   Put_Line(
      "Datos(256).The_Channel:"&Channel_Type'Image(Datos(256).The_Channel)&
      "Datos(256).The_Sample:"&Sample_Type'Image(Datos(256).The_Sample)&
      "Count: "&Num_Conv_Type'Image(C)&
      "status: "&Num_Conv_Type'Image(Pcm3718_Buffer.Get_Status));
   Pause;

   -- test overwrite of buffer
   Put_Line("3- Test overwritebuffer.");
   Pause;
   for I in 1 .. 258 loop
      Pcm3718_Buffer.Write(Byte_Low,Byte_High);
   end loop;
   Pcm3718_Buffer.Read(Datos,C);
   Put_Line("Datos(1).The_Channel:"&Channel_Type'Image(Datos(1).
         The_Channel)&
      "Datos(1).The_Sample:"&Sample_Type'Image(Datos(1).The_Sample)&
      "Count: "&Num_Conv_Type'Image(C)&
      "status: "&Num_Conv_Type'Image(Pcm3718_Buffer.Get_Status));
   Put_Line(
      "Datos(256).The_Channel:"&Channel_Type'Image(Datos(256).The_Channel)&
      "Datos(256).The_Sample:"&Sample_Type'Image(Datos(256).The_Sample)&
      "Count: "&Num_Conv_Type'Image(C)&
      "status: "&Num_Conv_Type'Image(Pcm3718_Buffer.Get_Status));
   Pause;

exception
   when others =>
      Put("exceptiooooooooooooooon");
end Test_Pcm3718_Buffer;
