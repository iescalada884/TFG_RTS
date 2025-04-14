--------------------------------------------------------------------------
-----------------------        M a R T E   O S          ------------------
--------------------------------------------------------------------------
--                                                         V1.51  Sep 2004
--
--                                'test_cmps03'
--
--                                   Body
--
--
--  File 'test_cmps03.adb'                             By Sangorrin
--
--  to compile: mgnatmake test_cmps03.adb
--
--------------------------------------------------------------------------
--   Copyright (C) 2004   Universidad de Cantabria, SPAIN
--
--   MaRTE OS web page: http://marte.unican.es
--   Contact Addresses: Mario Aldea Rivas          aldeam@unican.es
--                      Michael González Harbour      mgh@unican.es
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
--------------------------------------------------------------------------
with Marte_Os;
with Posix_Io;
with Ada.Streams;
with MaRTE.Integer_Types;
use MaRTE.Integer_Types;
with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;
with Ada.Exceptions;
use Ada.Exceptions;

with Cmps03;
use Cmps03;

with Ada.Unchecked_Conversion;

procedure Test_Cmps03 is

   pragma Priority(4);

   Fd : Posix_Io.File_Descriptor;

   type Sea_Ptr is access all Ada.Streams.Stream_Element_Array;
   type Data_Ptr is access all Unsigned_16;
   function To_Unsigned_16 is
   new Ada.Unchecked_Conversion (Sea_Ptr,Data_Ptr);

   Buffer : Sea_Ptr;
   Offset : Ada.Streams.Stream_Element_Offset;

   Cmd : Cmps03.Cmps03_Ioctl_Cmd;
   Arg : Cmps03.Cmps03_Ioctl_Arg;
   procedure Ai_Ioctl is
   new Posix_Io.Generic_Ioctl (Cmps03.Cmps03_Ioctl_Cmd,
      Cmps03.Cmps03_Ioctl_Arg);

   procedure Pause is
      H : Character;
   begin
      Put(" Press..");
      Get_Immediate(H);
      New_Line;
   end Pause;

   procedure Message (
         Str : String) is
   begin
      Put(Str);
      Pause;
   end Message;

begin
   New_Line;
   Message("This is the Ada Test program for CMPS03 Compass.");

   Buffer := new Ada.Streams.Stream_Element_Array(1 .. 2);

   Fd := Posix_Io.Open ("/dev/cmps03", Posix_Io.Read_Only);

   loop
      Message("Read Compass Bearing as a Byte (no blocking): ");
      -- 1- Start conversion
      Cmd := Start_Conversion;
      Arg.Mode := Bearing_Mode_Byte;
      Ai_Ioctl(Fd, Cmd, Arg);
      -- 2- Now check the Status of the conversion
      Cmd := Get_Status;
      loop
         Ai_Ioctl(Fd, Cmd, Arg);
         delay 1.0;
         case Arg.Status is
            when Conversion_In_Progress =>
               Put_Line("CONVERSION_IN_PROGRESS: Mr I2C-Daemon is busy");
            when No_Conversion_Started =>
               Put_Line("NOT_IN_USE: You better send a command");
               return;
            when Cmps03_Error =>
               Put_Line("CMPS03_ERROR: oooooops");
               return;
            when Conversion_Done =>
               Put_Line("CONVERSION_DONE: Good work Mr I2C-Daemon");
               exit;
         end case;
      end loop;
      -- 3- Finally read the data
      Posix_Io.Read (Fd, Buffer(1 .. 1), Offset);
      Put_Line("Compass Bearing as a Byte:"&
         Unsigned_8'Image(Unsigned_8(Buffer(1))));
      Put_Line("Which is: "&Integer'Image( Integer(Buffer(1))*360/255));

      --------------------------------------------------------------------

      Message("Read Compass Bearing as a Word (no blocking): ");
      -- 1- Start conversion
      Cmd := Start_Conversion;
      Arg.Mode := Bearing_Mode_Word;
      Ai_Ioctl(Fd, Cmd, Arg);
      -- 2- Now check the Status of the conversion
      Cmd := Get_Status;
      loop
         Ai_Ioctl(Fd, Cmd, Arg);
         delay 1.0;
         case Arg.Status is
            when Conversion_In_Progress =>
               Put_Line("CONVERSION_IN_PROGRESS: Mr I2C-Daemon is busy");
            when No_Conversion_Started =>
               Put_Line("NOT_IN_USE: You better send a command");
               return;
            when Cmps03_Error =>
               Put_Line("CMPS03_ERROR: oooooops");
               return;
            when Conversion_Done =>
               Put_Line("CONVERSION_DONE: Good work Mr I2C-Daemon");
               exit;
         end case;
      end loop;
      -- 3- Finally read the data
      Posix_Io.Read (Fd, Buffer(1 .. 2), Offset);
      Put_Line("Compass Bearing as a Word:"&
         Unsigned_16'Image(To_Unsigned_16(Buffer).all));
   end loop;

   Posix_Io.Close (Fd);

exception
   when Except:others =>
      Ada.Text_Io.Put_Line ("Unexpected exception raised " &
         Ada.Exceptions.Exception_Name (Except));
      Ada.Text_Io.Put_Line(Exception_Information(Except));
      Ada.Text_Io.Put_Line(Exception_Message (Except));
end Test_Cmps03;
