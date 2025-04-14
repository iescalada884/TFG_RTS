--------------------------------------------------------------------------
-----------------------        M a R T E   O S          ------------------
--------------------------------------------------------------------------
--                                                         V1.51  Sep 2004
--
--                 		  'test_pcm3718_dio'
--
--                                   Body
--
--
--  File 'test_pcm3718_dio.adb'                             By Sangorrin
--
--  This program uses the demonstration Ada-driver in
--  'drivers/demo_driver_ada/' directory. In order to run the program
--  you should install that driver in the system. To do so, uncomment
--  the appropriate lines in 'kernel/k-devices_table.ads', and compile
--  this program running 'mgnatmake'.
--
--  For more information about installing drivers in MaRTE OS refer to
--  the "MaRTE OS User's Guide" (marte_ug.html).
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
with Posix_Io;
with Pcm3718;
use Pcm3718;

with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;

procedure Test_Pcm3718_Dio is

   Fd          : Posix_Io.File_Descriptor;
   Dio_Data    : Pcm3718.Digital_Data;
   Dio_Command : Pcm3718.Dio_Ioctl_Cmd;

   procedure Dio_Write is
   new Posix_Io.Generic_Write (Digital_Data);
   procedure Dio_Read is
   new Posix_Io.Generic_Read (Digital_Data);
   procedure Dio_Ioctl is
   new Posix_Io.Generic_Ioctl(Dio_Ioctl_Cmd,Integer);

   procedure Pause is
      H : Character;
   begin
      Put("Press..");
      Get_Immediate(H);
      New_Line;
   end Pause;

   Mode,
   Ioctl_Arg : Integer := 0;

begin

   Fd := Posix_Io.Open ("/dev/dio", Posix_Io.Read_Write);
   -------------------------------------------------------------------
   Put("Mode Byte_1(1) Byte_2(2) Word(3): ");
   Ada.Integer_Text_Io.Get(Mode);
   -------------------------------------------------------------------
   Dio_Command := Dio_Ioctl_Cmd'Val(Mode-1);
   Dio_Ioctl(Fd,Dio_Command,Ioctl_Arg);
   Pause;
   -- 1.a.- Digital Output test
   Dio_Data := 9580; -- 16#256C#
   Ada.Text_Io.Put_Line("Data: "&Pcm3718.Digital_Data'Image(Dio_Data));
   Ada.Text_Io.Put_Line("Check the measurement (16#256C#)");
   Dio_Write(Fd, Dio_Data);
   Pause;
   -- 1.b.- Digital Input test
   Ada.Text_Io.Put_Line("Put some data on the port (TTL 6mA max)");
   Pause;
   Dio_Read(Fd, Dio_Data);
   Ada.Text_Io.Put_Line("Data: "&Pcm3718.Digital_Data'Image(Dio_Data));
   -------------------------------------------------------------------
   Posix_Io.Close (Fd);

end Test_Pcm3718_Dio;
