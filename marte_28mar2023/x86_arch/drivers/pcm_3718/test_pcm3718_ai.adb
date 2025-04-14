------------------------------------------------------------------------------
--  ------------------         M a R T E   O S         -------------------  --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                              'test_pcm3718_ai'
--
--                                   Body
--
--
--  File 'test_pcm3718_ai.adb'                                   By Sangorrin
--
--  This program uses the Analog Input of the PCM-3718H Driver.
--  In order to run the program you should install that driver in the
--  system. For more information about installing drivers in MaRTE OS
--  refer to the "MaRTE OS User's Guide" (marte_ug.html).
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
-- POSIX IO --------------------------------------------------------------
with Posix_Io;
with Ada.Streams;
with Ada.Unchecked_Conversion;
-- TEXT IO ---------------------------------------------------------------
with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;
-- TYPES -----------------------------------------------------------------
with Pcm3718;
use Pcm3718;
with MaRTE.Integer_Types;
--------------------------------------------------------------------------
with Ada.Exceptions;
use Ada.Exceptions;
procedure Test_Pcm3718_Ai is

   package Bit renames MaRTE.Integer_Types;
   use type Bit.Unsigned_16;

   pragma Priority(4);

   -----------------------------------------------------------------------
   -- 1.- Useful Functions and Procedures
   -----------------------------------------------------------------------
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

   function Sample2volt (
         The_Sample : Sample_Type;
         Range_P    : Float;
         Bipolar    : Boolean)
     return Float is
      Range_N : Float := 0.0;
   begin
      if Bipolar then
         Range_N := Range_P;
      end if;
      return (Range_P-(-Range_N))*Float(The_Sample)/4096.0-Range_N;
   end Sample2volt;
   -----------------------------------------------------------------------
   -- 2.- GENERIC POSIX_IO INSTANCE
   -----------------------------------------------------------------------
   procedure Ai_Ioctl is
   new Posix_Io.Generic_Ioctl (Pcm3718.Ai_Ioctl_Cmd,Pcm3718.Ai_Ioctl_Arg);
   -----------------------------------------------------------------------
   -- 3.- UNCHECKED OPERATIONS FOR POSIX_IO ADA INTERFACE
   -----------------------------------------------------------------------
   type Sea_Ptr is access all Ada.Streams.Stream_Element_Array;
   type Data_Ptr is access all Analog_Data;
   function To_Data is
   new Ada.Unchecked_Conversion (Sea_Ptr,Data_Ptr);
   -----------------------------------------------------------------------
   -- 4.- VARIABLES
   -----------------------------------------------------------------------
   Fd         : Posix_Io.File_Descriptor;
   Buffer     : Sea_Ptr;
   Position   : Ada.Streams.Stream_Element_Offset;
   Ai_Command : Pcm3718.Ai_Ioctl_Cmd;
   Ai_Arg     : Pcm3718.Ai_Ioctl_Arg;
   Data       : Analog_Data_Type;
   Volt       : Float;
   Ranges     : array (Channel_Type) of Range_Type := (others => Unipolar_5);
   N,
   R          : Positive;
   Mode       : Positive range 1 .. 6;
   -----------------------------------------------------------------------
begin
   New_Line;
   Message("This is the Ada Test program for Analog Input of PCM-3718H.");
   Fd := Posix_Io.Open ("/dev/daq", Posix_Io.Read_Only);

   loop
      -----------------------------------------------------------------------
      Put("Enter a number of samples: ");
      Get(N);
      Skip_Line;
      Put("Enter Rate (us): ");
      Get(R);
      Skip_Line;
      Put("Mode soft(1) pacer+fixed(2) pacer+scan(3) external+fixed(4)"&
         "external+scan(5) exit(6): ");
      Get(Mode);
      Skip_Line;
      -- Create the buffer to hold the samples
      Buffer := new Ada.Streams.Stream_Element_Array
         (1 .. Ada.Streams.Stream_Element_Offset(N*Data'Size/8));
      -- Set the Range of each channel we want to use
      Ai_Command := Set_Range_Of_Channel;
      for I in Channel_Type range 0 .. 15 loop
         Ai_Arg.Start_Ch := I;
         Ai_Arg.Input_Range := Ranges(I);
         Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
      end loop;
      case Mode is
         ----------------------------------------------------------------------
         when 1 =>
            -- A) SOFTWARE TRIGGER
            Put_Line("SOFTWARE TRIGGER test");
            -- Set the parameters for software trigger
            Ai_Command := Set_Parameters;
            Ai_Arg.Start_Ch     := 0;
            Ai_Arg.Stop_Ch      := 15;
            Ai_Arg.Trigger      := Software;
            Ai_Arg.Count        := Num_Conv_Type(N);
            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);

            -- Read and display the samples
            Posix_Io.Read (Fd, Buffer.All, Position);

            for I in 1 .. Integer(Position)/(Data'Size/8) loop
               Data := To_Data(Buffer).all(I);
               Put(Integer'Image(I));
               Put(" Channel: "&Channel_Type'Image(Data.The_Channel));
               Volt := Sample2volt(Data.The_Sample, 5.0, False);
               Put("  Voltage: "&Float'Image(Volt));
               New_Line;
            end loop;
         ----------------------------------------------------------------------
         when 2 =>
            -- B) PACER TRIGGER Fixed MODE
            Message("PACER TRIGGER Fixed MODE test.");

            Ai_Command := Set_Parameters;
            Ai_Arg.Start_Ch     := 0;
            Ai_Arg.Stop_Ch      := 15;
            Ai_Arg.Trigger      := Pacer;
            Ai_Arg.Mode         := Fixed;
            Ai_Arg.Count        := Num_Conv_Type(N);
            Ai_Arg.Scan_Rate    := Scan_Rate_Type(R);

            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put("Aproximated Rate: "&Pcm3718.Scan_Rate_Type'Image(
                  Ai_Arg.Scan_Rate));
            New_Line;

            Posix_Io.Read (Fd,Buffer.All, Position);

            for I in 1 .. Integer(Position)/(Data'Size/8) loop
               Data := To_Data(Buffer).all(I);
               Put(Integer'Image(I));
               Put(" Channel: "&Channel_Type'Image(Data.The_Channel));
               Volt := Sample2volt(Data.The_Sample, 5.0, False);
               Put("  Voltage: "&Float'Image(Volt));
               New_Line;
            end loop;
         ----------------------------------------------------------------------
         when 3 =>
            -- C) PACER TRIGGER SCAN MODE
            Message("PACER TRIGGER SCAN MODE test.");

            Ai_Command := Flush;
            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);

            Ai_Command := Set_Parameters;
            Ai_Arg.Start_Ch     := 0;
            Ai_Arg.Stop_Ch      := 15;
            Ai_Arg.Trigger      := Pacer;
            Ai_Arg.Mode         := Scan;
            Ai_Arg.Scan_Rate    := Scan_Rate_Type(R);

            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put("Aproximated Rate:"&Pcm3718.Scan_Rate_Type'Image(
                  Ai_Arg.Scan_Rate));
            New_Line;

            delay 0.01;
            Posix_Io.Read (Fd,Buffer.All, Position);

            Ai_Command := Get_Status;
            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put_Line("Samples in Buffer: "&Num_Conv_Type'Image(
                  Ai_Arg.Count));

            for I in 1 .. Integer(Position)/(Data'Size/8) loop
               Data := To_Data(Buffer).all(I);
               Put(Integer'Image(I));
               Put(" Channel: "&Channel_Type'Image(Data.The_Channel));
               Volt := Sample2volt(Data.The_Sample, 5.0, False);
               Put("  Voltage: "&Float'Image(Volt));
               New_Line;
            end loop;

            delay 0.6;
            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put_Line("Samples in Buffer: "&Num_Conv_Type'Image(
                  Ai_Arg.Count));
         ----------------------------------------------------------------------
         when 4 =>
            -- D) EXTERNAL TRIGGER Fixed MODE
            Message("EXTERNAL TRIGGER Fixed MODE test.");

            Ai_Command := Set_Parameters;
            Ai_Arg.Start_Ch     := 0;
            Ai_Arg.Stop_Ch      := 15;
            Ai_Arg.Trigger      := External;
            Ai_Arg.Mode         := Fixed;
            Ai_Arg.Count        := Num_Conv_Type(N);
            Ai_Arg.Scan_Rate    := Scan_Rate_Type(R);

            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put("Aproximated Rate: "&Pcm3718.Scan_Rate_Type'Image(
                  Ai_Arg.Scan_Rate));
            New_Line;

            Posix_Io.Read (Fd,Buffer.All, Position);

            for I in 1 .. Integer(Position)/(Data'Size/8) loop
               Data := To_Data(Buffer).all(I);
               Put(Integer'Image(I));
               Put(" Channel: "&Channel_Type'Image(Data.The_Channel));
               Volt := Sample2volt(Data.The_Sample, 5.0, False);
               Put("  Voltage: "&Float'Image(Volt));
               New_Line;
            end loop;
         ----------------------------------------------------------------------
         when 5 =>
            -- E) EXTERNAL TRIGGER SCAN MODE
            Message("EXTERNAL TRIGGER SCAN MODE test.");

            Ai_Command := Flush;
            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);

            Ai_Command := Set_Parameters;
            Ai_Arg.Start_Ch     := 0;
            Ai_Arg.Stop_Ch      := 15;
            Ai_Arg.Trigger      := External;
            Ai_Arg.Mode         := Scan;
            Ai_Arg.Scan_Rate    := Scan_Rate_Type(R);

            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put("Aproximated Rate:"&Pcm3718.Scan_Rate_Type'Image(
                  Ai_Arg.Scan_Rate));
            New_Line;

            delay 0.01;
            Posix_Io.Read (Fd,Buffer.All, Position);

            Ai_Command := Get_Status;
            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put_Line("Samples in Buffer: "&Num_Conv_Type'Image(
                  Ai_Arg.Count));

            for I in 1 .. Integer(Position)/(Data'Size/8) loop
               Data := To_Data(Buffer).all(I);
               Put(Integer'Image(I));
               Put(" Channel: "&Channel_Type'Image(Data.The_Channel));
               Volt := Sample2volt(Data.The_Sample, 5.0, False);
               Put("  Voltage: "&Float'Image(Volt));
               New_Line;
            end loop;

            delay 0.6;
            Ai_Ioctl(Fd,Ai_Command,Ai_Arg);
            Put_Line("Samples in Buffer: "&Num_Conv_Type'Image(
                  Ai_Arg.Count));
            -------------------------------------------------------------------
            when 6 => exit;
      end case;
      Message("THE END");
   end loop;
   Posix_Io.Close (Fd);
exception
   when Except:others =>
      Ada.Text_Io.Put_Line ("Unexpected exception raised " &
         Ada.Exceptions.Exception_Name (Except));
      Ada.Text_Io.Put_Line(Exception_Information(Except));
      Ada.Text_Io.Put_Line(Exception_Message (Except));
end Test_Pcm3718_Ai;
