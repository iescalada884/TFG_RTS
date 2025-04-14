------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--               'U s e s _ S e r i a l _ P o r t _ D r i v e r'
--
--                                   Body
--
--
--  File 'uses_serial_port_driver.adb'                           By Fguerreira
--                                                                  and MAR.
--
--
--  This program uses the serial port driver in 'drivers/serial_port/'
--  directory. In order to run the program you should install that
--  driver in the system. To do so, uncomment the appropriate lines in
--  'kernel/kernel-devices_table.ads'. After that you must recompile
--  the kernel (p.e. executing 'mkkernel -gnatn -gnatp -O3'), and then
--  compile this program using 'mgnatmake'.
--
--  For more information about installing drivers in MaRTE OS refer to
--  the "MaRTE OS User's Guide" (marte_ug.html).
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
------------------------------------------------------------------------------

with Serial_Port_Driver; use Serial_Port_Driver;

with POSIX_IO;  use POSIX_IO;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Text_IO;     use Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;

with MaRTE_OS;

procedure Uses_Serial_Port_Driver is

   procedure Put_SEA (SEA : in Ada.Streams.Stream_Element_Array);
   procedure String_To_SEA (Str : in String;
                            SEA : out Ada.Streams.Stream_Element_Array);
   procedure SEA_To_String (SEA : in Ada.Streams.Stream_Element_Array;
                            Str : out String);

   The_Fd       : File_Descriptor;
   Written_Str  : String := "0123456789";
   Key          : Character;
   Buffer_Write : Ada.Streams.Stream_Element_Array (1 .. 10);
   Buffer_Read  : Ada.Streams.Stream_Element_Array (1 .. 18);
   Position     : Ada.Streams.Stream_Element_Offset;


   --  *****************  --
   --  Ioctl (Data types) --
   --  *****************  --

   procedure Serial_Port_Ioctl is new
     POSIX_IO.Generic_Ioctl (Serial_Port_Driver.Ioctl_Options,
                             Serial_Port_Driver.Ioctl_Data);

   Ioctl_Data_Used : Serial_Port_Driver.Ioctl_Data;


   ---------------------------
   --  Internal procedures  --
   ---------------------------

   --  *************  --
   --  String_To_SEA  --
   --  *************  --

   procedure String_To_SEA (Str : in String;
                            SEA : out Ada.Streams.Stream_Element_Array) is

      type String_T    is new String (1 .. Str'Length);
      type SEA_T       is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (SEA'Length));
      type String_T_Ac is access all String_T;
      type SEA_T_Ac    is access all SEA_T;

      function String_T_Ac_To_SEA_T_Ac is new
        Ada.Unchecked_Conversion (String_T_Ac, SEA_T_Ac);

      Str_Tmp : aliased String_T := String_T (Str);
      SEA_Tmp_Ac : SEA_T_Ac;

   begin
      SEA_Tmp_Ac := String_T_Ac_To_SEA_T_Ac (Str_Tmp'Access);
      SEA := Ada.Streams.Stream_Element_Array (SEA_Tmp_Ac.all);
   end String_To_SEA;


   --  *************  --
   --  SEA_To_String  --
   --  *************  --

   procedure SEA_To_String (SEA : in Ada.Streams.Stream_Element_Array;
                            Str : out String) is

      type String_T    is new String (1 .. Str'Length);
      type SEA_T       is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (SEA'Length));
      type String_T_Ac is access all String_T;
      type SEA_T_Ac    is access all SEA_T;

      function SEA_T_Ac_To_String_T_Ac is new
        Ada.Unchecked_Conversion (SEA_T_Ac, String_T_Ac);

      SEA_Tmp : aliased SEA_T := SEA_T (SEA);
      Str_Tmp_Ac : String_T_Ac;

   begin
      Str_Tmp_Ac := SEA_T_Ac_To_String_T_Ac (SEA_Tmp'Access);
      Str := String (Str_Tmp_Ac.all);
   end SEA_To_String;


   --  *******  --
   --  Put_SEA  --
   --  *******  --

   procedure Put_SEA (SEA : in Ada.Streams.Stream_Element_Array) is
      Tmp_Str : String (1 .. Integer (SEA'Length));
   begin
      SEA_To_String (SEA, Tmp_Str);
      Put (Tmp_Str);
   end Put_SEA;



begin

   Put ("Opening serial port... ");
   The_Fd := Open ("/dev/ttyS0", READ_WRITE);
   Put_Line (" OK");

   -- Initial values

   New_Line;
   Serial_Port_Ioctl (The_Fd, Get_Speed, Ioctl_Data_Used);
   New_Line;
   Put ("INITIAL SPEED : ");
   Put (Integer (Ioctl_Data_Used.Output_Speed));
   Put (" bps"); New_Line;
   Put ("  Remote host speed must be set according to this value !!!");
   New_Line;


   -- Initial writing

   Put ("Press key to start sending info (WRITE function)....");
   Get_Immediate (Key);
   String_To_SEA (Written_Str, Buffer_Write);
   Write (The_Fd, Buffer_Write, Position);
   Put ("  Message '" & Written_Str & "' sent.");


   -- Initial reading

   New_Line;
   Put ("Waiting for remote info to be sent for READ.....");
   Read (The_Fd, Buffer_Read, Position);
   New_Line;
   Put ("INFO RECEIVED : ");
   Put_SEA (Buffer_Read);
   New_Line;
   Put ("  Press key to continue...");
   Get_Immediate (Key);

   -- NEW speed (19200 bauds)

   Ioctl_Data_Used.Output_Speed := Speed_Values (B19200);
   Serial_Port_Ioctl (The_Fd, Set_Speed, Ioctl_Data_Used);
   New_Line;
   Put ("CHANGED SPEED : ");
   Serial_Port_Ioctl (The_Fd, Get_Speed, Ioctl_Data_Used);
   Put (Integer (Ioctl_Data_Used.Output_Speed));
   Put (" bps"); New_Line;
   Put ("  Remote host speed must be set according to this value !!!");
   New_Line;

   --  Write
   Put ("Press key to start sending info (WRITE function)....");
   Get_Immediate (Key);
   Write (The_Fd, Buffer_Read, Position);
   Put ("  Message '"); Put_SEA (Buffer_Read); Put ("' sent.");

   --  Read
   New_Line;
   Put ("Waiting for remote info to be sent for READ.....");
   Read (The_Fd, Buffer_Write, Position);
   New_Line;
   Put ("INFO RECEIVED : ");
   Put_SEA (Buffer_Write);
   New_Line;


   Close (The_Fd);



exception
      when Excep_Event:others =>
         Put ("Exception:");
         Put (Ada.Exceptions.Exception_Name (Excep_Event));
         Put ("  " & Ada.Exceptions.Exception_Message (Excep_Event));

end Uses_Serial_Port_Driver;

