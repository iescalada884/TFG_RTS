--------------------------------------------------------------------------
-----------------------        M a R T E   O S          ------------------
--------------------------------------------------------------------------
--                                                        V1.51  Jul 2005
--
--                                'cmps03'
--
--                                   spec
--
--
--  File 'cmps03.ads'                                           By Sangorrin
--
--  This package contains data structures to be used by applications.
--  There are some important constants in cmps03.h
--
--  How to use it => see test_cmps03.adb
--  ----------------------------------------------------------------------
--   Copyright (C) 2005   Universidad de Cantabria, SPAIN
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
with MaRTE.Integer_Types; -- for Unsigned_8/16

package Cmps03 is

   package Bit renames MaRTE.Integer_Types;

   -----------------------------------------------------------------------
   -- 1.- DATA TYPES
   -----------------------------------------------------------------------
   -- You can use STREAM_ELEMENT_ARRAYs as usual or otherwise, instance the
   -- GENERIC POSIX READ function with the this types. You can read the compass
   -- value as a byte, i.e. 0-255 for a full circle (it may be easier for some
   -- applications than 0-360 which requires two bytes. For those who require
   -- better resolution 'bearing_word' is a 16 bit variable in the range 0-3599.
   -- This represents 0-359.9º.
   --    a) bearing_byte
   subtype Bearing_Byte is Bit.Unsigned_8;

   --    b) bearing_word
   subtype Bearing_Word is Bit.Unsigned_16;

   -----------------------------------------------------------------------
   -- 2.- IOCTL COMMANDS and ARGUMENTS
   -----------------------------------------------------------------------
   -- Control the device through a Generic IOCTL Posix_IO function.
   --    a) COMMANDS
   type Cmps03_Ioctl_Cmd is -- Cmps03 IOCTL Commands ( Arguments needed )
         (Start_Conversion, --     (Args: Mode)
          Get_Status);      --     (Args: Status)

   --    b) ARGUMENTS
   type Bearing_Mode is
         (Bearing_Mode_Byte,
          Bearing_Mode_Word);

   type Cmps03_Status is
         (No_Conversion_Started,
          Conversion_Done,
          Conversion_In_Progress,
          Cmps03_Error);

   type Cmps03_Ioctl_Arg is
      record
         Mode   : Bearing_Mode;
         Status : Cmps03_Status;
      end record;
   pragma Convention(C, Cmps03_Ioctl_Arg);

private

   for Bearing_Mode'Size use 8;
   for Bearing_Mode use (
      Bearing_Mode_Byte => 0,
      Bearing_Mode_Word => 1);

   for Cmps03_Status'Size use 8;
   for Cmps03_Status use (
      No_Conversion_Started  => 1,
      Conversion_Done        => 2,
      Conversion_In_Progress => 3,
      Cmps03_Error           => 4);

   for Cmps03_Ioctl_Cmd'Size use Integer'Size;
   for Cmps03_Ioctl_Cmd use (
      Start_Conversion => 0,
      Get_Status       => 1);


end Cmps03;
