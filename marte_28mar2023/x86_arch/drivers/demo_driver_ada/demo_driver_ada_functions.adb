------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--              'D e m o  _ D r i v e r _ A d a _ F u n c t i o n s'
--
--                                      Body
--
--
--  File 'demo_driver_ada_functions.adb'                     By Fguerreira and
--                                                              MAR.
--
--
--  A demostration Ada-driver. Just to show how drivers works in MaRTE
--  OS. You can test this driver with program
--  'uses_demo_driver_ada.adb' in the 'examples/drivers/' directory.
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
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;

with Drivers_MaRTE;       use Drivers_MaRTE;
with MaRTE.Direct_IO;    use MaRTE.Direct_IO;
with MaRTE.Integer_Types;

with Demo_Driver_Ada; --  Data types definition.

package body Demo_Driver_Ada_Functions is

   use type Int;

   ---------------------------
   --  Conversion to 'Data' --
   ---------------------------
   type Data_Ac is access all Demo_Driver_Ada.Data;
   function To_Data_Ac is new Ada.Unchecked_Conversion (Buffer_Ac, Data_Ac);

   ---------------------------------
   --  Conversion to 'Ioctl_Arg'  --
   ---------------------------------
   type Ioctl_Arg_Ac is access all Demo_Driver_Ada.Ioctl_Arg;
   function To_Ioctl_Arg_Ac is
      new Ada.Unchecked_Conversion (Buffer_Ac, Ioctl_Arg_Ac);

   -------------
   --  Buffer --
   -------------
   Demo_Buffer : Demo_Driver_Ada.Data;

   ------------
   -- Create --
   ------------
   function Create return Int is
   begin
      delay 1.0;
      return 0;
   end Create;

   ------------
   -- Remove --
   ------------
   function Remove return Int is
   begin
      return 0;
   end Remove;

   ----------
   -- Open --
   ----------
   function Open (Fd   : in File_Descriptor;
                  Mode : in File_Access_Mode) return Int is
      Mj : Major;
      Mn : Minor;
      Invalid_Fd : Boolean;
   begin
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put ("Demo Ada Driver: Opening device file " &
           File_Descriptor'Image (Fd) &
           "(Major:" & Major'Image (Mj) &
           " Minor:" & Minor'Image (Mn) &
           "). Mode:" & File_Access_Mode'Image (Mode));
      New_Line;
      return 0;
   end Open;

   -----------
   -- Close --
   -----------
   function Close (Fd : in File_Descriptor) return Int is
      Mj : Major;
      Mn : Minor;
      Invalid_Fd : Boolean;
   begin
      Get_Major_Number (Fd, Mj, Invalid_Fd);
      Get_Minor_Number (Fd, Mn, Invalid_Fd);
      Put ("Demo Ada Driver: Closing device file " &
           File_Descriptor'Image (Fd) &
           "(Major:" & Major'Image (Mj) &
           " Minor:" & Minor'Image (Mn) & ")");
      New_Line;
      return 0;
   end Close;

   ----------
   -- Read --
   ----------
   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Ssize_T is
      use type Buffer_Length, Demo_Driver_Ada.Data;
   begin
      if Bytes /= Demo_Driver_Ada.Data'Size/8 then
         return -1;
      end if;

      To_Data_Ac (Buffer_Ptr).all := Demo_Buffer;

      Put ("Demo Ada Diver: read '" & Demo_Buffer &
           "'(" & Buffer_Length'Image (Bytes) & " bytes)");
      New_Line;
      return Int (Bytes);
   end Read;

   -----------
   -- Write --
   -----------
   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Ssize_T is
      use type Buffer_Length, Demo_Driver_Ada.Data;
   begin
      if Bytes /= Demo_Driver_Ada.Data'Size/8 then
         return -1;
      end if;

      Demo_Buffer := To_Data_Ac (Buffer_Ptr).all;

      Put ("Demo Ada Diver: written '" & Demo_Buffer &
           "'(" & Buffer_Length'Image (Bytes) & " bytes)");
      New_Line;
      return Int (Bytes);
   end Write;

   -----------
   -- Ioctl --
   -----------
   function Ioctl (Fd             : in File_Descriptor;
                   Request        : in Ioctl_Option_Value;
                   Ioctl_Data_Ptr : in Buffer_Ac)  return Int is
      use type Demo_Driver_Ada.Ioctl_Arg;
   begin
      Put ("Demo Ada Diver: Ioctl. Request:" &
           Ioctl_Option_Value'Image (Request) &
           " Arg:'" & To_Ioctl_Arg_Ac (Ioctl_Data_Ptr).all & "'");
      New_Line;
      return 0;
   end Ioctl;

end Demo_Driver_Ada_Functions;
