------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V1.51  Oct 2005
--
--                  'c m p s 0 3 _ f u n c t i o n s'
--
--                                    Spec
--
--
--  File 'cmps03_functions.ads'                             By Sangorrin
--
--
--  Import the functions of "cmps03_c"
--
--  ----------------------------------------------------------------------
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
--
------------------------------------------------------------------------------
with Drivers_Marte;
use Drivers_Marte;
with System, Ada.Unchecked_Conversion;

package Cmps03_Functions is

   --  Create
   function Create return Int;
   pragma Import (C, Create, "cmps03_c_create");
   function Address_To_Create_Ac is
   new Ada.Unchecked_Conversion (System.Address, Create_Function_Ac);
   Create_Ac : Create_Function_Ac := Address_To_Create_Ac (Create'Address);

   --  Remove
   function Remove return Int;
   pragma Import (C, Remove, "cmps03_c_remove");
   function Address_To_Remove_Ac is
   new Ada.Unchecked_Conversion (System.Address, Remove_Function_Ac);
   Remove_Ac : Remove_Function_Ac := Address_To_Remove_Ac (Remove'Address);

   --  Open
   function Open (
         Fd   : in     File_Descriptor;
         Mode : in     File_Access_Mode)
     return Int;
   pragma Import (C, Open, "cmps03_c_open");
   function Address_To_Open_Ac is
   new Ada.Unchecked_Conversion (System.Address, Open_Function_Ac);
   Open_Ac : Open_Function_Ac := Address_To_Open_Ac (Open'Address);

   --  Close
   function Close (
         Fd : in     File_Descriptor)
     return Int;
   pragma Import (C, Close, "cmps03_c_close");
   function Address_To_Close_Ac is
   new Ada.Unchecked_Conversion (System.Address, Close_Function_Ac);
   Close_Ac : Close_Function_Ac := Address_To_Close_Ac (Close'Address);

   --  Read
   function Read (
         Fd         : in     File_Descriptor;
         Buffer_Ptr : in     Buffer_Ac;
         Bytes      : in     Buffer_Length)
     return Ssize_T;
   pragma Import (C, Read, "cmps03_c_read");
   function Address_To_Read_Ac is
   new Ada.Unchecked_Conversion (System.Address, Read_Function_Ac);
   Read_Ac : Read_Function_Ac := Address_To_Read_Ac (Read'Address);

   --  Ioctl
   function Ioctl (
         Fd             : in     File_Descriptor;
         Request        : in     Ioctl_Option_Value;
         Ioctl_Data_Ptr : in     Buffer_Ac)
     return Int;
   pragma Import (C, Ioctl, "cmps03_c_ioctl");
   function Address_To_Ioctl_Ac is
   new Ada.Unchecked_Conversion (System.Address, Ioctl_Function_Ac);
   Ioctl_Ac : Ioctl_Function_Ac := Address_To_Ioctl_Ac (Ioctl'Address);

end Cmps03_Functions;

