------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                             'membuffer_driver_import'
--
--                                    Spec
--
--
--  File 'membuffer_driver_import.ads'                             By Sangorrin
--
--
--  Import the functions of the "membuffer_driver_c" driver
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

with Drivers_MaRTE;  use Drivers_MaRTE;
with System, Ada.Unchecked_Conversion;

package Membuffer_Driver_Import is

   --  Create
   function Create return Int;
   pragma Import (C, Create, "membuffer_driver_create");
   function Address_To_Create_Ac is
      new Ada.Unchecked_Conversion (System.Address, Create_Function_Ac);
   Create_Ac : Create_Function_Ac := Address_To_Create_Ac (Create'Address);

   --  Remove
   function Remove return Int;
   pragma Import (C, Remove, "membuffer_driver_remove");
   function Address_To_Remove_Ac is
      new Ada.Unchecked_Conversion (System.Address, Remove_Function_Ac);
   Remove_Ac : Remove_Function_Ac := Address_To_Remove_Ac (Remove'Address);

   --  Read
   function Read (Fd         : in File_Descriptor;
                  Buffer_Ptr : in Buffer_Ac;
                  Bytes      : in Buffer_Length) return Int;
   pragma Import (C, Read, "membuffer_driver_read");
   function Address_To_Read_Ac is
      new Ada.Unchecked_Conversion (System.Address, Read_Function_Ac);
   Read_Ac : Read_Function_Ac := Address_To_Read_Ac (Read'Address);

   --  Write
   function Write (Fd         : in File_Descriptor;
                   Buffer_Ptr : in Buffer_Ac;
                   Bytes      : in Buffer_Length) return Int;
   pragma Import (C, Write, "membuffer_driver_write");
   function Address_To_Write_Ac is
      new Ada.Unchecked_Conversion (System.Address, Write_Function_Ac);
   Write_Ac : Write_Function_Ac := Address_To_Write_Ac (Write'Address);

end Membuffer_Driver_Import;
