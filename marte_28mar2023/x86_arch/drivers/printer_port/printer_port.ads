------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                         'P r i n t e r _ P o r t'
--
--                                   Spec
--
--
--  File 'printer_port.ads'                                           By MAR.
--
--
--  Driver for the PC printer port.
--
--  This file contains all the data types necessary for applications
--  in order to use the driver.
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
with MaRTE.Integer_Types;

package Printer_Port is

   -------------------
   --  Type 'Data'  --
   -------------------
   --
   --  Data to be read or write in driver.
   subtype Data is MaRTE.Integer_Types.Unsigned_8;

   ------------------------
   --  'Ioctl' Commands  --
   ------------------------
   --
   --  Command for the 'Ioctl' function.
   type Ioctl_Commads is
     (Enable_Interrupts,
      --  .

      Disable_interrupts,
      --  .
      Set_Strobe, Set_Auto_Feed, Set_Init_Printer, Set_Select_Input,

      Reset_Strobe, Reset_Auto_Feed, Reset_Init_Printer, Reset_Select_Input,

      Read_Busy, Read_Acknowledge, Read_Paper_End,
      Read_Device_Select, Read_Error
      );
   for Ioctl_Commads use (Enable_Interrupts     => 250,
                          Disable_interrupts    => 251,

                          Set_Strobe    => 252,
                          Set_Auto_Feed    => 253,
                          Set_Init_Printer    => 254,
                          Set_Select_Input    => 255,

                          Reset_Strobe    => 256,
                          Reset_Auto_Feed    => 257,
                          Reset_Init_Printer    => 258,
                          Reset_Select_Input    => 259,

                          Read_Busy    => 260,
                          Read_Acknowledge    => 261,
                          Read_Paper_End    => 262,
                          Read_Device_Select    => 263,
                          Read_Error    => 264);

   -----------------------
   --  Type 'Ioctl_Arg' --
   -----------------------
   --
   --  Argument for the 'Ioctl' function (not used).
   subtype Ioctl_Arg is Integer;

end Printer_Port;
