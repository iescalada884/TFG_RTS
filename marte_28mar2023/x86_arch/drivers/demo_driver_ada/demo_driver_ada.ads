------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                      'D e m o  _ D r i v e r _ A d a'
--
--                                   Spec
--
--
--  File 'demo_driver_ada.ads'                               By Fguerreira and
--                                                              MAR.
--
--
--  A demostration Ada-driver. Just to show how drivers works in MaRTE
--  OS. You can test this driver with program
--  'uses_demo_driver_ada.adb' in the 'examples/drivers/' directory.
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

package Demo_Driver_Ada is

   -------------------
   --  Type 'Data'  --
   -------------------
   --
   --  Data to be read or write in driver.
   subtype Data is String (1 .. 20);

   ----------------------
   -- Type 'Ioctl_Cmd' --
   ----------------------
   --
   --  Command for the 'Ioctl' function.
   subtype Ioctl_Cmd is Integer;

   -----------------------
   --  Type 'Ioctl_Arg' --
   -----------------------
   --
   --  Argument for the 'Ioctl' function.
   subtype Ioctl_Arg is  String (1 .. 5);

end Demo_Driver_Ada;
