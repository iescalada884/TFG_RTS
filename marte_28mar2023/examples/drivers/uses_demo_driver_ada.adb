------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                 'U s e s _ D e m o  _ D r i v e r _ A d a'
--
--                                   Body
--
--
--  File 'uses_demo_driver_ada.adb'                          By Fguerreira and
--                                                              MAR.
--
--
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

with MaRTE_OS;
with Demo_Driver_Ada;
with POSIX_IO;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Uses_Demo_Driver_Ada is

   Fd : POSIX_IO.File_Descriptor;
   Data : Demo_Driver_Ada.Data;
   Ioctl_Arg : Demo_Driver_Ada.Ioctl_Arg;

   procedure Write is new POSIX_IO.Generic_Write (Demo_Driver_Ada.Data);
   procedure Read is new POSIX_IO.Generic_Read (Demo_Driver_Ada.Data);
   procedure Ioctl is new POSIX_IO.Generic_Ioctl (Demo_Driver_Ada.Ioctl_Cmd,
                                                  Demo_Driver_Ada.Ioctl_Arg);

begin
   --  Open device file and get file descriptor
   Fd := POSIX_IO.Open ("/dev/demo_ada", POSIX_IO.Read_Write);

   --  Write on device file
   Data := "Hello driver!!      ";
   Put_Line ("Application: writing '" & Data & "'(" &
             Integer'Image (Data'Size/8) & " bytes) in the device file");
   Write (Fd, Data);

   --  Read from device file
   Read (Fd, Data);
   Put_Line ("Application: read '" & Data & "'(" & Integer'Image (Data'Size/8)
             & " bytes) from the device file");

   --  Ioctl call on device file
   Ioctl_Arg := "12345";
   Ioctl (Fd, 8, Ioctl_Arg);

   --  Close device file
   POSIX_IO.Close (Fd);

exception
      when Excep_Event:others =>
         Put ("Exception:");
         Put (Ada.Exceptions.Exception_Name (Excep_Event));
         Put ("  " & Ada.Exceptions.Exception_Message (Excep_Event));
end Uses_Demo_Driver_Ada;
