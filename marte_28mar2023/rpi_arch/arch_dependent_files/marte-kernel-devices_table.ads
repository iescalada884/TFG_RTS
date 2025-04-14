------------------------------------------------------------------------------
-- --------------------        M a R T E   O S          ------------------- --
------------------------------------------------------------------------------
--                                                             V2.0 2019-05-24
--
--                ' K e r n e l . D e v i c e s _ T a b l e'
--
--                                    Spec
--
--
--  File 'k-devices_table.ads                                    By Fguerreira
--
--  This is the RPi architecture version of this package.
--
--  Table definition where all the drivers are loaded.
--
--  To add a new device to the system you should add a new entry in
--  the "Drivers table" and at least one new device file in the "Device
--  files table".
--
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

--with UART_Simple_Import;  --  standard output and error

--  MaRTE OS "withs" (Do not edit)
with MaRTE.Kernel.File_System_Data_Types;
use MaRTE.Kernel.File_System_Data_Types;

package MaRTE.Kernel.Devices_Table is

   package K renames MaRTE.Kernel;

   ---------------------------------------------------------------------------
   --  Drivers Table  --------------------------------------------------------
   ---------------------------------------------------------------------------
   --
   --  Device drivers included in the system. This table associates
   --  major numbers with device drivers. To include a driver just add
   --  the following lines to the table:
   --
   --  any major number available => (
   --  {your_driver}_create'access,
   --  {your_driver}_remove'access,
   --  {your_driver}_open'access,
   --  {your_driver}_close'access,
   --  {your_driver}_read'access,
   --  {your_driver}_write'access,
   --  {your_driver}_ioctl'access etc ...)
   --
   --  For your driver you can use any major number not in use, for
   --  example, if you are not using serial ports, major number 4 is
   --  free to be used in any other driver.
   --
   --  Major numbers are in range (1
   --  .. Configuration_Parameters.Devices_Files_Mx). In case you need
   --  a wider range, modify Devices_Files_Mx constant (file
   --  configuration_parameters.ads) and recompile MaRTE OS as
   --  explained in the user's guide (marte_ug.html).

   The_Driver_Table :  K.File_System_Data_Types.Driver_Table_Type :=
     (
      --  Standard Input
      --  DO NOT COMMENT OUT. If you don't want to use this device
      --  just set all entries to 'null'.
      --  If you modify this device, you should also change
      --  'Stdin_Direct_Read' in 'kernel_console.ads'.
--        1 => (Name   => "No stdin        ",
--              Create => null,
--              Remove => null,
--              Open   => null,
--              Close  => null,
--              Read   => null,
--              Write  => null,
--              Ioctl  => null,
--              Delete => null,
--              Lseek  => null),

      --  Standard Output
      --  DO NOT COMMENT OUT. If you don't want to use this device
      --  just set all entries to 'null'.
      --  If you modify this device, you should also change
      --  'Stdout_Basic_Init' and 'Stdout_Direct_Write' in
      --  'kernel_console.ads'.
--        2 => (Name   => "Serial          ",
--              Create => UART_Simple_Import.Create_Ac,
--              Remove => null,
--              Open   => null,
--              Close  => null,
--              Read   => null,
--              Write  => UART_Simple_Import.Write_Ac,
--              Ioctl  => null,
--              Delete => null,
--              Lseek  => null),

      --  Standard Error
      --  DO NOT COMMENT OUT. If you don't want to use this device
      --  just set all entries to 'null'.
      --  If you modify this device, you should also change
      --  'Stderr_Basic_Init' and 'Stderr_Direct_Write' in
      --  'kernel_console.ads'.
--        3 => (Name   => "Serial          ",
--              Create => UART_Simple_Import.Create_Ac,
--              Remove => null,
--              Open   => null,
--              Close  => null,
--              Read   => null,
--              Write  => UART_Simple_Import.Write_Ac,
--              Ioctl  => null,
--              Delete => null,
--              Lseek  => null),

      others =>
         ("                ",
         null, null, null, null, null, null, null, null, null));

   ---------------------------------------------------------------------------
   --  Device files table  ---------------------------------------------------
   ---------------------------------------------------------------------------
   --
   --  This table defines the files in the file-system. To create a new
   --  file, add a new entry to the table using the appropriate
   --  parameters:
   --
   --  The major number must be the one that points to the driver in
   --  The_Driver_Table rows
   --
   --  The minor number may be anyone, as it's used as a minor number
   --  to differentiate assignments of the same Major_Number
   --
   --  The Boolean indicates whether the assignment is in use or not

   The_Device_Files_Table :
     K.File_System_Data_Types.Device_Files_Table_Type :=
     (
      --        File path  Major  Minor   Used   Type   Del  Count
      --  Standard files. DO NOT COMMENT OUT
--          1  => ("/dev/stdin      ", 1, 0, True, Device, False, 0),
--          2  => ("/dev/stdout     ", 2, 0, True, Device, False, 0),
--          3  => ("/dev/stderr     ", 3, 0, True, Device, False, 0),
      others => ("                ", 9, 0, False, Device, False, 0)
      );

   --------------------------
   --  Initialize_Devices  --
   --------------------------
   procedure Initialize_Devices;

end MaRTE.Kernel.Devices_Table;
